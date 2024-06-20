using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading;
using System.Threading.Tasks;

namespace IxMilia.Lisp
{
    // https://gist.github.com/chaitanyagupta/9324402
    public delegate Task LispSpecialOperatorDelegate(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken); // args unevaluated, no result
    public delegate Task<LispObject> LispMacroDelegate(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken); // args unevaluated, result evaluated
    public delegate Task<LispObject> LispFunctionDelegate(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken); // args evaluated, result unevaluated

    public class LispHost
    {
        private const string BootstrapFileName = "bootstrap.lisp";
        private const string InitFileName = "init.lisp";
        private const string PackageString = "*PACKAGE*";
        private const string TerminalIOString = "*TERMINAL-IO*";

        private LispObject _eofMarker = new LispResolvedSymbol("(EOF-PACKAGE)", "(EOF)", isPublic: true);
        public readonly LispRootStackFrame RootFrame;
        private LispPackage _currentPackage;

        public LispHostConfiguration Configuration { get; }

        public LispObject T => RootFrame.T;
        public LispObject Nil => RootFrame.Nil;
        public LispTextStream TerminalIO => RootFrame.TerminalIO;

        public LispPackage CurrentPackage
        {
            get => _currentPackage;
            set
            {
                var packageToSet = RootFrame.GetPackage(value.Name);
                if (!ReferenceEquals(packageToSet, value))
                {
                    throw new ArgumentException("The package is not part of the host");
                }

                _currentPackage = packageToSet;
                SetValue($"{LispRootStackFrame.CommonLispPackageName}:{PackageString}", packageToSet);
            }
        }

        private LispHost(LispHostConfiguration configuration)
        {
            Configuration = configuration ?? throw new ArgumentNullException(nameof(configuration));
            RootFrame = new LispRootStackFrame(
                input: configuration.Input,
                output: configuration.Output,
                getUntrackedValue: configuration.GetUntrackedValue,
                trySetUntrackedValue: configuration.TrySetUntrackedValue);
            var commonLispPackage = RootFrame.GetPackage(LispRootStackFrame.CommonLispPackageName);
            CurrentPackage = commonLispPackage;
            AddContextObject(new LispSpecialOperatorsContext());
            AddContextObject(new LispDefaultContext());
        }

        public static async Task<LispHost> CreateAsync(LispHostConfiguration configuration = default, CancellationToken cancellationToken = default)
        {
            var host = new LispHost(configuration ?? new LispHostConfiguration());
            host.SetValue("*AN-OBJECT-CANNOT-START-WITH-RIGHT-PAREN-ERROR*", new LispString("An object cannot start with #\\)")); // required in bootstrap script
            host.SetValue("*ILLEGAL-END-OF-DOTTED-LIST-ERROR*", new LispString("Illegal end of dotted list")); // required in bootstrap script
            host.SetValue("*UNMATCHED-LEFT-PAREN-ERROR*", new LispString("Unmatched #\\(")); // required in bootstrap script
            host.SetValue("*END-OF-STRING*", new LispString("Unexpected end of string")); // required in init before strings can be read
            await host.ApplyBootstrapScriptAsync(cancellationToken);
            if (host.Configuration.UseInitScript)
            {
                host.SetReaderFunction(LispReaderType.NoReaderMacros);
                await host.ApplyInitScriptAsync(cancellationToken);
            }

            host.SetReaderFunction(host.Configuration.ReaderType);
            return host;
        }

        internal void SetReaderFunction(LispReaderType readerType)
        {
            var readFunctionName = readerType switch
            {
                LispReaderType.Compiled => "READ-COMPILED",
                LispReaderType.Interpreted => "READ-INTERPRETED",
                LispReaderType.NoReaderMacros => "READ-COMPILED-NO-MACROS",
                _ => throw new NotSupportedException($"Unknown reader type {readerType}"),
            };
            var read = RootFrame.GetValue<LispFunction>(new LispResolvedSymbol(LispRootStackFrame.CommonLispPackageName, readFunctionName, true));
            var readSymbol = new LispResolvedSymbol(LispRootStackFrame.CommonLispPackageName, "READ", true);
            read.NameSymbol = readSymbol; // some hackery to make the function name be "READ" instead of "READ-COMPILED" or "READ-INTERPRETED"
            RootFrame.SetValue(readSymbol, read);
        }

        public LispPackage AddPackage(string packageName, IEnumerable<LispPackage> inheritedPackages = null) => RootFrame.AddPackage(packageName, inheritedPackages);

        public void AddSpecialOperator(LispSymbol symbol, string documentation, string signature, LispSpecialOperatorDelegate del)
        {
            var resolvedSymbol = symbol.Resolve(CurrentPackage);
            var op = new LispSpecialOperator(resolvedSymbol, documentation, signature, del);
            SetValue(resolvedSymbol.Value, op, createPackage: true);
        }

        public void AddMacro(string name, LispMacroDelegate del) => AddMacro(LispSymbol.CreateFromString(name), del);

        public void AddMacro(LispSymbol symbol, LispMacroDelegate del) => AddMacro(symbol, null, null, del);

        public void AddMacro(LispSymbol symbol, string documentation, string signature, LispMacroDelegate del)
        {
            var resolvedSymbol = symbol.Resolve(CurrentPackage);
            var macro = new LispNativeMacro(resolvedSymbol, documentation, signature, del);
            SetValue(resolvedSymbol.Value, macro, createPackage: true);
        }

        public void AddFunction(string name, LispFunctionDelegate del) => AddFunction(LispSymbol.CreateFromString(name), del);

        public void AddFunction(LispSymbol symbol, LispFunctionDelegate del) => AddFunction(symbol, null, null, del);

        public void AddFunction(LispSymbol symbol, string documentation, string signature, LispFunctionDelegate del)
        {
            var resolvedSymbol = symbol.Resolve(CurrentPackage);
            var function = new LispNativeFunction(resolvedSymbol, documentation, signature, del);
            SetValue(resolvedSymbol.Value, function, createPackage: true);
        }

        public void AddContextObject(object context)
        {
            if (context == null)
            {
                throw new ArgumentNullException(nameof(context));
            }

            // bind public methods with the appropriate attribute and shape
            foreach (var methodInfo in context.GetType().GetTypeInfo().DeclaredMethods)
            {
                var parameterInfo = methodInfo.GetParameters();
                if (parameterInfo.Length == 4 &&
                    parameterInfo[0].ParameterType == typeof(LispHost) &&
                    parameterInfo[1].ParameterType == typeof(LispExecutionState) &&
                    parameterInfo[2].ParameterType == typeof(LispObject[]) &&
                    parameterInfo[3].ParameterType == typeof(CancellationToken))
                {
                    // native special operators (unevaluated arguments, no result)
                    var operators = methodInfo.GetCustomAttributes<LispSpecialOperatorAttribute>(inherit: true).ToList();
                    if (operators.Any())
                    {
                        if (methodInfo.ReturnType == typeof(Task))
                        {
                            var del = (LispSpecialOperatorDelegate)methodInfo.CreateDelegate(typeof(LispSpecialOperatorDelegate), context);
                            foreach (var op in operators)
                            {
                                var symbol = LispSymbol.CreateFromString(op.Name);
                                AddSpecialOperator(symbol, op.Documentation, op.Signature ?? "...", del);
                            }
                        }
                        else
                        {
                            throw new InvalidOperationException($"Native special operator expected to have return type of 'void' but found '{methodInfo.ReturnType.Name}'.");
                        }
                    }

                    // native macros (unevaluated arguments, evaluated result)
                    var macros = methodInfo.GetCustomAttributes<LispMacroAttribute>(inherit: true).ToList();
                    if (macros.Any())
                    {
                        if (methodInfo.ReturnType == typeof(Task<LispObject>))
                        {
                            var del = (LispMacroDelegate)methodInfo.CreateDelegate(typeof(LispMacroDelegate), context);
                            foreach (var macro in macros)
                            {
                                var symbol = LispSymbol.CreateFromString(macro.Name);
                                AddMacro(symbol, macro.Documentation, macro.Signature ?? "...", del);
                            }
                        }
                        else
                        {
                            throw new InvalidOperationException($"Native macro expected to have return type of '{nameof(LispObject)}' but found '{methodInfo.ReturnType.Name}'.");
                        }
                    }

                    // native functions (evaluated arguments, unevaluated result)
                    var functions = methodInfo.GetCustomAttributes<LispFunctionAttribute>(inherit: true).ToList();
                    if (functions.Any())
                    {
                        if (methodInfo.ReturnType == typeof(Task<LispObject>))
                        {
                            var del = (LispFunctionDelegate)methodInfo.CreateDelegate(typeof(LispFunctionDelegate), context);
                            foreach (var function in functions)
                            {
                                var symbol = LispSymbol.CreateFromString(function.Name);
                                AddFunction(symbol, function.Documentation, function.Signature ?? "...", del);
                            }
                        }
                        else
                        {
                            throw new InvalidOperationException($"Native function expected to have return type of '{nameof(LispObject)}' but found '{methodInfo.ReturnType.Name}'.");
                        }
                    }
                }
            }
        }

        private static async Task<string> GetEmbeddedScriptContentsAsync(string fileName)
        {
            var type = typeof(LispHost);
            var lastDotIndex = type.FullName.LastIndexOf('.');
            var namespacePrefix = type.FullName.Substring(0, lastDotIndex);
            var assembly = type.GetTypeInfo().Assembly;
            using (var initStream = assembly.GetManifestResourceStream($"{namespacePrefix}.{fileName}"))
            using (var reader = new StreamReader(initStream))
            {
                var content = await reader.ReadToEndAsync();
                return content;
            }
        }

        public static Task<string> GetBootstrapScriptContents() => GetEmbeddedScriptContentsAsync(BootstrapFileName);

        public static Task<string> GetInitScriptContents() => GetEmbeddedScriptContentsAsync(InitFileName);

        private async Task ApplyBootstrapScriptAsync(CancellationToken cancellationToken)
        {
            var content = await GetBootstrapScriptContents();
            var reader = new StringReader(content);
            var stream = new LispTextStream(BootstrapFileName, reader, TextWriter.Null);
            var eofValue = LispDefaultContext.GenSym();
            var compiledParser = new LispCompiledParser(stream);
            LispObject next;
            LispObject lastResult = null;
            LispEvaluationState lastState = default;
            var executionState = CreateExecutionState();
            while ((next = await compiledParser.ParseItem(this, RootFrame, eofValue, cancellationToken: cancellationToken)) is not null)
            {
                if (ReferenceEquals(next, eofValue) ||
                    next.IsEof())
                {
                    break;
                }

                executionState.InsertOperation(new LispEvaluatorObjectExpression(next));
                lastState = await LispEvaluator.EvaluateAsync(this, executionState, cancellationToken);
                executionState.TryPopArgument(out lastResult);
            }

            var result = new LispEvalResult(lastState, lastResult);
            ValidateScriptExecution(result);
        }

        private async Task ApplyInitScriptAsync(CancellationToken cancellationToken)
        {
            var content = await GetInitScriptContents();
            await EvalInternalScript(InitFileName, content, cancellationToken);
        }

        private async Task EvalInternalScript(string inputName, string content, CancellationToken cancellationToken)
        {
            var executionState = CreateExecutionState(allowHalting: false);
            var evalResult = await EvalAsync(inputName, content, executionState, cancellationToken);
            ValidateScriptExecution(evalResult);
        }

        private void ValidateScriptExecution(LispEvalResult result)
        {
            if (result.State == LispEvaluationState.FatalHalt)
            {
                throw new Exception("Expected evaluation of script to be complete");
            }

            if (!ReferenceEquals(T, result.Value))
            {
                throw new Exception($"Expected evaluation of script to be `t` but found: {result.Value}");
            }
        }

        public LispExecutionState CreateExecutionState(bool allowHalting = true) => LispExecutionState.CreateExecutionState(RootFrame, allowHalting: allowHalting);

        public async Task<LispEvalResult> EvalAsync(string inputName, string code, LispExecutionState executionState, CancellationToken cancellationToken = default)
        {
            executionState.InsertCodeOperations(inputName, code);
            return await EvalContinueAsync(executionState, cancellationToken);
        }

        internal async Task<LispEvalResult> EvalAsync(LispObject obj, LispExecutionState executionState, bool createDribbleInstructions = false, CancellationToken cancellationToken = default)
        {
            executionState.InsertObjectOperations(obj, createDribbleInstructions);
            return await EvalContinueAsync(executionState, cancellationToken);
        }

        private void ThrowIfError(LispEvalResult evalResult)
        {
            if (evalResult.Value != T)
            {
                var message = $"Expected 'T' but found '{evalResult.Value}' at ({evalResult.Value?.SourceLocation?.Start.Line}, {evalResult.Value?.SourceLocation?.Start.Column}).";
                throw new Exception(message);
            }
        }

        public async Task<LispParseResult> ParseUntilSourceLocationAsync(string inputName, string code, LispSourcePosition position, CancellationToken cancellationToken = default)
        {
            var allowIncompleteSymbol = new LispUnresolvedSymbol("*ALLOW-INCOMPLETE-OBJECTS*").Resolve(CurrentPackage);
            var allowUnresolvedSymbol = new LispUnresolvedSymbol("*ALLOW-UNRESOLVED-SYMBOLS*").Resolve(CurrentPackage);

            using (new LimitedVariableScope(allowIncompleteSymbol, RootFrame, T))
            using (new LimitedVariableScope(allowUnresolvedSymbol, RootFrame, T))
            {
                var boundValues = RootFrame.GetBoundValues();
                var textReader = new StringReader(code);
                var textStream = new LispTextStream(inputName, textReader, TextWriter.Null);
                var readOperation = LispList.FromItems(
                    new LispResolvedSymbol("COMMON-LISP", "READ", true),
                    textStream,
                    T,
                    Nil,
                    T
                );

                LispObject containingObject = null;
                var executionState = CreateExecutionState();
                while (true)
                {
                    var evalResult = await EvalAsync(readOperation, executionState, cancellationToken: cancellationToken);
                    if (evalResult.State == LispEvaluationState.FatalHalt ||
                        (evalResult.Value is LispError error && error.Message == "EOF"))
                    {
                        break;
                    }

                    boundValues.TryAddSourceBinding(CurrentPackage, evalResult.Value);

                    if (evalResult.Value.SourceLocation?.ContainsPosition(position) == true)
                    {
                        containingObject = evalResult.Value;
                        break;
                    }
                }

                var narrowestChild = containingObject?.GetNarrowestChild(position);
                if (narrowestChild != null && !ReferenceEquals(narrowestChild, containingObject))
                {
                    boundValues.TryAddSourceBinding(CurrentPackage, narrowestChild);
                }

                var parseResult = new LispParseResult(this, narrowestChild, boundValues);
                return parseResult;
            }
        }

        public void SetValue(string name, LispObject value) => SetValue(name, value, createPackage: false);

        private void SetValue(string name, LispObject value, bool createPackage)
        {
            var symbol = LispSymbol.CreateFromString(name).Resolve(CurrentPackage);
            RootFrame.SetValue(symbol, value, createPackage);
        }

        public LispObject GetValue(string name)
        {
            var symbol = LispSymbol.CreateFromString(name).Resolve(CurrentPackage);
            return RootFrame.GetValue(symbol);
        }

        public TObject GetValue<TObject>(string name) where TObject: LispObject
        {
            var result = GetValue(name);
            return (TObject)result;
        }

        public async Task<IEnumerable<LispObject>> ParseAllAsync(string code, CancellationToken cancellationToken = default)
        {
            var allowIncompleteSymbol = new LispUnresolvedSymbol("*ALLOW-INCOMPLETE-OBJECTS*").Resolve(CurrentPackage);
            var allowUnresolvedSymbol = new LispUnresolvedSymbol("*ALLOW-UNRESOLVED-SYMBOLS*").Resolve(CurrentPackage);

            using (new LimitedVariableScope(allowIncompleteSymbol, RootFrame, T))
            using (new LimitedVariableScope(allowUnresolvedSymbol, RootFrame, T))
            {
                var textReader = new StringReader(code);
                var textStream = new LispTextStream("TODO", textReader, TextWriter.Null);
                var readOperation = LispList.FromItems(
                    new LispResolvedSymbol("COMMON-LISP", "READ", true),
                    textStream);
                var result = new List<LispObject>();
                while (true)
                {
                    var executionState = CreateExecutionState(allowHalting: false);
                    var evalResult = await EvalAsync(readOperation, executionState, cancellationToken: cancellationToken);
                    var lastResult = evalResult.Value;
                    if (lastResult is null)
                    {
                        break;
                    }

                    if (lastResult is LispError error &&
                        error.Message == "EOF")
                    {
                        break;
                    }

                    result.Add(lastResult);
                }

                return result;
            }
        }

        public async Task<LispEvalResult> EvalContinueAsync(LispExecutionState executionState, CancellationToken cancellationToken = default)
        {
            var state = await LispEvaluator.EvaluateAsync(this, executionState, cancellationToken);
            var evalResult = new LispEvalResult(state, executionState);
            return evalResult;
        }

        public async Task<LispObject> EvalAtStackFrameAsync(LispStackFrame frame, LispObject obj, CancellationToken cancellationToken = default)
        {
            var executionState = LispExecutionState.CreateExecutionState(frame, allowHalting: false);
            executionState.InsertExpressionOperation(obj);
            var evalResult = await EvalContinueAsync(executionState, cancellationToken);
            return evalResult.Value;
        }

        public async Task<LispEvalResult> StepOverAsync(LispExecutionState executionState, CancellationToken cancellationToken = default)
        {
            var currentExpression = executionState.PeekCurrentExpression();
            if (currentExpression?.Parent is LispList parentList &&
                ReferenceEquals(parentList.LastItem(), currentExpression))
            {
                // really a step-out
                var result = await StepOutAsync(executionState, cancellationToken);
                return result;
            }

            // ref-count function exits...
            var functionEnteredCounter = 0;
            var exitedViaFunctionRefCount = false;
            var functionEnterEventHandler = new EventHandler<LispFunctionEnteredEventArgs>((s, e) =>
            {
                functionEnteredCounter++;
            });
            var functionExitedEventHandler = new EventHandler<LispFunctionReturnedEventArgs>((s, e) =>
            {
                if (functionEnteredCounter == 0)
                {
                    e.HaltExecution = true;
                    exitedViaFunctionRefCount = true;
                }
                else
                {
                    functionEnteredCounter--;
                }
            });

            // ... or the next expression (i.e., not current) that has the same parent
            var expressionHalter = new EventHandler<LispEvaluatingExpressionEventArgs>((s, e) =>
            {
                // run until the next (i.e., not current) expression with the same parent
                if (!ReferenceEquals(currentExpression, e.Expression) &&
                    ReferenceEquals(currentExpression?.Parent, e.Expression?.Parent) &&
                    ShouldStopAtLocation(e.Expression?.SourceLocation))
                {
                    e.HaltExecution = true;
                }
            });

            RootFrame.FunctionEntered += functionEnterEventHandler;
            RootFrame.FunctionReturned += functionExitedEventHandler;
            RootFrame.EvaluatingExpression += expressionHalter;
            var evalResult = await EvalContinueAsync(executionState, cancellationToken);
            RootFrame.EvaluatingExpression -= expressionHalter;
            RootFrame.FunctionReturned -= functionExitedEventHandler;
            RootFrame.FunctionEntered -= functionEnterEventHandler;

            if (exitedViaFunctionRefCount)
            {
                // if we were at the end of a function and actually stepped out, halt on the next expression
                evalResult = await HaltOnNextExpressionAsync(executionState, 0, cancellationToken);
            }

            return evalResult;
        }

        public async Task<LispEvalResult> StepInAsync(LispExecutionState executionState, CancellationToken cancellationToken = default)
        {
            // halting without skipping will halt where we're already halted
            var evalResult = await HaltOnNextExpressionAsync(executionState, 1, cancellationToken);
            return evalResult;
        }

        public async Task<LispEvalResult> StepOutAsync(LispExecutionState executionState, CancellationToken cancellationToken = default)
        {
            var initialDepth = executionState.StackFrame.Depth;

            // ref-count function exits...
            var functionEnteredCounter = 0;
            var functionEnterHandler = new EventHandler<LispFunctionEnteredEventArgs>((s, e) =>
            {
                functionEnteredCounter++;
            });
            var functionExitHalter = new EventHandler<LispFunctionReturnedEventArgs>((s, e) =>
            {
                if (functionEnteredCounter == 0)
                {
                    e.HaltExecution = true;
                }
                else
                {
                    functionEnteredCounter--;
                }
            });
            RootFrame.FunctionEntered += functionEnterHandler;
            RootFrame.FunctionReturned += functionExitHalter;
            await EvalContinueAsync(executionState, cancellationToken);
            RootFrame.FunctionReturned -= functionExitHalter;
            RootFrame.FunctionEntered -= functionEnterHandler;

            // ...then halt on the very next expression
            var evalResult = await HaltOnNextExpressionAsync(executionState, 0, cancellationToken);
            return evalResult;
        }

        private bool IsUserCode(LispSourceLocation? location)
        {
            var filePath = location?.FilePath;
            switch (filePath)
            {
                case "bootstrap.lisp":
                case "init.lisp":
                    // embedded script that ships with this library; not user code
                    return false;
                default:
                    return true;
            }
        }

        private bool ShouldStopAtLocation(LispSourceLocation? location)
        {
            if (Configuration.UseJustMyCode)
            {
                // only halt if we're in user code
                return IsUserCode(location);
            }

            // always halt
            return true;
        }

        private async Task<LispEvalResult> HaltOnNextExpressionAsync(LispExecutionState executionState, int skipExpressionCount, CancellationToken cancellationToken)
        {
            // allows various pop and exit operations to be processed
            var skippedExpressions = 0;
            var nextExpressionHalter = new EventHandler<LispEvaluatingExpressionEventArgs>((s, e) =>
            {
                if (ShouldStopAtLocation(e.Expression.SourceLocation))
                {
                    // if a candidate for stopping, count expressions
                    if (skippedExpressions >= skipExpressionCount)
                    {
                        e.HaltExecution = true;
                    }
                    else
                    {
                        skippedExpressions++;
                    }
                }
            });
            RootFrame.EvaluatingExpression += nextExpressionHalter;
            var evalResult = await EvalContinueAsync(executionState, cancellationToken);
            RootFrame.EvaluatingExpression -= nextExpressionHalter;
            return evalResult;
        }
    }
}
