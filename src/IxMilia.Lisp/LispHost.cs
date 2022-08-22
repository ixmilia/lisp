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
        private const string PackageString = "*PACKAGE*";
        private const string TerminalIOString = "*TERMINAL-IO*";

        public string InitialFilePath { get; private set; }
        private LispObject _eofMarker = new LispResolvedSymbol("(EOF-PACKAGE)", "(EOF)", isPublic: true);
        public readonly LispRootStackFrame RootFrame;
        private LispPackage _currentPackage;

        public bool UseTailCalls { get; }
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

        private LispHost(string filePath = null, TextReader input = null, TextWriter output = null, bool useTailCalls = false, Func<LispResolvedSymbol, LispObject> getUntrackedValue = null, Func<LispResolvedSymbol, LispObject, bool> trySetUntrackedValue = null)
        {
            InitialFilePath = filePath;
            RootFrame = new LispRootStackFrame(input ?? TextReader.Null, output ?? TextWriter.Null, getUntrackedValue, trySetUntrackedValue);
            var commonLispPackage = RootFrame.GetPackage(LispRootStackFrame.CommonLispPackageName);
            CurrentPackage = commonLispPackage;
            UseTailCalls = useTailCalls;
            AddContextObject(new LispSpecialOperatorsContext());
            AddContextObject(new LispDefaultContext());
        }

        public static async Task<LispHost> CreateAsync(string filePath = null, TextReader input = null, TextWriter output = null, bool useTailCalls = false, bool useInitScript = true, Func<LispResolvedSymbol, LispObject> getUntrackedValue = null, Func<LispResolvedSymbol, LispObject, bool> trySetUntrackedValue = null, CancellationToken cancellationToken = default)
        {
            var host = new LispHost(filePath, input, output, useTailCalls, getUntrackedValue, trySetUntrackedValue);
            if (useInitScript)
            {
                await host.ApplyInitScriptAsync(cancellationToken);
            }

            return host;
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

        public static async Task<string> GetInitScriptContents()
        {
            var type = typeof(LispHost);
            var lastDotIndex = type.FullName.LastIndexOf('.');
            var namespacePrefix = type.FullName.Substring(0, lastDotIndex);
            var assembly = type.GetTypeInfo().Assembly;
            using (var initStream = assembly.GetManifestResourceStream($"{namespacePrefix}.init.lisp"))
            using (var reader = new StreamReader(initStream))
            {
                var content = await reader.ReadToEndAsync();
                return content;
            }
        }

        private async Task ApplyInitScriptAsync(CancellationToken cancellationToken)
        {
            var content = await GetInitScriptContents();
            var evalResult = await EvalAsync("init.lisp", content, cancellationToken);
            if (evalResult.ExecutionState?.LastResult != T)
            {
                var message = $"Expected 'T' but found '{evalResult.ExecutionState?.LastResult}' at ({evalResult.ExecutionState?.LastResult.SourceLocation?.Start.Line}, {evalResult.ExecutionState?.LastResult.SourceLocation?.Start.Column}).";
                if (evalResult.ReadError != null)
                {
                    message += "\nRead error:\n" + evalResult.ReadError.ToString();
                }

                throw new Exception(message);
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
            var eofValue = new LispError("EOF");
            var textReader = new StringReader(code);
            var textStream = new LispTextStream("", textReader, TextWriter.Null);
            var objectReader = new LispObjectReader(this, textStream, allowIncompleteObjects: true);
            var result = new List<LispObject>();
            while (true)
            {
                var readResult = await objectReader.ReadAsync(RootFrame, false, eofValue, true, cancellationToken);
                if (ReferenceEquals(readResult.LastResult, eofValue))
                {
                    break;
                }

                result.Add(readResult.LastResult);
            }

            return result;
        }

        public LispExecutionState CreateExecutionState(string code) => CreateExecutionState(InitialFilePath, code);

        public LispExecutionState CreateExecutionState(string filePath, string code)
        {
            var executionState = LispExecutionState.CreateExecutionState(RootFrame, filePath, code, UseTailCalls, allowHalting: true);
            return executionState;
        }

        public Task<LispEvalResult> EvalAsync(string code, CancellationToken cancellationToken = default)
        {
            return EvalAsync(InitialFilePath, code, cancellationToken);
        }

        public async Task<LispEvalResult> EvalAsync(string filePath, string code, CancellationToken cancellationToken = default)
        {
            var executionState = CreateExecutionState(filePath, code);
            return await EvalContinueAsync(executionState, cancellationToken);
        }

        public async Task<LispEvalResult> EvalAsync(LispObject obj, CancellationToken cancellationToken = default)
        {
            var executionState = LispExecutionState.CreateExecutionState(RootFrame, "TODO: input name", obj, UseTailCalls, allowHalting: true, createDribbleInstructions: true);
            return await EvalContinueAsync(executionState, cancellationToken);
        }

        public async Task<LispEvalResult> EvalContinueAsync(LispExecutionState executionState, CancellationToken cancellationToken = default)
        {
            var evalResult = new LispEvalResult(executionState);
            var evaluationState = await RunQueuedOperationsAsync(executionState, cancellationToken);
            if (evaluationState != LispEvaluationState.Complete ||
                (executionState.LastResult is object && executionState.IsExecutionComplete))
            {
                return evalResult;
            }
            
            var objectReader = new LispObjectReader(this, executionState.CodeInputStream);
            var readerResult = await ReadWithoutDribbleStreamAsync(objectReader, executionState.StackFrame, cancellationToken);
            while (!ReferenceEquals(readerResult.LastResult, _eofMarker))
            {
                evalResult.ExpressionDepth = readerResult.ExpressionDepth;
                evalResult.IncompleteInput = readerResult.IncompleteInput;
                if (readerResult.LastResult is LispError readError)
                {
                    evalResult.ReadError = readError;
                    break;
                }

                executionState.TryPopArgument(out var _discard); // when evaluating multiple expressions, discard all but the last
                executionState.InsertObjectOperations(readerResult.LastResult, createDribbleInstructions: true);
                evaluationState = await RunQueuedOperationsAsync(executionState, cancellationToken);
                if (evaluationState != LispEvaluationState.Complete ||
                    evalResult.LastResult is LispError)
                {
                    break;
                }

                readerResult = await ReadWithoutDribbleStreamAsync(objectReader, executionState.StackFrame, cancellationToken);
            }

            return evalResult;
        }

        private async Task<LispEvaluationState> RunQueuedOperationsAsync(LispExecutionState executionState, CancellationToken cancellationToken)
        {
            var evaluationState = await LispEvaluator.EvaluateAsync(this, executionState, cancellationToken);
            RootFrame.OnHalted(evaluationState);
            return evaluationState;
        }

        private async Task<LispObjectReaderResult> ReadWithoutDribbleStreamAsync(LispObjectReader objectReader, LispStackFrame stackFrame, CancellationToken cancellationToken)
        {
            var dribbleStream = RootFrame.DribbleStream;
            RootFrame.DribbleStream = null;
            var obj = await objectReader.ReadAsync(stackFrame, false, _eofMarker, false, cancellationToken);
            RootFrame.DribbleStream = dribbleStream;
            return obj;
        }

        public async Task<LispObject> EvalAtStackFrameAsync(LispStackFrame frame, string code, CancellationToken cancellationToken = default)
        {
            var executionState = LispExecutionState.CreateExecutionState(frame, "REPL", code, useTailCalls: false, allowHalting: false);
            var evalResult = await EvalContinueAsync(executionState, cancellationToken);
            return evalResult.LastResult;
        }

        public async Task<LispObject> EvalAtStackFrameAsync(LispStackFrame frame, LispObject obj, CancellationToken cancellationToken = default)
        {
            var executionState = LispExecutionState.CreateExecutionState(frame, "TODO: input name", obj, useTailCalls: false, allowHalting: false, createDribbleInstructions: false);
            var evalResult = await EvalContinueAsync(executionState, cancellationToken);
            return evalResult.LastResult;
        }

        public async Task StepOverAsync(LispExecutionState executionState, CancellationToken cancellationToken = default)
        {
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

            // ... or the next expression (i.e., not current) has the same parent
            var currentExpression = executionState.PeekCurrentExpression();
            var expressionHalter = new EventHandler<LispEvaluatingExpressionEventArgs>((s, e) =>
            {
                // run until the next (i.e., not current) expression with the same parent
                if (!ReferenceEquals(currentExpression, e.Expression) &&
                    ReferenceEquals(currentExpression?.Parent, e.Expression?.Parent))
                {
                    e.HaltExecution = true;
                }
            });

            RootFrame.FunctionEntered += functionEnterEventHandler;
            RootFrame.FunctionReturned += functionExitedEventHandler;
            RootFrame.EvaluatingExpression += expressionHalter;
            await EvalContinueAsync(executionState, cancellationToken);
            RootFrame.EvaluatingExpression -= expressionHalter;
            RootFrame.FunctionReturned -= functionExitedEventHandler;
            RootFrame.FunctionEntered -= functionEnterEventHandler;

            if (exitedViaFunctionRefCount)
            {
                // if we were at the end of a function and actually stepped out, halt on the next expression
                await HaltOnNextExpressionAsync(executionState, 0, cancellationToken);
            }
        }

        public async Task StepInAsync(LispExecutionState executionState, CancellationToken cancellationToken = default)
        {
            // halting without skipping will halt where we're already halted
            await HaltOnNextExpressionAsync(executionState, 1, cancellationToken);
        }

        public async Task StepOutAsync(LispExecutionState executionState, CancellationToken cancellationToken = default)
        {
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
            await HaltOnNextExpressionAsync(executionState, 0, cancellationToken);
        }

        private async Task HaltOnNextExpressionAsync(LispExecutionState executionState, int skipExpressionCount, CancellationToken cancellationToken)
        {
            // allows various pop and exit operations to be processed
            var skippedExpressions = 0;
            var nextExpressionHalter = new EventHandler<LispEvaluatingExpressionEventArgs>((s, e) =>
            {
                if (skippedExpressions == skipExpressionCount)
                {
                    e.HaltExecution = true;
                }
                else
                {
                    skippedExpressions++;
                }
            });
            RootFrame.EvaluatingExpression += nextExpressionHalter;
            await EvalContinueAsync(executionState, cancellationToken);
            RootFrame.EvaluatingExpression -= nextExpressionHalter;
        }
    }
}
