using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

namespace IxMilia.Lisp
{
    // https://gist.github.com/chaitanyagupta/9324402
    public delegate void LispSpecialOperatorDelegate(LispHost host, LispExecutionState executionState, LispObject[] args); // args unevaluated, no result
    public delegate LispObject LispMacroDelegate(LispHost host, LispExecutionState executionState, LispObject[] args); // args unevaluated, result evaluated
    public delegate LispObject LispFunctionDelegate(LispHost host, LispExecutionState executionState, LispObject[] args); // args evaluated, result unevaluated

    public class LispHost
    {
        private const string PackageString = "*PACKAGE*";
        private const string TerminalIOString = "*TERMINAL-IO*";

        private string _initialFilePath;
        private LispObjectReader _objectReader;
        private LispObject _eofMarker = new LispResolvedSymbol("(EOF-PACKAGE)", "(EOF)", isPublic: true);
        public readonly LispRootStackFrame RootFrame;
        private LispPackage _currentPackage;

        internal LispObjectReader ObjectReader => _objectReader;
        public bool UseTailCalls { get; }
        public LispObject T { get; }
        public LispObject Nil { get; }
        public LispTextStream TerminalIO { get; }
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

        public LispHost(string filePath = null, TextReader input = null, TextWriter output = null, bool useTailCalls = false, bool useInitScript = true)
        {
            _initialFilePath = filePath;
            RootFrame = new LispRootStackFrame(input ?? TextReader.Null, output ?? TextWriter.Null);
            var commonLispPackage = RootFrame.GetPackage(LispRootStackFrame.CommonLispPackageName);
            CurrentPackage = commonLispPackage;
            UseTailCalls = useTailCalls;
            T = RootFrame.T;
            Nil = RootFrame.Nil;
            TerminalIO = RootFrame.TerminalIO;
            AddContextObject(new LispSpecialOperatorsContext());
            AddContextObject(new LispDefaultContext());

            var nullStream = new LispTextStream("<null>", TextReader.Null, TextWriter.Null);
            _objectReader = new LispObjectReader(this);
            _objectReader.SetReaderStream(nullStream);
            if (useInitScript)
            {
                ApplyInitScript();
            }
        }

        public LispPackage AddPackage(string packageName, IEnumerable<LispPackage> inheritedPackages = null) => RootFrame.AddPackage(packageName, inheritedPackages);

        public void AddSpecialOperator(LispSymbol symbol, LispSpecialOperatorDelegate del)
        {
            var resolvedSymbol = symbol.Resolve(CurrentPackage);
            var op = new LispSpecialOperator(resolvedSymbol, del);
            SetValue(resolvedSymbol.Value, op, createPackage: true);
        }

        public void AddMacro(string name, LispMacroDelegate del) => AddMacro(LispSymbol.CreateFromString(name), del);

        public void AddMacro(LispSymbol symbol, LispMacroDelegate del) => AddMacro(symbol, null, del);

        public void AddMacro(LispSymbol symbol, string documentation, LispMacroDelegate del)
        {
            var resolvedSymbol = symbol.Resolve(CurrentPackage);
            var macro = new LispNativeMacro(resolvedSymbol, documentation, del);
            SetValue(resolvedSymbol.Value, macro, createPackage: true);
        }

        public void AddFunction(string name, LispFunctionDelegate del) => AddFunction(LispSymbol.CreateFromString(name), del);

        public void AddFunction(LispSymbol symbol, LispFunctionDelegate del) => AddFunction(symbol, null, del);

        public void AddFunction(LispSymbol symbol, string documentation, LispFunctionDelegate del)
        {
            var resolvedSymbol = symbol.Resolve(CurrentPackage);
            var function = new LispNativeFunction(resolvedSymbol, documentation, del);
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
                if (parameterInfo.Length == 3 &&
                    parameterInfo[0].ParameterType == typeof(LispHost) &&
                    parameterInfo[1].ParameterType == typeof(LispExecutionState) &&
                    parameterInfo[2].ParameterType == typeof(LispObject[]))
                {
                    // native special operators (unevaluated arguments, no result)
                    var operatorNames = methodInfo.GetCustomAttributes<LispSpecialOperatorAttribute>(inherit: true).Select(a => a.Name).ToList();
                    if (operatorNames.Any())
                    {
                        if (methodInfo.ReturnType == typeof(void))
                        {
                            var del = (LispSpecialOperatorDelegate)methodInfo.CreateDelegate(typeof(LispSpecialOperatorDelegate), context);
                            foreach (var name in operatorNames)
                            {
                                var symbol = LispSymbol.CreateFromString(name);
                                AddSpecialOperator(symbol, del);
                            }
                        }
                        else
                        {
                            throw new InvalidOperationException($"Native special operator expected to have return type of 'void' but found '{methodInfo.ReturnType.Name}'.");
                        }
                    }

                    // native macros (unevaluated arguments, evaluated result)
                    var macroNames = methodInfo.GetCustomAttributes<LispMacroAttribute>(inherit: true).Select(a => a.Name).ToList();
                    if (macroNames.Any())
                    {
                        if (methodInfo.ReturnType == typeof(LispObject))
                        {
                            var del = (LispMacroDelegate)methodInfo.CreateDelegate(typeof(LispMacroDelegate), context);
                            foreach (var name in macroNames)
                            {
                                var symbol = LispSymbol.CreateFromString(name);
                                AddMacro(symbol, del);
                            }
                        }
                        else
                        {
                            throw new InvalidOperationException($"Native macro expected to have return type of '{nameof(LispObject)}' but found '{methodInfo.ReturnType.Name}'.");
                        }
                    }

                    // native functions (evaluated arguments, unevaluated result)
                    var functionNames = methodInfo.GetCustomAttributes<LispFunctionAttribute>(inherit: true).Select(a => a.Name).ToList();
                    if (functionNames.Any())
                    {
                        if (methodInfo.ReturnType == typeof(LispObject))
                        {
                            var del = (LispFunctionDelegate)methodInfo.CreateDelegate(typeof(LispFunctionDelegate), context);
                            foreach (var name in functionNames)
                            {
                                var symbol = LispSymbol.CreateFromString(name);
                                AddFunction(symbol, del);
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

        private void ApplyInitScript()
        {
            var type = GetType();
            var lastDotIndex = type.FullName.LastIndexOf('.');
            var namespacePrefix = type.FullName.Substring(0, lastDotIndex);
            var assembly = type.GetTypeInfo().Assembly;
            using (var initStream = assembly.GetManifestResourceStream($"{namespacePrefix}.init.lisp"))
            using (var reader = new StreamReader(initStream))
            {
                var content = reader.ReadToEnd();
                var evalResult = Eval("init.lisp", content);
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

        public LispEvalResult Eval(string code)
        {
            return Eval(_initialFilePath, code);
        }

        public LispEvalResult Eval(string filePath, string code)
        {
            var executionState = LispExecutionState.CreateExecutionState(RootFrame, filePath, code, UseTailCalls, allowHalting: true);
            _objectReader.SetReaderStream(executionState.CodeInputStream);
            return EvalContinue(executionState);
        }

        public LispEvalResult Eval(LispObject obj)
        {
            var executionState = LispExecutionState.CreateExecutionState(RootFrame, "TODO: input name", obj, UseTailCalls, allowHalting: true, createDribbleInstructions: true);
            return EvalContinue(executionState);
        }

        public LispEvalResult EvalContinue(LispExecutionState executionState)
        {
            var evalResult = new LispEvalResult(executionState);

            var evaluationState = RunQueuedOperations(executionState);
            if (evaluationState != LispEvaluationState.Complete ||
                (executionState.LastResult is object && executionState.IsExecutionComplete))
            {
                return evalResult;
            }

            var readerResult = ReadWithoutDribbleStream();
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
                evaluationState = RunQueuedOperations(executionState);
                if (evaluationState != LispEvaluationState.Complete ||
                    evalResult.LastResult is LispError)
                {
                    break;
                }

                readerResult = ReadWithoutDribbleStream();
            }

            return evalResult;
        }

        private LispEvaluationState RunQueuedOperations(LispExecutionState executionState)
        {
            var evaluationState = LispEvaluator.Evaluate(this, executionState);
            RootFrame.OnHalted(evaluationState);
            return evaluationState;
        }

        private LispObjectReaderResult ReadWithoutDribbleStream()
        {
            var dribbleStream = RootFrame.DribbleStream;
            RootFrame.DribbleStream = null;
            var obj = _objectReader.Read(false, _eofMarker, false);
            RootFrame.DribbleStream = dribbleStream;
            return obj;
        }

        public LispObject EvalAtStackFrame(LispStackFrame frame, LispObject obj)
        {
            var executionState = LispExecutionState.CreateExecutionState(frame, "TODO: input name", obj, useTailCalls: false, allowHalting: false, createDribbleInstructions: false);
            var evalResult = EvalContinue(executionState);
            return evalResult.LastResult;
        }

        public void StepOver(LispExecutionState executionState)
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
            EvalContinue(executionState);
            RootFrame.EvaluatingExpression -= expressionHalter;
            RootFrame.FunctionReturned -= functionExitedEventHandler;
            RootFrame.FunctionEntered -= functionEnterEventHandler;

            if (exitedViaFunctionRefCount)
            {
                // if we were at the end of a function and actually stepped out, halt on the next expression
                HaltOnNextExpression(executionState);
            }
        }

        public void StepIn(LispExecutionState executionState)
        {
            // halting without skipping will halt where we're already halted
            HaltOnNextExpression(executionState, skipExpressionCount: 1);
        }

        public void StepOut(LispExecutionState executionState)
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
            EvalContinue(executionState);
            RootFrame.FunctionReturned -= functionExitHalter;
            RootFrame.FunctionEntered -= functionEnterHandler;

            // ...then halt on the very next expression
            HaltOnNextExpression(executionState);
        }

        private void HaltOnNextExpression(LispExecutionState executionState, int skipExpressionCount = 0)
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
            EvalContinue(executionState);
            RootFrame.EvaluatingExpression -= nextExpressionHalter;
        }
    }
}
