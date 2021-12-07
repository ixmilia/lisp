using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using IxMilia.Lisp.Parser;
using IxMilia.Lisp.Tokens;

namespace IxMilia.Lisp
{
    // https://gist.github.com/chaitanyagupta/9324402
    public delegate void LispSpecialOperatorDelegate(LispHost host, LispExecutionState executionState, LispObject[] args); // args unevaluated, no result
    public delegate LispObject LispMacroDelegate(LispHost host, LispExecutionState executionState, LispObject[] args); // args unevaluated, result evaluated
    public delegate LispObject LispFunctionDelegate(LispHost host, LispExecutionState executionState, LispObject[] args); // args evaluated, result unevaluated

    public class LispHost
    {
        private const string TerminalIOString = "*TERMINAL-IO*";

        private string _initialFilePath;
        public readonly LispRootStackFrame RootFrame;

        public bool UseTailCalls { get; }
        public LispObject T { get; }
        public LispObject Nil { get; }
        public LispStream TerminalIO { get; }

        public LispHost(string filePath = null, TextReader input = null, TextWriter output = null, bool useTailCalls = false)
        {
            _initialFilePath = filePath;
            RootFrame = new LispRootStackFrame(input ?? TextReader.Null, output ?? TextWriter.Null);
            UseTailCalls = useTailCalls;
            T = RootFrame.T;
            Nil = RootFrame.Nil;
            TerminalIO = RootFrame.TerminalIO;
            AddContextObject(new LispSpecialOperatorsContext());
            AddContextObject(new LispDefaultContext());
            ApplyInitScript();
        }

        public void AddSpecialOperator(string name, LispSpecialOperatorDelegate del)
        {
            var op = new LispSpecialOperator(name, del);
            SetValue(name, op);
        }

        public void AddMacro(string name, LispMacroDelegate del)
        {
            var macro = new LispNativeMacro(name, del);
            SetValue(name, macro);
        }

        public void AddFunction(string name, LispFunctionDelegate del)
        {
            AddFunction(name, null, del);
        }

        public void AddFunction(string name, string documentation, LispFunctionDelegate del)
        {
            var function = new LispNativeFunction(name, documentation, del);
            SetValue(name, function);
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
                                AddSpecialOperator(name, del);
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
                                AddMacro(name, del);
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
                                AddFunction(name, del);
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
                var executionState = Eval("init.lisp", content);
                if (executionState.LastResult != T)
                {
                    throw new Exception($"Expected 'T' but found '{executionState.LastResult}' at ({executionState.LastResult.SourceLocation?.Line}, {executionState.LastResult.SourceLocation?.Column}).");
                }

                //RootFrame.UpdateCallStackLocation(new LispInteger(0)
                //{
                //    SourceLocation = new LispSourceLocation(_initialFilePath, RootFrame.SourceLocation?.Line ?? 0, RootFrame.SourceLocation?.Column ?? 0)
                //});
            }
        }

        public void SetValue(string name, LispObject value)
        {
            RootFrame.SetValue(name, value);
        }

        public void SetValueInParentScope(string name, LispObject value)
        {
            if (RootFrame.Parent is object)
            {
                RootFrame.Parent.SetValue(name, value);
            }
            else
            {
                SetValue(name, value);
            }
        }

        public LispObject GetValue(string name)
        {
            return RootFrame.GetValue(name);
        }

        public TObject GetValue<TObject>(string name) where TObject: LispObject
        {
            return RootFrame.GetValue<TObject>(name);
        }

        public IEnumerable<LispObject> Parse(string code)
        {
            return Parse(_initialFilePath, code);
        }

        public IEnumerable<LispObject> Parse(string filePath, string code)
        {
            var tokenizer = new LispTokenizer(filePath, code);
            var tokens = tokenizer.GetTokens();
            var parser = new LispParser(tokens);
            var nodes = parser.Parse().Nodes;
            return nodes;
        }

        public LispExecutionState CreateExecutionState(string code)
        {
            return CreateExecutionState(_initialFilePath, code);
        }

        public LispExecutionState CreateExecutionState(string filePath, string code)
        {
            var nodes = Parse(filePath, code);
            return CreateExecutionState(nodes);
        }

        public LispExecutionState CreateExecutionState(IEnumerable<LispObject> nodes)
        {
            return LispExecutionState.CreateExecutionState(RootFrame, nodes, UseTailCalls, allowHalting: true, createDribbleInstructions: true);
        }

        public LispExecutionState Eval(string code)
        {
            return Eval(_initialFilePath, code);
        }

        public LispExecutionState Eval(string filePath, string code)
        {
            var nodes = Parse(filePath, code);
            return Eval(nodes);
        }

        public LispExecutionState Eval(LispObject obj)
        {
            return Eval(new LispObject[] { obj });
        }

        public LispExecutionState Eval(IEnumerable<LispObject> nodes)
        {
            var executionState = CreateExecutionState(nodes);
            Run(executionState);
            return executionState;
        }

        public LispObject EvalAtStackFrame(LispStackFrame frame, LispObject obj)
        {
            var executionState = LispExecutionState.CreateExecutionState(frame, new[] { obj }, useTailCalls: false, allowHalting: false, createDribbleInstructions: false);
            LispEvaluator.Evaluate(this, executionState);
            return executionState.LastResult;
        }

        public void Run(LispExecutionState executionState)
        {
            var evaluationState = LispEvaluator.Evaluate(this, executionState);
            RootFrame.OnHalted(evaluationState);
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
            Run(executionState);
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
            Run(executionState);
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
            Run(executionState);
            RootFrame.EvaluatingExpression -= nextExpressionHalter;
        }
    }
}
