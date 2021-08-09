using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using IxMilia.Lisp.Parser;
using IxMilia.Lisp.Tokens;

namespace IxMilia.Lisp
{
    public delegate IEnumerable<LispObject> LispMacroDelegate(LispStackFrame frame, LispObject[] args);
    public delegate LispObject LispFunctionDelegate(LispStackFrame frame, LispObject[] args);

    public class LispHost
    {
        private const string TerminalIOString = "*terminal-io*";

        private string _initialFilePath;
        public readonly LispRootStackFrame RootFrame;

        public LispObject T { get; }
        public LispObject Nil { get; }
        public LispStream TerminalIO { get; }

        public LispHost(string filePath = null, TextReader input = null, TextWriter output = null)
        {
            _initialFilePath = filePath;
            RootFrame = new LispRootStackFrame(input ?? TextReader.Null, output ?? TextWriter.Null);
            T = RootFrame.T;
            Nil = RootFrame.Nil;
            TerminalIO = RootFrame.TerminalIO;
            AddContextObject(new LispDefaultContext());
            ApplyInitScript();
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
                if (parameterInfo.Length == 2 &&
                    parameterInfo[0].ParameterType == typeof(LispStackFrame) &&
                    parameterInfo[1].ParameterType == typeof(LispObject[]))
                {
                    // native macros (unevaluated arguments)
                    var macroNames = methodInfo.GetCustomAttributes<LispMacroAttribute>(inherit: true).Select(a => a.Name).ToList();
                    if (macroNames.Any())
                    {
                        if (methodInfo.ReturnType == typeof(IEnumerable<LispObject>))
                        {
                            var del = (LispMacroDelegate)methodInfo.CreateDelegate(typeof(LispMacroDelegate), context);
                            foreach (var name in macroNames)
                            {
                                AddMacro(name, del);
                            }
                        }
                        else
                        {
                            throw new InvalidOperationException($"Native macro expected to have return type of '{nameof(IEnumerable<LispObject>)}' but found '{methodInfo.ReturnType.Name}'.");
                        }
                    }

                    // native functions (evaluated arguments)
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
                    throw new Exception($"Expected 't' but found '{executionState.LastResult}' at ({executionState.LastResult.SourceLocation?.Line}, {executionState.LastResult.SourceLocation?.Column}).");
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

        public LispExecutionState Eval(string code)
        {
            return Eval(_initialFilePath, code);
        }

        public LispExecutionState Eval(string filePath, string code)
        {
            var tokenizer = new LispTokenizer(filePath, code);
            var tokens = tokenizer.GetTokens();
            var parser = new LispParser(tokens);
            var nodes = parser.Parse().Nodes;
            return Eval(nodes);
        }

        public LispExecutionState Eval(IEnumerable<LispObject> nodes)
        {
            var executionState = LispExecutionState.CreateExecutionState(RootFrame, nodes, createDribbleInstructions: true);
            executionState = LispEvaluator.Evaluate(executionState);
            return executionState;
        }

        public LispExecutionState Eval(LispObject obj)
        {
            return Eval(new LispObject[] { obj });
        }

        private void TryApplyStackFrame(LispError error)
        {
            if (error.StackFrame == null)
            {
                error.StackFrame = RootFrame;
            }
        }
    }
}
