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
        private readonly LispStackFrame _rootFrame;

        public LispObject T { get; }
        public LispObject Nil { get; }

        public LispHost()
        {
            _rootFrame = LispStackFrame.CreateRootStackFrame();
            T = _rootFrame.T;
            Nil = _rootFrame.Nil;
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
                var result = Eval(content);
                if (result != T)
                {
                    throw new Exception($"Expected 't' but found '{result}' at ({result.Line}, {result.Column}).");
                }
            }
        }

        public void SetValue(string name, LispObject value)
        {
            _rootFrame.SetValue(name, value);
        }

        public void SetValueInParentScope(string name, LispObject value)
        {
            if (_rootFrame.Parent is object)
            {
                _rootFrame.Parent.SetValue(name, value);
            }
            else
            {
                SetValue(name, value);
            }
        }

        public LispObject GetValue(string name)
        {
            return _rootFrame.GetValue(name);
        }

        public TObject GetValue<TObject>(string name) where TObject: LispObject
        {
            return _rootFrame.GetValue<TObject>(name);
        }

        public LispObject Eval(string code)
        {
            var tokenizer = new LispTokenizer(code);
            var tokens = tokenizer.GetTokens();
            var parser = new LispParser(tokens);
            var nodes = parser.Parse().Nodes;
            return Eval(nodes);
        }

        public LispObject Eval(IEnumerable<LispObject> nodes)
        {
            LispObject lastValue = null;
            foreach (var node in nodes)
            {
                lastValue = Eval(node);
                if (lastValue is LispError error)
                {
                    TryApplyStackFrame(error);
                    break;
                }
            }

            return lastValue;
        }

        public LispObject Eval(LispObject obj)
        {
            return LispEvaluator.Evaluate(obj, _rootFrame, false);
        }

        private void TryApplyStackFrame(LispError error)
        {
            if (error.StackFrame == null)
            {
                error.StackFrame = _rootFrame;
            }
        }
    }
}
