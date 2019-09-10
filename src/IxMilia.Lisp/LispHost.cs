using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using IxMilia.Lisp.Parser;
using IxMilia.Lisp.Tokens;

namespace IxMilia.Lisp
{
    public delegate LispObject LispMacroDelegate(LispHost host, LispObject[] args);
    public delegate LispObject LispFunctionDelegate(LispHost host, LispObject[] args);

    public class LispHost
    {
        private const string NilString = "nil";
        private const string TString = "t";
        private readonly Dictionary<string, LispMacroDelegate> _macroMap = new Dictionary<string, LispMacroDelegate>();
        private LispScope _scope;
        private LispStackFrame _currentFrame = new LispStackFrame("<root>", null);

        public LispObject Nil => GetValue<LispList>(NilString);
        public LispObject T => GetValue<LispSymbol>(TString);

        public LispHost()
        {
            _scope = new LispScope(this);
            AddWellKnownSymbols();
            AddContextObject(new LispDefaultContext());
            ApplyInitScript();
        }

        private void AddWellKnownSymbols()
        {
            SetValue(TString, new LispSymbol(TString));
            SetValue(NilString, LispNilList.Instance);
        }

        public void AddMacro(string name, LispMacroDelegate del)
        {
            _macroMap[name] = del;
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
                    parameterInfo[0].ParameterType == typeof(LispHost) &&
                    parameterInfo[1].ParameterType == typeof(LispObject[]) &&
                    methodInfo.ReturnType == typeof(LispObject))
                {
                    // native macros (unevaluated arguments)
                    var macroNames = methodInfo.GetCustomAttributes<LispMacroAttribute>(inherit: true).Select(a => a.Name).ToList();
                    if (macroNames.Any())
                    {
                        var del = (LispMacroDelegate)methodInfo.CreateDelegate(typeof(LispMacroDelegate), context);
                        foreach (var name in macroNames)
                        {
                            AddMacro(name, del);
                        }
                    }

                    // native functions (evaluated arguments)
                    var functionNames = methodInfo.GetCustomAttributes<LispFunctionAttribute>(inherit: true).Select(a => a.Name).ToList();
                    if (functionNames.Any())
                    {
                        var del = (LispFunctionDelegate)methodInfo.CreateDelegate(typeof(LispFunctionDelegate), context);
                        foreach (var name in functionNames)
                        {
                            AddFunction(name, del);
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

        private void SetMacroExpansion(string name, LispObject expansion)
        {
            _scope.SetMacroExpansion(name, expansion);
        }

        public void SetValue(string name, LispObject value)
        {
            _scope[name] = value;
        }

        public LispObject GetValue(string name)
        {
            var expansion = _scope.GetMacroExpansion(name);
            if (expansion != null)
            {
                return Eval(expansion);
            }
            else
            {
                return _scope[name];
            }
        }

        public TObject GetValue<TObject>(string name) where TObject: LispObject
        {
            return (TObject)GetValue(name);
        }

        private void IncreaseScope()
        {
            _scope = new LispScope(this, _scope);
        }

        private void DecreaseScope()
        {
            Debug.Assert(_scope != null);
            Debug.Assert(_scope.Parent != null);
            _scope = _scope.Parent;
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
            if (obj is LispError)
            {
                return obj;
            }

            UpdateCallStackLocation(obj);
            switch (obj)
            {
                case LispNumber _:
                case LispString _:
                    return obj;
                case LispQuotedObject quote:
                    return quote.Value;
                case LispSymbol symbol:
                    return EvalSymbol(symbol);
                case LispList list:
                    return EvalList(list);
                case LispForwardListReference forwardRef:
                    return EvalForwardReference(forwardRef);
                default:
                    return Nil;
            }
        }

        private LispObject EvalSymbol(LispSymbol symbol)
        {
            return GetValue(symbol.Value);
        }

        private LispObject EvalList(LispList list)
        {
            if (list.IsNil)
            {
                return list;
            }

            var functionNameSymbol = (LispSymbol)list.Value;
            var functionName = functionNameSymbol.Value;
            var args = list.ToList().Skip(1).ToArray();
            var value = GetValue(functionName);
            UpdateCallStackLocation(functionNameSymbol);
            LispObject result;
            if (value is LispMacro)
            {
                var macro = (LispMacro)value;
                PushStackFrame(macro.Name);
                // scope not modified when evaluating macros; stack is modified to give better errors

                // TODO: bind arguments by replacement

                // bind arguments
                for (int i = 0; i < macro.Arguments.Length; i++)
                {
                    SetMacroExpansion(macro.Arguments[i], args[i]);
                }

                // expand body
                var lastValue = Nil;
                foreach (var item in macro.Body)
                {
                    lastValue = Eval(item);
                }

                PopStackFrame();
                result = lastValue;
            }
            else if (value is LispFunction)
            {
                // TODO: what if it's a regular variable?
                var function = (LispFunction)value;
                PushStackFrame(function.Name);
                IncreaseScope();

                // evaluate arguments
                var evaluatedArgs = args.Select(a => Eval(a)).ToArray();
                var firstError = evaluatedArgs.OfType<LispError>().FirstOrDefault();
                if (firstError != null)
                {
                    result = firstError;
                }
                else
                {
                    result = function.Execute(this, evaluatedArgs);
                }

                DecreaseScope();
                PopStackFrame();
            }
            else if (_macroMap.TryGetValue(functionName, out var macro))
            {
                PushStackFrame(functionName);
                result = macro.Invoke(this, args);
                PopStackFrame();
            }
            else
            {
                result = GenerateError($"Undefined macro/function '{functionName}'");
            }

            TryApplyLocation(result, functionNameSymbol);
            return result;
        }

        private LispObject EvalForwardReference(LispForwardListReference forwardRef)
        {
            LispObject result;
            var finalList = new LispCircularList();
            SetValue(forwardRef.ForwardReference.SymbolReference, finalList);
            var values = forwardRef.List.ToList();
            var evaluatedValues = values.Select(Eval);
            var firstError = evaluatedValues.OfType<LispError>().FirstOrDefault();
            if (firstError != null)
            {
                result = firstError;
            }
            else
            {
                var tempList = forwardRef.List.IsProperList
                    ? LispList.FromEnumerable(evaluatedValues)
                    : LispList.FromEnumerableImproper(evaluatedValues.First(), evaluatedValues.Skip(1).First(), evaluatedValues.Skip(2));
                finalList.ApplyForCircularReference(tempList, isProperList: forwardRef.List.IsProperList);
                result = finalList;
            }

            TryApplyLocation(result, forwardRef);
            return result;
        }

        private static void TryApplyLocation(LispObject obj, LispObject parent)
        {
            if (obj.Line == 0 && obj.Column == 0)
            {
                obj.Line = parent.Line;
                obj.Column = parent.Column;
            }
        }

        private void UpdateCallStackLocation(LispObject obj)
        {
            _currentFrame.Line = obj.Line;
            _currentFrame.Column = obj.Column;
        }

        private void PushStackFrame(string functionName)
        {
            _currentFrame = new LispStackFrame(functionName, _currentFrame);
        }

        private void PopStackFrame()
        {
            _currentFrame = _currentFrame.Parent;
        }

        private LispError GenerateError(string message)
        {
            var error = new LispError(message);
            TryApplyStackFrame(error);
            return error;
        }

        private void TryApplyStackFrame(LispError error)
        {
            if (error.StackFrame == null)
            {
                error.StackFrame = _currentFrame;
            }
        }
    }
}
