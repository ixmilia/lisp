using System;
using System.Collections.Generic;
using System.Diagnostics;
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
        private readonly Dictionary<string, LispFunctionDelegate> _delegateMap = new Dictionary<string, LispFunctionDelegate>();
        private LispScope _scope;
        private LispStackFrame _currentFrame = new LispStackFrame("<root>", null);

        public LispSymbol Nil => GetValue<LispSymbol>(NilString);
        public LispSymbol T => GetValue<LispSymbol>(TString);

        public LispHost()
        {
            _scope = new LispScope(this);
            AddWellKnownSymbols();
            AddContextObject(new LispDefaultContext());
        }

        private void AddWellKnownSymbols()
        {
            void addSymbol(string name)
            {
                SetValue(name, new LispSymbol(name));
            }
            addSymbol(NilString);
            addSymbol(TString);
        }

        public void AddMacro(string name, LispMacroDelegate del)
        {
            _macroMap[name] = del;
        }

        public void AddFunction(string name, LispFunctionDelegate del)
        {
            _delegateMap[name] = del;
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
                    var macroAttribute = methodInfo.GetCustomAttribute<LispMacroAttribute>(inherit: true);
                    if (macroAttribute != null)
                    {
                        var del = (LispMacroDelegate)methodInfo.CreateDelegate(typeof(LispMacroDelegate), context);
                        AddMacro(macroAttribute.Name, del);
                    }

                    // native functions (evaluated arguments)
                    var functionAttribute = methodInfo.GetCustomAttribute<LispFunctionAttribute>(inherit: true);
                    if (functionAttribute != null)
                    {
                        var del = (LispFunctionDelegate)methodInfo.CreateDelegate(typeof(LispFunctionDelegate), context);
                        AddFunction(functionAttribute.Name, del);
                    }
                }
            }
        }

        private void SetMacroExpansion(string name, LispObject expansion)
        {
            _scope.SetMacroExpansion(name, expansion);
        }

        private LispObject GetMacroExpansion(LispObject obj)
        {
            if (obj is LispSymbol symbol)
            {
                return _scope.GetMacroExpansion(symbol.Value) ?? symbol;
            }
            else
            {
                return obj;
            }
        }

        public void SetValue(string name, LispObject value)
        {
            _scope[name] = value;
        }

        public LispObject GetValue(string name)
        {
            return _scope[name];
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
            var nodes = parser.Parse();
            return Eval(nodes);
        }

        public LispObject Eval(IEnumerable<LispObject> nodes)
        {
            LispObject lastValue = Nil;
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
            UpdateCallStackLocation(obj);
            switch (obj)
            {
                case LispNumber _:
                case LispString _:
                    return obj;
                case LispSymbol symbol:
                    return EvalSymbol(symbol);
                case LispList list:
                    return EvalList(list);
                default:
                    return Nil;
            }
        }

        private LispObject EvalSymbol(LispSymbol symbol)
        {
            return symbol.IsQuoted
                ? symbol
                : GetValue(symbol.Value);
        }

        private LispObject EvalList(LispList list)
        {
            if (list.IsQuoted)
            {
                return list;
            }

            var functionNameSymbol = (LispSymbol)list.Value.First();
            var functionName = functionNameSymbol.Value;
            var args = list.Value.Skip(1).Select(GetMacroExpansion).ToArray();
            var value = GetValue(functionName);
            UpdateCallStackLocation(functionNameSymbol);
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
                var lastValue = (LispObject)Nil;
                foreach (var item in macro.Body)
                {
                    lastValue = Eval(item);
                }

                PopStackFrame();

                return lastValue;
            }
            else if (value is LispFunction)
            {
                // TODO: what if it's a regular variable?
                var function = (LispFunction)value;
                PushStackFrame(function.Name);
                IncreaseScope();

                // bind arguments
                // TODO: validate argument count
                for (int i = 0; i < function.Arguments.Length; i++)
                {
                    SetValue(function.Arguments[i], Eval(args[i]));
                }

                // eval values
                var result = Eval(function.Commands);
                DecreaseScope();
                PopStackFrame();
                return result;
            }
            else if (_macroMap.TryGetValue(functionName, out var macro))
            {
                PushStackFrame(functionName);
                var body = macro.Invoke(this, args);
                var result = Eval(body);
                PopStackFrame();
                return result;
            }
            else if (_delegateMap.TryGetValue(functionName, out var function))
            {
                var evaluatedArgs = args.Select(a => Eval(a)).ToArray();
                var result = function.Invoke(this, evaluatedArgs);
                return result;
            }
            else
            {
                return GenerateError($"Undefined macro/function '{functionName}'");
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
