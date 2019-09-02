using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using IxMilia.Lisp.Parser;
using IxMilia.Lisp.Tokens;

namespace IxMilia.Lisp
{
    public delegate LispObject LispDelegate(LispHost host, LispObject[] args);

    public class LispHost
    {
        private const string NilString = "nil";
        private const string TString = "t";
        private readonly Dictionary<string, LispDelegate> _delegateMap = new Dictionary<string, LispDelegate>();
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

        public void AddFunction(string name, LispDelegate del)
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
                var lispAttribute = methodInfo.GetCustomAttribute<LispValueAttribute>(inherit: true);
                if (lispAttribute != null)
                {
                    var parameterInfo = methodInfo.GetParameters();
                    if (parameterInfo.Length == 2 &&
                        parameterInfo[0].ParameterType == typeof(LispHost) &&
                        parameterInfo[1].ParameterType == typeof(LispObject[]) &&
                        methodInfo.ReturnType== typeof(LispObject))
                    {
                        var del = (LispDelegate)methodInfo.CreateDelegate(typeof(LispDelegate), context);
                        AddFunction(lispAttribute.Name, del);
                    }
                }
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
            var parent = _scope.Parent;
            if (parent != null)
            {
                _scope = parent;
            }
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
                    return symbol.IsQuoted
                        ? symbol
                        : GetValue(symbol.Value);
                case LispList list:
                    return list.IsQuoted
                        ? list
                        : EvalList(list);
                default:
                    return Nil;
            }
        }

        private LispObject EvalList(LispList list)
        {
            var functionNameSymbol = (LispSymbol)list.Value.First();
            var functionName = functionNameSymbol.Value;
            var args = list.Value.Skip(1).ToArray();
            var value = GetValue(functionName);
            UpdateCallStackLocation(functionNameSymbol);
            if (value is LispFunction)
            {
                // TODO: what if it's a regular variable?
                var function = (LispFunction)value;
                PushStackFrame(functionNameSymbol.Value);
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
            else if (_delegateMap.TryGetValue(functionName, out var function))
            {
                var result = function.Invoke(this, args);
                return result;
            }
            else
            {
                return GenerateError($"Undefined function '{functionName}'");
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
