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

        public LispObject Nil => GetValue<LispList>(NilString);
        public LispObject T => GetValue<LispSymbol>(TString);

        public LispHost()
        {
            _scope = new LispScope(this);
            AddWellKnownSymbols();
            AddContextObject(new LispDefaultContext());
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
            return symbol.IsQuoted
                ? symbol
                : GetValue(symbol.Value);
        }

        private LispObject EvalList(LispList list)
        {
            if (list.IsQuoted || list.IsNil)
            {
                return list;
            }

            var functionNameSymbol = (LispSymbol)list.Value;
            var functionName = functionNameSymbol.Value;
            var args = list.ToList().Skip(1).Select(GetMacroExpansion).ToArray();
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

                // bind arguments
                var evaluatedArgs = args.Select(a => Eval(a)).ToArray();
                var firstError = evaluatedArgs.OfType<LispError>().FirstOrDefault();
                if (firstError != null)
                {
                    result = firstError;
                }
                else
                {
                    // TODO: validate argument count
                    for (int i = 0; i < function.Arguments.Length; i++)
                    {
                        SetValue(function.Arguments[i], evaluatedArgs[i]);
                    }

                    // eval values
                    result = Eval(function.Commands);
                    DecreaseScope();
                    PopStackFrame();
                }
            }
            else if (_macroMap.TryGetValue(functionName, out var macro))
            {
                PushStackFrame(functionName);
                result = macro.Invoke(this, args);
                PopStackFrame();
            }
            else if (_delegateMap.TryGetValue(functionName, out var function))
            {
                var evaluatedArgs = args.Select(a => Eval(a)).ToArray();
                var firstError = evaluatedArgs.OfType<LispError>().FirstOrDefault();
                if (firstError != null)
                {
                    result = firstError;
                }
                else
                {
                    result = function.Invoke(this, evaluatedArgs);
                }
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
