using System;
using System.Collections.Generic;
using System.Linq;

namespace IxMilia.Lisp
{
    public class LispStackFrame
    {
        protected const string NilString = "nil";
        protected const string TString = "t";

        private Dictionary<string, LispObject> _values = new Dictionary<string, LispObject>();

        public string FunctionName { get; }
        public LispStackFrame Parent { get; }
        public int Line { get; private set; }
        public int Column { get; private set; }

        public LispObject T => GetValue<LispSymbol>(TString);
        public LispObject Nil => GetValue<LispList>(NilString);

        public LispRootStackFrame Root => NavigateToRoot().Item1;
        public int Depth => NavigateToRoot().Item2;

        private Tuple<LispRootStackFrame, int> NavigateToRoot()
        {
            int depth = 0;
            var candidate = this;
            while (!(candidate is LispRootStackFrame))
            {
                depth++;
                candidate = candidate.Parent;
            }

            return Tuple.Create((LispRootStackFrame)candidate, depth - 1);
        }

        public LispStackFrame(string functionName, LispStackFrame parent)
        {
            FunctionName = functionName;
            Parent = parent;
        }

        public override string ToString()
        {
            return $"  at {FunctionName}: ({Line}, {Column})\n{Parent}";
        }

        public void SetValue(string name, LispObject value)
        {
            _values[name] = value;
        }

        internal void DeleteValue(string name)
        {
            _values.Remove(name);
        }

        public void SetValueInParentScope(string name, LispObject value)
        {
            if (Parent is object)
            {
                Parent.SetValue(name, value);
            }
            else
            {
                SetValue(name, value);
            }
        }

        public LispObject GetValue(string name)
        {
            if (_values.TryGetValue(name, out var value))
            {
                return value;
            }

            if (Parent is object)
            {
                return Parent.GetValue(name);
            }

            return null;
        }

        public TObject GetValue<TObject>(string name) where TObject : LispObject
        {
            return (TObject)GetValue(name);
        }

        public LispObject Eval(LispObject obj)
        {
            return LispEvaluator.Evaluate(obj, this, true);
        }

        public LispStackFrame Push(string frameName)
        {
            return new LispStackFrame(frameName, this);
        }

        internal LispStackFrame Pop()
        {
            return Parent;
        }

        internal LispStackFrame PopForTailCall()
        {
            return PopForTailCall(Enumerable.Empty<string>());
        }

        internal LispStackFrame PopForTailCall(IEnumerable<string> arguments)
        {
            var argsHash = new HashSet<string>(arguments);

            // copy variables to parent
            foreach (var value in _values)
            {
                if (argsHash.Contains(value.Key))
                {
                    Parent.SetValue(value.Key, value.Value);
                }
            }

            return Parent;
        }

        internal void UpdateCallStackLocation(LispObject obj)
        {
            Line = obj.Line;
            Column = obj.Column;
        }
    }

    public class LispRootStackFrame : LispStackFrame
    {
        public event EventHandler<LispFunctionEnteredEventArgs> FunctionEntered;
        public event EventHandler<LispFunctionReturnedEventArgs> FunctionReturned;
        public event EventHandler<LispFunctionEnteredEventArgs> TraceFunctionEntered;
        public event EventHandler<LispFunctionReturnedEventArgs> TraceFunctionReturned;

        public HashSet<string> TracedFunctions { get; } = new HashSet<string>();

        internal LispRootStackFrame()
            : base("(root)", null)
        {
            SetValue(TString, new LispSymbol(TString));
            SetValue(NilString, LispNilList.Instance);
        }

        internal void OnFunctionEnter(LispStackFrame frame, LispObject[] functionArguments)
        {
            var args = new LispFunctionEnteredEventArgs(frame, functionArguments);
            FunctionEntered?.Invoke(this, args);
            if (TracedFunctions.Contains(frame.FunctionName))
            {
                TraceFunctionEntered?.Invoke(this, args);
            }
        }

        internal void OnFunctionReturn(LispStackFrame frame, LispObject returnValue)
        {
            var args = new LispFunctionReturnedEventArgs(frame, returnValue);
            FunctionReturned?.Invoke(this, args);
            if (TracedFunctions.Contains(frame.FunctionName))
            {
                TraceFunctionReturned?.Invoke(this, args);
            }
        }

        internal LispObject Trace(LispObject[] args)
        {
            if (args.Length == 0)
            {
                return LispList.FromEnumerable(TracedFunctions.Select(f => new LispSymbol(f)));
            }
            else
            {
                var addedFunctions = new HashSet<string>();
                var hasInvalidArguments = false;
                foreach (var arg in args)
                {
                    if (arg is LispSymbol symbol)
                    {
                        TracedFunctions.Add(symbol.Value);
                        addedFunctions.Add(symbol.Value);
                    }
                    else
                    {
                        hasInvalidArguments = true;
                    }
                }

                if (hasInvalidArguments)
                {
                    return new LispError("Expected only symbols");
                }
                else
                {
                    return LispList.FromEnumerable(addedFunctions.Select(f => new LispSymbol(f)));
                }
            }
        }

        internal LispObject Untrace(LispObject[] args)
        {
            if (args.Length == 0)
            {
                var result = LispList.FromEnumerable(TracedFunctions.Select(f => new LispSymbol(f)));
                TracedFunctions.Clear();
                return result;
            }
            else
            {
                var removedFunctions = new HashSet<string>();
                var hasInvalidArguments = false;
                foreach (var arg in args)
                {
                    if (arg is LispSymbol symbol)
                    {
                        if (TracedFunctions.Remove(symbol.Value))
                        {
                            removedFunctions.Add(symbol.Value);
                        }
                    }
                    else
                    {
                        hasInvalidArguments = true;
                    }
                }

                if (hasInvalidArguments)
                {
                    return new LispError("Expected only symbols");
                }
                else
                {
                    return LispList.FromEnumerable(removedFunctions.Select(f => new LispSymbol(f)));
                }
            }
        }
    }
}
