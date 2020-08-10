using System.Collections.Generic;
using System.Linq;

namespace IxMilia.Lisp
{
    public class LispStackFrame
    {
        private const string NilString = "nil";
        private const string TString = "t";

        private Dictionary<string, LispObject> _values = new Dictionary<string, LispObject>();

        public string FunctionName { get; }
        public LispStackFrame Parent { get; }
        public int Line { get; private set; }
        public int Column { get; private set; }

        public LispObject T => GetValue<LispSymbol>(TString);
        public LispObject Nil => GetValue<LispList>(NilString);

        public LispStackFrame(string functionName, LispStackFrame parent)
        {
            FunctionName = functionName;
            Parent = parent;
        }

        internal static LispStackFrame CreateRootStackFrame()
        {
            var root = new LispStackFrame("<root>", null);
            root.SetValue(TString, new LispSymbol(TString));
            root.SetValue(NilString, LispNilList.Instance);
            return root;
        }

        public override string ToString()
        {
            return $"  at {FunctionName}: ({Line}, {Column})\n{Parent}";
        }

        public void SetValue(string name, LispObject value)
        {
            _values[name] = value;
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
}
