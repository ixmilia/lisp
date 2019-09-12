using System.Collections.Generic;

namespace IxMilia.Lisp
{
    public class LispStackFrame
    {
        private Dictionary<string, LispObject> _macroExpansions = new Dictionary<string, LispObject>();
        private Dictionary<string, LispObject> _values = new Dictionary<string, LispObject>();

        public string FunctionName { get; }
        public LispStackFrame Parent { get; }
        public int Line { get; internal set; }
        public int Column { get; internal set; }

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

        public LispObject GetValue(LispHost host, string name)
        {
            if (_macroExpansions.TryGetValue(name, out var expansion) && Parent is object)
            {
                return host.EvalAtStackFrame(expansion, Parent);
            }
            else if (_values.TryGetValue(name, out var value))
            {
                return value;
            }
            else if (Parent is object)
            {
                return Parent.GetValue(host, name);
            }
            else
            {
                return null;
            }
        }

        internal void SetMacroExpansion(string name, LispObject expansion)
        {
            _macroExpansions.Add(name, expansion);
        }
    }
}
