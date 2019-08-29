using System.Collections.Generic;

namespace IxMilia.Lisp
{
    internal class LispScope
    {
        private Dictionary<string, LispObject> _values = new Dictionary<string, LispObject>();
        public LispScope Parent { get; }

        public LispScope(LispScope parent = null)
        {
            Parent = parent;
        }

        public LispObject this[string key]
        {
            get
            {
                if (_values.TryGetValue(key, out var result))
                {
                    return result;
                }
                else if (Parent != null)
                {
                    return Parent[key];
                }
                else
                {
                    return LispObject.Nil;
                }
            }
            set => _values[key] = value;
        }
    }
}
