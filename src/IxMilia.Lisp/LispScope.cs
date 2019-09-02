using System.Collections.Generic;

namespace IxMilia.Lisp
{
    internal class LispScope
    {
        private Dictionary<string, LispObject> _values = new Dictionary<string, LispObject>();
        private LispHost _host;
        public LispScope Parent { get; }

        public LispScope(LispHost host, LispScope parent = null)
        {
            _host = host;
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
                    return _host.Nil;
                }
            }
            set => _values[key] = value;
        }
    }
}
