using System.Collections.Generic;

namespace IxMilia.Lisp
{
    public class LispPackage : LispObject
    {
        private Dictionary<string, LispObject> _values = new Dictionary<string, LispObject>();
        private List<LispPackage> _inheritedPackages = new List<LispPackage>();
        public string Name { get; }

        public LispPackage(string name, IEnumerable<LispPackage> inheritedPackages = null)
        {
            Name = name;
            if (inheritedPackages != null)
            {
                _inheritedPackages.AddRange(inheritedPackages);
            }
        }

        public void SetValue(string name, LispObject value)
        {
            _values[name] = value;
        }

        public LispResolvedSymbol ResolveSymbol(LispUnresolvedSymbol symbol)
        {
            // first try to resolve against one of the inherited packages
            LispResolvedSymbol resolvedSymbol = null;
            foreach (var package in _inheritedPackages)
            {
                var inheritedValue = package.GetValue(symbol.LocalName);
                if (inheritedValue != null)
                {
                    // TODO: fix calculation of `isPublic`
                    resolvedSymbol = new LispResolvedSymbol(package.Name, symbol.LocalName, isPublic: true);
                    break;
                }
            }

            // otherwise resolve to this package
            // TODO: fix calculation of `isPublic`
            if (resolvedSymbol == null)
            {
                resolvedSymbol = new LispResolvedSymbol(Name, symbol.LocalName, isPublic: true);
            }

            resolvedSymbol.Parent = symbol.Parent;
            resolvedSymbol.SourceLocation = symbol.SourceLocation;
            return resolvedSymbol;
        }

        public bool HasSymbolWithName(string name)
        {
            foreach (var package in _inheritedPackages)
            {
                if (package.HasSymbolWithName(name))
                {
                    return true;
                }
            }

            return _values.ContainsKey(name);
        }

        public virtual LispObject GetValue(string name)
        {
            LispObject inheritedValue;
            foreach (var package in _inheritedPackages)
            {
                inheritedValue = package.GetValue(name);
                if (inheritedValue != null)
                {
                    return inheritedValue;
                }
            }

            if (_values.TryGetValue(name, out var value))
            {
                return value;
            }

            return null;
        }

        public TObject GetValue<TObject>(string name) where TObject : LispObject
        {
            return (TObject)GetValue(name);
        }

        public void DeleteValue(string name)
        {
            _values.Remove(name);
        }

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        protected override LispObject CloneProtected()
        {
            var clone = new LispPackage(Name);
            foreach (var item in _values)
            {
                clone._values.Add(item.Key, item.Value);
            }

            return clone;
        }

        public override string ToString()
        {
            return $"#<PACKAGE {Name}>";
        }
    }

    public class LispKeywordPackage : LispPackage
    {
        public LispKeywordPackage()
            : base("KEYWORD", null)
        {
        }

        public override LispObject GetValue(string name)
        {
            var symbol = new LispResolvedSymbol(Name, name, isPublic: true);
            SetValue(name, symbol);
            return base.GetValue(name);
        }
    }
}
