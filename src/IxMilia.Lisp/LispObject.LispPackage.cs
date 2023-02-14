using System.Collections.Generic;

namespace IxMilia.Lisp
{
    public class LispPackage : LispObject
    {
        private Dictionary<string, LispObject> _values = new Dictionary<string, LispObject>();
        private List<LispPackage> _inheritedPackages = new List<LispPackage>();
        public string Name { get; }

        internal IEnumerable<KeyValuePair<string, LispObject>> Values => _values;

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
            if (value is null)
            {
                _values.Remove(name);
            }
            else
            {
                _values[name] = value;
            }
        }

        public LispResolvedSymbol ResolveSymbol(LispUnresolvedSymbol symbol)
        {
            var owningPackage = GetNarrowestOwningPackage(symbol.LocalName);
            var resolvedSymbol = new LispResolvedSymbol(owningPackage.Name, symbol.LocalName, isPublic: true); // TODO: fix calculation of `isPublic`

            resolvedSymbol.Parent = symbol.Parent;
            resolvedSymbol.SourceLocation = symbol.SourceLocation;
            return resolvedSymbol;
        }

        internal IEnumerable<string> GetAllAccessiblePackageNames()
        {
            yield return Name;
            foreach (var package in _inheritedPackages)
            {
                foreach (var name in package.GetAllAccessiblePackageNames())
                {
                    yield return name;
                }
            }
        }

        internal bool IsSymbolWithNameInScope(string name)
        {
            foreach (var package in _inheritedPackages)
            {
                if (package.IsSymbolWithNameInScope(name))
                {
                    return true;
                }
            }

            return _values.ContainsKey(name);
        }

        internal LispPackage GetNarrowestOwningPackage(string localName)
        {
            // if it's defined here, report it
            if (_values.ContainsKey(localName))
            {
                return this;
            }

            // otherwise look as deep as possible
            var enqueuedPackageNames = new HashSet<string>();
            var packagesToCheck = new Queue<LispPackage>();
            AddPackageToCheck(this);
            while (packagesToCheck.Count > 0)
            {
                var packageToCheck = packagesToCheck.Dequeue();
                if (packageToCheck._values.ContainsKey(localName))
                {
                    return packageToCheck;
                }

                AddPackagesToCheck(packageToCheck._inheritedPackages);
            }

            // didn't find anything deeply nested, so it must be us
            return this;

            void AddPackageToCheck(LispPackage package)
            {
                if (enqueuedPackageNames.Add(package.Name))
                {
                    packagesToCheck.Enqueue(package);
                }
            }
            void AddPackagesToCheck(IEnumerable<LispPackage> packages)
            {
                foreach (var package in packages)
                {
                    AddPackageToCheck(package);
                }
            }
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

        public IEnumerable<(LispResolvedSymbol, LispObject)> GetValues()
        {
            foreach (var pair in _values)
            {
                yield return (new LispResolvedSymbol(Name, pair.Key, isPublic: true), pair.Value);
            }
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

            foreach (var inheritedPackage in _inheritedPackages)
            {
                clone._inheritedPackages.Add(inheritedPackage);
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
