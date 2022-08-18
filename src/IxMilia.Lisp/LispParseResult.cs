using System;
using System.Collections.Generic;
using System.Linq;

namespace IxMilia.Lisp
{
    public class LispParseResult
    {
        private LispHost _host;

        public LispBoundValues VisibleValues { get; }
        public LispObject Object { get; }

        public LispParseResult(LispHost host, LispObject obj, LispBoundValues visibleValues)
        {
            _host = host;
            Object = obj;
            VisibleValues = visibleValues;
        }

        public string GetMarkdownDisplay()
        {
            var baseObject = Object;
            if (Object is LispSymbol symbol)
            {
                var resolvedSymbol = symbol.Resolve(_host.CurrentPackage);
                if (VisibleValues.TryGetBoundValue(resolvedSymbol, out var boundValue))
                {
                    baseObject = boundValue;
                }
                else
                {
                    baseObject = _host.GetValue(resolvedSymbol.Value);
                }
            }

            return baseObject.GetMarkdownDisplay(_host);
        }

        public IEnumerable<T> GetReducedCompletionItems<T>(LispPackage currentPackage, Func<LispResolvedSymbol, LispObject, T> symbolItemCreator, Func<string, T> packageItemCreator)
        {
            if (Object is LispString)
            {
                return Enumerable.Empty<T>();
            }

            var (accessiblePackageNames, values) = Object is LispResolvedSymbol resolved
                ? (new HashSet<string>(new[] { resolved.PackageName }), VisibleValues.Values.Where(v => v.Symbol.PackageName == resolved.PackageName))
                : (new HashSet<string>(currentPackage.GetAllAccessiblePackageNames().Distinct()), VisibleValues.Values);
            var accessibleValues = values.Where(v => accessiblePackageNames.Contains(v.Symbol.PackageName)).Select(pair => (pair.Symbol.ToDisplayString(currentPackage), symbolItemCreator(pair.Symbol, pair.Value)));
            var otherVisiblePackages = values.Where(v => !accessiblePackageNames.Contains(v.Symbol.PackageName)).GroupBy(v => v.Symbol.PackageName).Select(group => (group.Key, packageItemCreator(group.Key)));
            var ordered = accessibleValues.Concat(otherVisiblePackages).OrderBy(p => p.Item1);
            var result = ordered.Select(p => p.Item2);
            return result;
        }
    }
}
