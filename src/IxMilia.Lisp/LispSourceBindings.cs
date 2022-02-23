using System.Collections.Generic;
using System.Linq;

namespace IxMilia.Lisp
{
    public class LispSourceBindings
    {
        public LispPackage CurrentPackage { get; }

        private Dictionary<string, LispObject> _parsedBindings = new Dictionary<string, LispObject>();

        internal LispSourceBindings(LispPackage currentPackage)
        {
            CurrentPackage = currentPackage;
        }

        public void TryAddSourceBinding(LispObject obj)
        {
            // first process all parents and the children up to the current...
            if (obj.Parent is object)
            {
                foreach (var sibling in obj.Parent.GetChildren())
                {
                    if (ReferenceEquals(sibling, obj))
                    {
                        break;
                    }

                    TryAddSourceBinding(sibling);
                }
            }

            // ...then check this object specifically
            if (obj is LispList list)
            {
                var listItems = list.ToList();
                if (list.Value is LispSymbol symbol)
                {
                    var resolvedSymbol = symbol.Resolve(CurrentPackage);
                    switch (resolvedSymbol.Value)
                    {
                        case "COMMON-LISP:DEFUN":
                            {
                                if (LispDefaultContext.TryGetCodeFunctionFromItems(listItems.Skip(1).ToArray(), CurrentPackage, out var codeFunction, out var _error))
                                {
                                    if (codeFunction != null)
                                    {
                                        _parsedBindings[codeFunction.NameSymbol.Value] = codeFunction;
                                    }
                                }
                            }
                            break;
                        case "COMMON-LISP:DEFMACRO":
                            {
                                if (LispDefaultContext.TryGetCodeMacroFromItems(listItems.Skip(1).ToArray(), CurrentPackage, out var codeMacro, out var _error))
                                {
                                    if (codeMacro != null)
                                    {
                                        _parsedBindings[codeMacro.NameSymbol.Value] = codeMacro;
                                    }
                                }
                            }
                            break;
                        case "COMMON-LISP:SETF":
                            // starting at 1 to skip the `SETF` keyword
                            for (int i = 1; i < listItems.Count - 1; i++)
                            {
                                var name = listItems[i];
                                var value = listItems[i + 1];
                                if (name is LispSymbol symbolName)
                                {
                                    var resolvedSymbolName = symbolName.Resolve(CurrentPackage);
                                    _parsedBindings[resolvedSymbolName.Value] = value;
                                }
                            }
                            break;
                    }
                }
            }
        }

        public bool TryGetBoundValue(LispResolvedSymbol symbol, out LispObject value)
        {
            if (_parsedBindings.TryGetValue(symbol.Value, out value))
            {
                return true;
            }

            return false;
        }
    }
}
