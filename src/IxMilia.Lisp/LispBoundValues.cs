using System.Collections.Generic;
using System.Linq;

namespace IxMilia.Lisp
{
    public class LispBoundValues
    {
        private Dictionary<string, (LispResolvedSymbol Symbol, LispObject Value)> _boundValues = new();

        public IEnumerable<(LispResolvedSymbol Symbol, LispObject Value)> Values => _boundValues.Values;

        internal LispBoundValues()
        {
        }

        public void SetBoundValue(LispResolvedSymbol symbol, LispObject value)
        {
            _boundValues[symbol.Value] = (symbol, value);
        }

        public void TryAddSourceBinding(LispPackage currentPackage, LispObject obj)
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

                    TryAddSourceBinding(currentPackage, sibling);
                }
            }

            // ...then check this object specifically
            if (obj is LispList list)
            {
                var listItems = list.ToList();
                if (list.Value is LispSymbol symbol)
                {
                    var resolvedSymbol = symbol.Resolve(currentPackage);
                    switch (resolvedSymbol.Value)
                    {
                        case "COMMON-LISP:DEFUN":
                            {
                                if (LispDefaultContext.TryGetCodeFunctionFromItems(listItems.Skip(1).ToArray(), currentPackage, out var codeFunction, out var _error))
                                {
                                    if (codeFunction != null)
                                    {
                                        SetBoundValue(codeFunction.NameSymbol, codeFunction);
                                    }
                                }
                            }
                            break;
                        case "COMMON-LISP:DEFMACRO":
                            {
                                if (LispDefaultContext.TryGetCodeMacroFromItems(listItems.Skip(1).ToArray(), currentPackage, out var codeMacro, out var _error))
                                {
                                    if (codeMacro != null)
                                    {
                                        SetBoundValue(codeMacro.NameSymbol, codeMacro);
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
                                    var resolvedSymbolName = symbolName.Resolve(currentPackage);
                                    SetBoundValue(resolvedSymbolName, value);
                                }
                            }
                            break;
                    }
                }
            }
        }

        public bool TryGetBoundValue(LispResolvedSymbol symbol, out LispObject value)
        {
            value = default;
            if (_boundValues.TryGetValue(symbol.Value, out var valuePair))
            {
                value = valuePair.Value;
                return true;
            }

            return false;
        }
    }
}
