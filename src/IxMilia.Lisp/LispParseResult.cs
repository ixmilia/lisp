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
    }
}
