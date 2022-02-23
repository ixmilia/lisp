namespace IxMilia.Lisp
{
    public class LispParseResult
    {
        public LispHost Host { get; }
        public LispObject Object { get; }
        public LispSourceBindings Bindings { get; }

        public LispParseResult(LispHost host, LispObject obj, LispSourceBindings bindings)
        {
            Host = host;
            Object = obj;
            Bindings = bindings;
        }

        public string GetMarkdownDisplay()
        {
            var baseObject = Object;
            if (Object is LispSymbol symbol)
            {
                var resolvedSymbol = symbol.Resolve(Host.CurrentPackage);
                if (Bindings.TryGetBoundValue(resolvedSymbol, out var boundValue))
                {
                    baseObject = boundValue;
                }
                else
                {
                    baseObject = Host.GetValue(resolvedSymbol.Value);
                }
            }

            return baseObject.GetMarkdownDisplay(Host);
        }
    }
}
