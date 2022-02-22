namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class MarkupContent
    {
        public MarkupKind Kind { get; set; }
        public string Value { get; set; }

        public MarkupContent(MarkupKind kind, string value)
        {
            Kind = kind;
            Value = value;
        }
    }
}
