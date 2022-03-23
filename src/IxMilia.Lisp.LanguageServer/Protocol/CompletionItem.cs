namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class CompletionItem
    {
        public string Label { get; set; }
        public string Detail { get; set; }
        public MarkupContent Documentation { get; set; }

        public CompletionItem(string label, string detail, MarkupContent documentation)
        {
            Label = label;
            Detail = detail;
            Documentation = documentation;
        }
    }
}
