namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class EvalTextDocumentParams
    {
        public TextDocumentIdentifier TextDocument { get; set; }
        public Range Range { get; set; }

        public EvalTextDocumentParams(TextDocumentIdentifier textDocument, Range range = null)
        {
            TextDocument = textDocument;
            Range = range;
        }
    }
}
