namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class EvalTextDocumentParams
    {
        public TextDocumentIdentifier TextDocument { get; set; }

        public EvalTextDocumentParams(TextDocumentIdentifier textDocument)
        {
            TextDocument = textDocument;
        }
    }
}
