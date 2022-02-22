namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class DidOpenTextDocumentParams
    {
        public TextDocumentItem TextDocument { get; set; }

        public DidOpenTextDocumentParams(TextDocumentItem textDocument)
        {
            TextDocument = textDocument;
        }
    }
}
