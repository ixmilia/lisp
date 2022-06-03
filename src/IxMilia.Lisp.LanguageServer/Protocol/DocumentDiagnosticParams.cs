namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class DocumentDiagnosticParams
    {
        public TextDocumentIdentifier TextDocument { get; set; }

        public DocumentDiagnosticParams(TextDocumentIdentifier textDocument)
        {
            TextDocument = textDocument;
        }
    }
}
