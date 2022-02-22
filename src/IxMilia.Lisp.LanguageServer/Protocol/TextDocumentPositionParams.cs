namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class TextDocumentPositionParams
    {
        public TextDocumentIdentifier TextDocument { get; set; }
        public Position Position { get; set; }

        public TextDocumentPositionParams(TextDocumentIdentifier textDocument, Position position)
        {
            TextDocument = textDocument;
            Position = position;
        }
    }
}
