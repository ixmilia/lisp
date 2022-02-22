namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class HoverParams : TextDocumentPositionParams
    {
        public HoverParams(TextDocumentIdentifier textDocument, Position position)
            : base(textDocument, position)
        {
        }
    }
}
