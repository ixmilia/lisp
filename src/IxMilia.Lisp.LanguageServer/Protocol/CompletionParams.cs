namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class CompletionParams : TextDocumentPositionParams
    {
        public CompletionContext Context { get; set; }

        public CompletionParams(CompletionContext context, TextDocumentIdentifier textDocument, Position position)
            : base(textDocument, position)
        {
            Context = context;
        }
    }
}
