namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class ServerCapabilities
    {
        public TextDocumentSyncOptions TextDocumentSync { get; set; }
        public CompletionOptions CompletionProvider { get; set; }
        public bool HoverProvider { get; set; }
        public SemanticTokensOptions SemanticTokensProvider { get; set; }

        public ServerCapabilities(TextDocumentSyncKind syncKind)
        {
            TextDocumentSync = new TextDocumentSyncOptions(syncKind);
            CompletionProvider = new CompletionOptions();
            HoverProvider = true;
            SemanticTokensProvider = new SemanticTokensOptions();
        }
    }
}
