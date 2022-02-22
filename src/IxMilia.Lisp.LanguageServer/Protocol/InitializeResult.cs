namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class InitializeResult
    {
        public ServerCapabilities Capabilities { get; set; }

        public InitializeResult(TextDocumentSyncKind syncKind)
        {
            Capabilities = new ServerCapabilities(syncKind);
        }
    }
}
