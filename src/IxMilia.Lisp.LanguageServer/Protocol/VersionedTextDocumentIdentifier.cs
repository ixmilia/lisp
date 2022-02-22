namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class VersionedTextDocumentIdentifier : TextDocumentIdentifier
    {
        public int Version { get; set; }

        public VersionedTextDocumentIdentifier(string uri, int version)
            : base(uri)
        {
            Version = version;
        }
    }
}
