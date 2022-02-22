namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class TextDocumentIdentifier
    {
        public string Uri { get; set; }

        public TextDocumentIdentifier(string uri)
        {
            Uri = uri;
        }
    }
}
