namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class TextDocumentItem
    {
        public string Uri { get; set; }
        public string LanguageId { get; set; }
        public int Version { get; set; }
        public string Text { get; set; }

        public TextDocumentItem(string uri, string languageId, int version, string text)
        {
            Uri = uri;
            LanguageId = languageId;
            Version = version;
            Text = text;
        }
    }
}
