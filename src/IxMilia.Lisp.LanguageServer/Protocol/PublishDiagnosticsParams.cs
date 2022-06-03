namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class PublishDiagnosticsParams
    {
        public string Uri { get; set; }
        public Diagnostic[] Diagnostics { get; set; }

        public PublishDiagnosticsParams(string uri, Diagnostic[] diagnostics)
        {
            Uri = uri;
            Diagnostics = diagnostics;
        }
    }
}
