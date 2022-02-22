namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class WorkspaceFolder
    {
        public string Uri { get; set; }
        public string Name { get; set; }

        public WorkspaceFolder(string uri, string name)
        {
            Uri = uri;
            Name = name;
        }
    }
}
