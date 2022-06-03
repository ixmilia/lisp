namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class DiagnosticOptions
    {
        public bool InterFileDependencies { get; set; }
        public bool WorkspaceDiagnostics { get; set; }

        public DiagnosticOptions()
        {
            InterFileDependencies = false;
            WorkspaceDiagnostics = false;
        }
    }
}
