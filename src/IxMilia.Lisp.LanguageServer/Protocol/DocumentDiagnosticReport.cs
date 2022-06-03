namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public abstract class DocumentDiagnosticReport
    {
    }

    public class FullDocumentDiagnosticReport : DocumentDiagnosticReport
    {
        public DocumentDiagnosticReportKind Kind { get; } = DocumentDiagnosticReportKind.Full;
        public Diagnostic[] Items { get; set; }

        public FullDocumentDiagnosticReport(Diagnostic[] items)
        {
            Items = items;
        }
    }
}
