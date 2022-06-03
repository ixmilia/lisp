using Newtonsoft.Json;

namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class Diagnostic
    {
        public Range Range { get; set; }

        [JsonConverter(typeof(ForceDefaultConverter))]
        public DiagnosticSeverity Severity { get; set; }
        public string Message { get; set; }

        public Diagnostic(Range range, DiagnosticSeverity severity, string message)
        {
            Range = range;
            Severity = severity;
            Message = message;
        }
    }
}
