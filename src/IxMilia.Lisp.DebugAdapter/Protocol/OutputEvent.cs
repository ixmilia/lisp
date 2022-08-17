namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class OutputEvent : Event
    {
        public const string EventName = "output";

        public OutputEventBody Body { get; set; }

        public OutputEvent(int seq, OutputEventBody body)
            : base(seq, EventName)
        {
            Body = body;
        }
    }

    public enum OutputEventCategory
    {
        Console,
        Important,
        Stdout,
        Stderr,
        Telemetry,
    }

    public class OutputEventBody
    {
        public OutputEventCategory Category { get; set; }
        public string Output { get; set; }

        public OutputEventBody(OutputEventCategory category, string output)
        {
            Category = category;
            Output = output;
        }
    }
}
