namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class BreakpointEvent : Event
    {
        public const string EventName = "breakpoint";

        public BreakpointEventBody Body { get; set; }

        public BreakpointEvent(int seq, BreakpointEventBody body)
            : base(seq, EventName)
        {
            Body = body;
        }
    }

    public enum BreakpointEventReason
    {
        Changed,
        New,
        Removed,
    }

    public class BreakpointEventBody
    {
        public BreakpointEventReason Reason { get; set; }
        public Breakpoint Breakpoint { get; set; }

        public BreakpointEventBody(BreakpointEventReason reason, Breakpoint breakpoint)
        {
            Reason = reason;
            Breakpoint = breakpoint;
        }
    }
}
