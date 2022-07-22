namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class StoppedEvent : Event
    {
        public const string EventName = "stopped";

        public StoppedEventBody Body { get; set; }

        public StoppedEvent(int seq, StoppedEventBody body)
            : base(seq, EventName)
        {
            Body = body;
        }
    }

    public class StoppedEventBody
    {
        public string Reason { get; set; }
        public int ThreadId { get; set; }
        public int[] HitBreakpointIds { get; set; }

        public StoppedEventBody(string reason, int threadId, int[] hitBreakpointIds)
        {
            Reason = reason;
            ThreadId = threadId;
            HitBreakpointIds = hitBreakpointIds;
        }
    }
}
