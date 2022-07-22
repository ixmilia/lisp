namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class TerminatedEvent : Event
    {
        public const string EventName = "terminated";

        public TerminatedEvent(int seq)
            : base(seq, EventName)
        {
        }
    }
}
