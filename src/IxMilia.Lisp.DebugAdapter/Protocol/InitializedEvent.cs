namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class InitializedEvent : Event
    {
        public const string EventName = "initialized";

        public InitializedEvent(int seq)
            : base(seq, EventName)
        {
        }
    }
}
