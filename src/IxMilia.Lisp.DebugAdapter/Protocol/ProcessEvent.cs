namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class ProcessEvent : Event
    {
        public const string EventName = "process";

        public ProcessEventBody Body { get; set; }

        public ProcessEvent(int seq, ProcessEventBody body)
            : base(seq, EventName)
        {
            Body = body;
        }
    }

    public class ProcessEventBody
    {
        public string Name { get; set; }

        public ProcessEventBody(string name)
        {
            Name = name;
        }
    }
}
