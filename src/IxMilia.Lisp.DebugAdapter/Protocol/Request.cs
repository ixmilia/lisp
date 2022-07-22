namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public abstract class Request : ProtocolMessage
    {
        public const string RequestType = "request";

        public string Command { get; set; }

        public Request(int seq, string command)
            : base(seq, ProtocolMessageType.Request)
        {
            Command = command;
        }
    }
}
