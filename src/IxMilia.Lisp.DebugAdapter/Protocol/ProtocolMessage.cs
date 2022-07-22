namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public enum ProtocolMessageType
    {
        Request,
        Response,
        Event,
    }

    public abstract class ProtocolMessage
    {
        public int Seq { get; set; }
        public ProtocolMessageType Type { get; set; }

        protected ProtocolMessage(int seq, ProtocolMessageType type)
        {
            Seq = seq;
            Type = type;
        }
    }
}
