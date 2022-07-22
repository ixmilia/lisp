namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class InitializeResponse : Response
    {
        public Capabilities Body { get; } = new Capabilities();

        public InitializeResponse(int seq, int requestSeq)
            : base(seq, requestSeq, true, InitializeRequest.CommandName, null)
        {
        }
    }
}
