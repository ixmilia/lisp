namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class DisconnectResponse : Response
    {
        public DisconnectResponse(int seq, int requestSeq)
            : base(seq, requestSeq, true, DisconnectRequest.CommandName, null)
        {
        }
    }
}
