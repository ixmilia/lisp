namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class ContinueResponse : Response
    {
        public ContinueResponse(int seq, int requestSeq)
            : base(seq, requestSeq, true, ContinueRequest.CommandName, null)
        {
        }
    }
}
