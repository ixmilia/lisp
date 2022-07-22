namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class LaunchResponse : Response
    {
        public LaunchResponse(int seq, int requestSeq)
            : base(seq, requestSeq, true, LaunchRequest.CommandName, null)
        {
        }
    }
}
