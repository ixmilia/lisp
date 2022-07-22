namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class ThreadsRequest : Request
    {
        public const string CommandName = "threads";

        public ThreadsRequest(int seq)
            : base(seq, CommandName)
        {
        }
    }
}
