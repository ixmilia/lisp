namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class ContinueRequest : Request
    {
        public const string CommandName = "continue";

        public ContinueRequestArguments Arguments { get; set; }

        public ContinueRequest(int seq, ContinueRequestArguments arguments)
            : base(seq, CommandName)
        {
            Arguments = arguments;
        }
    }

    public class ContinueRequestArguments
    {
        public int ThreadId { get; set; }

        public ContinueRequestArguments(int threadId)
        {
            ThreadId = threadId;
        }
    }
}
