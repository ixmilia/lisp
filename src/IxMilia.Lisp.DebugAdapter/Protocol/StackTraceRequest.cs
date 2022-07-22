namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class StackTraceRequest : Request
    {
        public const string CommandName = "stackTrace";

        public StackTraceArguments Arguments { get; set; }

        public StackTraceRequest(int seq, StackTraceArguments arguments)
            : base(seq, CommandName)
        {
            Arguments = arguments;
        }
    }

    public class StackTraceArguments
    {
        public int ThreadId { get; set; }

        public StackTraceArguments(int threadId)
        {
            ThreadId = threadId;
        }
    }
}
