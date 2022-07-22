namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class ScopesRequest : Request
    {
        public const string CommandName = "scopes";

        public ScopesArguments Arguments { get; set; }

        public ScopesRequest(int seq, ScopesArguments arguments)
            : base(seq, CommandName)
        {
            Arguments = arguments;
        }
    }

    public class ScopesArguments
    {
        public int FrameId { get; set; }

        public ScopesArguments(int frameId)
        {
            FrameId = frameId;
        }
    }
}
