namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class SourceRequest : Request
    {
        public const string CommandName = "source";

        public SourceArguments Arguments { get; set; }

        public SourceRequest(int seq, SourceArguments arguments)
            : base(seq, CommandName)
        {
            Arguments = arguments;
        }
    }

    public class SourceArguments
    {
        public Source Source { get; set; }
        public int SourceReference { get; set; }
    }
}
