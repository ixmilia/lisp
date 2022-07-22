namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class SetExceptionBreakpointsRequest : Request
    {
        public const string CommandName = "setExceptionBreakpoints";

        public SetExceptionBreakpointsRequestArguments Arguments { get; set; }

        public SetExceptionBreakpointsRequest(int seq, SetExceptionBreakpointsRequestArguments arguments)
            : base(seq, CommandName)
        {
            Arguments = arguments;
        }
    }

    public class SetExceptionBreakpointsRequestArguments
    {
        public string[] Filters { get; set; }
    }
}
