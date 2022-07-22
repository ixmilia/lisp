namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class SetExceptionBreakpointsResponse : Response
    {
        public Breakpoint[] Breakpoints { get; set; }

        public SetExceptionBreakpointsResponse(int seq, int requestSeq, Breakpoint[] breakpoints)
            : base(seq, requestSeq, true, SetExceptionBreakpointsRequest.CommandName, null)
        {
            Breakpoints = breakpoints;
        }
    }
}
