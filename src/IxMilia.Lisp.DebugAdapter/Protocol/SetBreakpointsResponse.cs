namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class SetBreakpointsResponse : Response
    {
        public Breakpoint[] Breakpoints { get; set; }

        public SetBreakpointsResponse(int seq, int requestSeq, Breakpoint[] breakpoints)
            : base(seq, requestSeq, true, SetBreakpointsRequest.CommandName, null)
        {
            Breakpoints = breakpoints;
        }
    }
}
