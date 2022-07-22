namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class SetFunctionBreakpointsResponse : Response
    {
        public Breakpoint[] Breakpoints { get; set; }

        public SetFunctionBreakpointsResponse(int seq, int requestSeq, Breakpoint[] breakpoints)
            : base(seq, requestSeq, true, SetFunctionBreakpointsRequest.CommandName, null)
        {
            Breakpoints = breakpoints;
        }
    }
}
