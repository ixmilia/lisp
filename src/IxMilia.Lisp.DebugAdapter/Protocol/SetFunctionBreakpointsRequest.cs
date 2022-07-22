namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class SetFunctionBreakpointsRequest : Request
    {
        public const string CommandName = "setFunctionBreakpoints";

        public SetFunctionBreakpointsRequestArguments Arguments { get; set; }

        public SetFunctionBreakpointsRequest(int seq, SetFunctionBreakpointsRequestArguments arguments)
            : base(seq, CommandName)
        {
            Arguments = arguments;
        }
    }

    public class SetFunctionBreakpointsRequestArguments
    {
        public FunctionBreakpoint[] Breakpoints { get; set; }

        public SetFunctionBreakpointsRequestArguments(FunctionBreakpoint[] breakpoints)
        {
            Breakpoints = breakpoints;
        }
    }

    public class FunctionBreakpoint
    {
        public string Name { get; set; }

        public FunctionBreakpoint(string name)
        {
            Name = name;
        }
    }
}
