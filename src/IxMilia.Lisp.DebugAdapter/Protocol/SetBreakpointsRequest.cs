namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class SetBreakpointsRequest : Request
    {
        public const string CommandName = "setBreakpoints";

        public SetBreakpointsRequestArguments Arguments { get; set; }

        public SetBreakpointsRequest(int seq, SetBreakpointsRequestArguments arguments)
            : base(seq, CommandName)
        {
            Arguments = arguments;
        }
    }

    public class SetBreakpointsRequestArguments
    {
        public Source Source { get; set; }
        public SourceBreakpoint[] Breakpoints { get; set; }

        public SetBreakpointsRequestArguments(Source source, SourceBreakpoint[] breakpoints)
        {
            Source = source;
            Breakpoints = breakpoints;
        }
    }

    public class Source
    {
        public string Path { get; set; }

        public Source(string path)
        {
            Path = path;
        }
    }

    public class SourceBreakpoint
    {
        public int Line { get; set; }
    }
}
