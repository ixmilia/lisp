namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class Capabilities
    {
        public bool SupportsConfigurationDoneRequest => true;
        public bool SupportsFunctionBreakpoints => true;
        public bool SupportsEvaluateForHovers => true;
        public ExceptionBreakpointsFilter[] ExceptionBreakpointFilters { get; }

        public Capabilities()
        {
            ExceptionBreakpointFilters = new ExceptionBreakpointsFilter[]
            {
                new ExceptionBreakpointsFilter()
            };
        }
    }
}
