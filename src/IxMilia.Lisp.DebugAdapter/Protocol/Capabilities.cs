namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class Capabilities
    {
        public bool SupportsConfigurationDoneRequest => true;
        public bool SupportsFunctionBreakpoints => true;

        public Capabilities()
        {
        }
    }
}
