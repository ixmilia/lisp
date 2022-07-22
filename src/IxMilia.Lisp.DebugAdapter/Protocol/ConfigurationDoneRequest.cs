namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class ConfigurationDoneRequest : Request
    {
        public const string CommandName = "configurationDone";

        public ConfigurationDoneRequest(int seq)
            : base(seq, CommandName)
        {
        }
    }
}
