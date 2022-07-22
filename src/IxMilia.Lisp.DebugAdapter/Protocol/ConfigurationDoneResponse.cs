namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class ConfigurationDoneResponse : Response
    {
        public ConfigurationDoneResponse(int seq, int requestSeq)
            : base(seq, requestSeq, true, ConfigurationDoneRequest.CommandName, null)
        {
        }
    }
}
