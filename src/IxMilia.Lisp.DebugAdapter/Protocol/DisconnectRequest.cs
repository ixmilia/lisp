namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class DisconnectRequest : Request
    {
        internal const string CommandName = "disconnect";

        public DisconnectRequestArguments Arguments { get; set; }

        public DisconnectRequest(int seq, DisconnectRequestArguments arguments)
            : base(seq, CommandName)
        {
            Arguments = arguments;
        }
    }

    public class DisconnectRequestArguments
    {
        public bool Restart { get; set; }

        public DisconnectRequestArguments(bool restart)
        {
            Restart = restart;
        }
    }
}
