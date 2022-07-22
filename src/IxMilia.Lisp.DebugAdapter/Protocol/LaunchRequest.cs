namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class LaunchRequest : Request
    {
        internal const string CommandName = "launch";

        public LaunchRequestCommandArguments Arguments { get; set; }

        public LaunchRequest(int seq, LaunchRequestCommandArguments arguments)
            : base(seq, CommandName)
        {
            Arguments = arguments;
        }
    }

    public class LaunchRequestCommandArguments
    {
        public string Program { get; set; }

        public LaunchRequestCommandArguments(string program)
        {
            Program = program;
        }
    }
}
