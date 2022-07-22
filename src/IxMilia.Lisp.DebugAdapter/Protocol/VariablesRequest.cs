namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class VariablesRequest : Request
    {
        public const string CommandName = "variables";

        public VariablesRequestArguments Arguments { get; set; }

        public VariablesRequest(int seq, VariablesRequestArguments arguments)
            : base(seq, CommandName)
        {
            Arguments = arguments;
        }
    }

    public class VariablesRequestArguments
    {
        public int VariablesReference { get; set; }

        public VariablesRequestArguments(int variablesReference)
        {
            VariablesReference = variablesReference;
        }
    }
}
