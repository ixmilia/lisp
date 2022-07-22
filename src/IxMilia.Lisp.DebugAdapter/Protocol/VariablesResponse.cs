namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class VariablesResponse : Response
    {
        public VariablesResponseBody Body { get; set; }

        public VariablesResponse(int seq, int requestSeq, VariablesResponseBody body)
            : base(seq, requestSeq, true, VariablesRequest.CommandName, null)
        {
            Body = body;
        }
    }

    public class VariablesResponseBody
    {
        public Variable[] Variables { get; set; }

        public VariablesResponseBody(Variable[] variables)
        {
            Variables = variables;
        }
    }

    public class Variable
    {
        public string Name { get; set; }
        public string Value { get; set; }
        public int VariablesReference { get; set; }

        public Variable(string name, string value, int variablesReference)
        {
            Name = name;
            Value = value;
            VariablesReference = variablesReference;
        }
    }
}
