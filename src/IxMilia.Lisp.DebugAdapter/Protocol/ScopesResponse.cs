namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class ScopesResponse : Response
    {
        public ScopesResponseBody Body { get; set; }

        public ScopesResponse(int seq, int requestSeq, ScopesResponseBody body)
            : base(seq, requestSeq, true, ScopesRequest.CommandName, null)
        {
            Body = body;
        }
    }

    public class ScopesResponseBody
    {
        public Scope[] Scopes { get; set; }

        public ScopesResponseBody(Scope[] scopes)
        {
            Scopes = scopes;
        }
    }

    public class Scope
    {
        public string Name { get; set; }
        public int VariablesReference { get; set; }
        public bool Expensive { get; set; }

        public Scope(string name, int variablesReference, bool expensive)
        {
            Name = name;
            VariablesReference = variablesReference;
            Expensive = expensive;
        }
    }
}
