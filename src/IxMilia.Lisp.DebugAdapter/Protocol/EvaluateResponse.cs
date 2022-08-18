namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class EvaluateResponse : Response
    {
        public EvaluateResponseBody Body { get; }

        public EvaluateResponse(int seq, int requestSeq, EvaluateResponseBody body)
            : base(seq, requestSeq, true, EvaluateRequest.CommandName, null)
        {
            Body = body;
        }
    }

    public class EvaluateResponseBody
    {
        public string Result { get; }
        public int VariablesReference { get; }

        public EvaluateResponseBody(string result, int variablesReference)
        {
            Result = result;
            VariablesReference = variablesReference;
        }
    }
}
