namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class EvaluateRequest : Request
    {
        public const string CommandName = "evaluate";

        public EvaluateRequestArguments Arguments { get; set; }

        public EvaluateRequest(int seq, EvaluateRequestArguments arguments)
            : base(seq, CommandName)
        {
            Arguments = arguments;
        }
    }

    public class EvaluateRequestArguments
    {
        public string Expression { get; set; }
        public int? FrameId { get; set; }

        public EvaluateRequestArguments(string expression, int? frameId)
        {
            Expression = expression;
            FrameId = frameId;
        }
    }
}
