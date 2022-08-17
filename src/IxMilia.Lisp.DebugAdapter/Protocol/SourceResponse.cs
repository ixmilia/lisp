namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class SourceResponse : Response
    {
        public SourceResponseBody Body { get; set; }

        public SourceResponse(int seq, int requestSeq, SourceResponseBody body)
            : base(seq, requestSeq, true, SourceRequest.CommandName, null)
        {
            Body = body;
        }
    }

    public class SourceResponseBody
    {
        public string Content { get; set; }

        public SourceResponseBody(string content)
        {
            Content = content;
        }
    }
}
