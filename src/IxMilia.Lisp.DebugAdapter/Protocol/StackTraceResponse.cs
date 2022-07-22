namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class StackTraceResponse : Response
    {
        public StackTraceResponseBody Body { get; set; }

        public StackTraceResponse(int seq, int requestSeq, StackTraceResponseBody body)
            : base(seq, requestSeq, true, StackTraceRequest.CommandName, null)
        {
            Body = body;
        }
    }

    public class StackTraceResponseBody
    {
        public StackFrame[] StackFrames { get; set; }

        public StackTraceResponseBody(StackFrame[] stackFrames)
        {
            StackFrames = stackFrames;
        }
    }

    public class StackFrame
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public Source Source { get; set; }
        public int Line { get; set; }
        public int Column { get; set; }

        public StackFrame(int id, string name, Source source, int line, int column)
        {
            Id = id;
            Name = name;
            Source = source;
            Line = line;
            Column = column;
        }
    }
}
