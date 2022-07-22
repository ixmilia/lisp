namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class ThreadsResponse : Response
    {
        public ThreadsResponseBody Body { get; set; }

        public ThreadsResponse(int seq, int requestSeq, ThreadsResponseBody body)
            : base(seq, requestSeq, true, ThreadsRequest.CommandName, null)
        {
            Body = body;
        }
    }

    public class ThreadsResponseBody
    {
        public Thread[] Threads { get; set; }

        public ThreadsResponseBody(Thread[] threads)
        {
            Threads = threads;
        }
    }

    public class Thread
    {
        public int Id { get; set; }
        public string Name { get; set; }

        public Thread(int id, string name)
        {
            Id = id;
            Name = name;
        }
    }
}
