using System.Collections.Generic;

namespace IxMilia.Lisp.DebugAdapter
{
    public class ReaderResult
    {
        public IReadOnlyDictionary<string, string> Headers { get; }
        public byte[] Body { get; }

        public ReaderResult(IReadOnlyDictionary<string, string> headers, byte[] body)
        {
            Headers = headers;
            Body = body;
        }
    }
}
