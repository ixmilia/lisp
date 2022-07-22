using System;
using System.Threading.Tasks;

namespace IxMilia.Lisp.DebugAdapter
{
    public class DebugAdapterOptions
    {
        public Func<string, Task<string>> ResolveFileContents { get; }
        public Action<string> MessageLogger { get; }

        public DebugAdapterOptions(Func<string, Task<string>> resolveFileContents, Action<string> messageLogger = null)
        {
            ResolveFileContents = resolveFileContents;
            MessageLogger = messageLogger;
        }
    }
}
