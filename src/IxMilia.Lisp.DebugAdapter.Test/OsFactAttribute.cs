using System.Runtime.InteropServices;
using Xunit;

namespace IxMilia.Lisp.DebugAdapter.Test
{
    public class OsFactAttribute : FactAttribute
    {
        public OsFactAttribute(bool skipLinux)
        {
            if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux) && skipLinux)
            {
                Skip = "Test is skipped on Linux";
            }
        }
    }
}
