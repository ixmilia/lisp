using Microsoft.DotNet.Interactive;

namespace IxMilia.Lisp.Interactive
{
    public static class LispKernelExtension
    {
        public static void Load(Kernel kernel)
        {
            if (kernel is CompositeKernel composite)
            {
                composite.Add(new LispKernel());
            }
        }
    }
}
