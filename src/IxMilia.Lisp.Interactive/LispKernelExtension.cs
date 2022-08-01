using System.Threading.Tasks;
using Microsoft.DotNet.Interactive;

namespace IxMilia.Lisp.Interactive
{
    public class LispKernelExtension : IKernelExtension
    {
        public Task OnLoadAsync(Kernel kernel)
        {
            if (kernel is CompositeKernel composite)
            {
                composite.Add(new LispKernel());
            }

            return Task.CompletedTask;
        }
    }
}
