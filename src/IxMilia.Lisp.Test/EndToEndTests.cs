using System.IO;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class EndToEndTests
    {
        private static void EvalFile(string fileName)
        {
            var contents = File.ReadAllText(fileName);
            var host = new LispHost();
            var result = host.Eval(contents);
            Assert.IsType<LispT>(result);
        }

        [Fact]
        public void Runtime()
        {
            EvalFile("runtime-tests.lisp");
        }
    }
}
