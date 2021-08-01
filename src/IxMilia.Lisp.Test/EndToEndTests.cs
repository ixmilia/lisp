using System.IO;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class EndToEndTests
    {
        private string GetFileContents(string fileName)
        {
            var assemblyDirectory = Path.GetDirectoryName(GetType().Assembly.Location);
            var fullFilePath = Path.Combine(assemblyDirectory, fileName);
            var fileContents = File.ReadAllText(fullFilePath);
            return fileContents;
        }

        private void EvalFile(string fileName)
        {
            var contents = GetFileContents(fileName);
            var host = new LispHost();
            var result = host.Eval(contents);
            Assert.True(result.Equals(host.T), result.ToString());
        }

        [Fact]
        public void Runtime()
        {
            EvalFile("runtime-tests.lisp");
        }
    }
}
