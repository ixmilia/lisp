using System.IO;
using System.Threading.Tasks;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class EndToEndTests : TestBase
    {
        private string GetFileContents(string fileName)
        {
            var assemblyDirectory = Path.GetDirectoryName(GetType().Assembly.Location);
            var fullFilePath = Path.Combine(assemblyDirectory, fileName);
            var fileContents = File.ReadAllText(fullFilePath);
            return fileContents;
        }

        private async Task EvalFile(string fileName)
        {
            var contents = GetFileContents(fileName);
            var host = await CreateHostAsync();
            var executionState = LispExecutionState.CreateExecutionState(host.RootFrame, allowHalting: false);
            var result = await host.EvalAsync(fileName, contents, executionState);
            Assert.True(result.Value.Equals(host.T), result.Value.ToString());
        }

        [Fact]
        public async Task Runtime()
        {
            await EvalFile("runtime-tests.lisp");
        }
    }
}
