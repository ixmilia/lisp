using System.IO;
using System.Threading.Tasks;
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

        private async Task EvalFile(string fileName)
        {
            var contents = GetFileContents(fileName);
            var host = await LispHost.CreateAsync();
            var result = await host.EvalAsync(contents);
            Assert.True(result.LastResult.Equals(host.T), result.LastResult.ToString());
        }

        [Fact]
        public async Task Runtime()
        {
            await EvalFile("runtime-tests.lisp");
        }
    }
}
