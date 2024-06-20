using System.Diagnostics;
using System.IO;
using System.Text;
using System.Threading.Tasks;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class TimingTests : TestBase
    {
        [Fact]
        public async Task ReadCodeAsync()
        {
            var assemblyDirectory = Path.GetDirectoryName(GetType().Assembly.Location);
            var fullFilePath = Path.Combine(assemblyDirectory, "runtime-tests.lisp");
            var fileContents = File.ReadAllText(fullFilePath);
            var code = "(progn\n" + fileContents + "\n)\n";
            var results = new StringBuilder();
            foreach (var readerType in new[] { LispReaderType.Compiled, LispReaderType.NoReaderMacros })
            {
                var host = await CreateHostAsync();
                host.SetReaderFunction(readerType);

                for (int i = 0; i < 2; i++)
                {
                    var executionState = host.CreateExecutionState();
                    var reader = new StringReader(code);
                    var stream = new LispTextStream("TEST-INPUT", reader, TextWriter.Null);
                    var obj = LispList.FromItems(new LispUnresolvedSymbol("READ"), stream);
                    var sw = new Stopwatch();
                    sw.Start();
                    var evalResult = await host.EvalAsync(obj, executionState);
                    sw.Stop();
                    EnsureNotError(evalResult.Value);

                    if (i == 1)
                    {
                        results.AppendLine($"{readerType}: {sw.ElapsedMilliseconds}ms");
                    }
                }
            }

            // uncomment to see timings
            //Assert.Fail(results.ToString());
        }
    }
}
