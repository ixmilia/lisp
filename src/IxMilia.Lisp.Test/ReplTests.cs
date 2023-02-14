using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class ReplTests : TestBase
    {
        [Fact]
        public async Task Simple()
        {
            var repl = await LispRepl.CreateAsync();
            var result = await repl.EvalAsync("(+ (+ 1 2) (+ 3 4");
            Assert.Null(result.LastResult);
            Assert.Equal(2, result.ExpressionDepth);

            result = await repl.EvalAsync(")");
            Assert.Null(result.LastResult);
            Assert.Equal(1, result.ExpressionDepth);

            result = await repl.EvalAsync(")");
            Assert.Equal(new LispInteger(10), result.LastResult);
            Assert.Equal(0, result.ExpressionDepth);
        }

        [Fact]
        public async Task CompleteAndIncompleteSubmission()
        {
            var repl = await LispRepl.CreateAsync();
            var result = await repl.EvalAsync("(+ 1 2)(+ 5 6");
            Assert.Equal(new LispInteger(3), result.LastResult);
            Assert.Equal(1, result.ExpressionDepth);

            result = await repl.EvalAsync(")");
            Assert.Equal(new LispInteger(11), result.LastResult);
            Assert.Equal(0, result.ExpressionDepth);
        }

        [Fact]
        public async Task ReplErrorsArePropagated()
        {
            var repl = await LispRepl.CreateAsync();
            var result = await repl.EvalAsync("(+ 1 abcd)(+ 2 3)");
            var error = (LispError)result.LastResult;
            Assert.Equal("Symbol 'ABCD' not found", error.Message);
            Assert.Equal(1, error.SourceLocation?.Start.Line);
            Assert.Equal(6, error.SourceLocation?.Start.Column);
        }

        [Fact]
        public async Task FunctionTracing()
        {
            var traceWriter = new StringWriter();
            var repl = await LispRepl.CreateAsync(traceWriter: traceWriter);
            await repl.EvalAsync(@"
(defun half (n) (* n 0.5))
(defun average (x y)
    (+ (half x) (half y)))
(trace half average)
");
            await repl.EvalAsync("(average 3 7)");
            var actual = NormalizeNewlines(traceWriter.ToString().Trim('\r', '\n'));
            var expected = NormalizeNewlines(@"
0: (AVERAGE 3 7)
 1: (HALF 3)
 1: returned 1.5
 1: (HALF 7)
 1: returned 3.5
0: returned 5
".Trim('\r', '\n'));
            Assert.Equal(expected, actual);
        }

        [Fact]
        public async Task DribbleLogging()
        {
            var output = new StringWriter();
            var repl = await LispRepl.CreateAsync(output: output);
            using (var tempFile = new TemporaryFile(createFile: false))
            {
                var result = await repl.EvalAsync($@"
(+ 1 1) ; this shouldn't be in the log
(dribble ""{tempFile.FilePath.Replace("\\", "\\\\")}"") ; start recording
(+ 3 3) ; this should be in the log
(dribble) ; stop recording
(+ 5 5) ; this shouldn't be in the log
");
                var consoleOutput = NormalizeNewlines(output.ToString()).Trim();
                var expectedConsoleOutput = NormalizeNewlines($@"
Now recording in file {tempFile.FilePath}
Finished recording in file {tempFile.FilePath}
").Trim();
                Assert.Equal(expectedConsoleOutput, consoleOutput);

                var logContents = NormalizeNewlines(File.ReadAllText(tempFile.FilePath).Trim());
                // trim non-deterministic time stamp
                logContents = Regex.Replace(logContents, ";Recording started at .*$", ";Recording started at <TIME-STAMP>", RegexOptions.Multiline);
                var expectedLogContents = NormalizeNewlines($@"
;Recording in {tempFile.FilePath}
;Recording started at <TIME-STAMP>

> (+ 3 3)
6

> (DRIBBLE)
".Trim());
                Assert.Equal(expectedLogContents, logContents);
            }
        }
    }
}
