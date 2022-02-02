using System.IO;
using System.Text.RegularExpressions;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class ReplTests : TestBase
    {
        [Fact]
        public void Simple()
        {
            var repl = new LispRepl();
            var result = repl.Eval("(+ (+ 1 2) (+ 3 4");
            Assert.Null(result.LastResult);
            Assert.Equal(2, result.ExpressionDepth);

            result = repl.Eval(")");
            Assert.Null(result.LastResult);
            Assert.Equal(1, result.ExpressionDepth);

            result = repl.Eval(")");
            Assert.Equal(new LispInteger(10), result.LastResult);
            Assert.Equal(0, result.ExpressionDepth);
        }

        [Fact]
        public void CompleteAndIncompleteSubmission()
        {
            var repl = new LispRepl();
            var result = repl.Eval("(+ 1 2)(+ 5 6");
            Assert.Equal(new LispInteger(3), result.LastResult);
            Assert.Equal(1, result.ExpressionDepth);

            result = repl.Eval(")");
            Assert.Equal(new LispInteger(11), result.LastResult);
            Assert.Equal(0, result.ExpressionDepth);
        }

        [Fact]
        public void ReplErrorsArePropagated()
        {
            var repl = new LispRepl();
            var result = repl.Eval("(+ 1 abcd)(+ 2 3)");
            var error = (LispError)result.LastResult;
            Assert.Equal("Symbol 'ABCD' not found", error.Message);
            Assert.Equal(1, error.SourceLocation?.Start.Line);
            Assert.Equal(6, error.SourceLocation?.Start.Column);
        }

        [Fact]
        public void FunctionTracing()
        {
            var traceWriter = new StringWriter();
            var repl = new LispRepl(traceWriter: traceWriter);
            repl.Eval(@"
(defun half (n) (* n 0.5))
(defun average (x y)
    (+ (half x) (half y)))
(trace half average)
");
            repl.Eval("(average 3 7)");
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
        public void DribbleLogging()
        {
            var output = new StringWriter();
            var repl = new LispRepl(output: output);
            using (var tempFile = new TemporaryFile(createFile: false))
            {
                var result = repl.Eval($@"
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

        [Fact]
        public void GetObjectAtLocationGetsNarrowestObject()
        {
            var markedCode = @"
(+ 1 2)
(+ (* 12 3$$4) (/ 56 78))
(- 1 2)
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var repl = new LispRepl();
            var child = repl.GetObjectAtLocation(code, position);
            Assert.IsType<LispInteger>(child);
            Assert.Equal(34, ((LispInteger)child).Value);
        }

        [Fact]
        public void GetObjectAtLocationGetsParentObjectIfNoNarrowChildExists()
        {
            var markedCode = @"
(+ 1 2)
(+ (* 12 34) $$ (/ 56 78))
(- 1 2)
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var repl = new LispRepl();
            var child = repl.GetObjectAtLocation(code, position);
            Assert.IsType<LispList>(child);
            Assert.Equal("(+ (* 12 34) (/ 56 78))", child.ToString());
        }

        [Fact]
        public void GetObjectAtLocationReturnsNullIfNothingIsAvailable()
        {
            var markedCode = @"
(+ 1 2)
$$
(- 1 2)
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var repl = new LispRepl();
            var child = repl.GetObjectAtLocation(code, position);
            Assert.Null(child);
        }

        [Fact]
        public void GetMarkdownDisplayFromFunctionSourceObject()
        {
            var markedCode = @"
(add-2-num$$bers 3 5)
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var repl = new LispRepl();
            repl.Eval("(defun add-2-numbers (a b) \"Adds 2 numbers.\" (+ a b))");
            var obj = repl.GetObjectAtLocation(code, position);
            var markdown = repl.GetMarkdownDisplayFromSourceObject(obj);
            Assert.Equal("`(DEFUN ADD-2-NUMBERS (A B) ...)`\n\nAdds 2 numbers.", markdown);
        }

        [Fact]
        public void GetMarkdownDisplayFromNumberSourceObject()
        {
            var markedCode = @"
3.14$$159
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var repl = new LispRepl();
            var obj = repl.GetObjectAtLocation(code, position);
            var markdown = repl.GetMarkdownDisplayFromSourceObject(obj);
            Assert.Equal("3.14159", markdown);
        }
    }
}
