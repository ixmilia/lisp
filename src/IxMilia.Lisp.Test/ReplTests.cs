﻿using System.IO;
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
            Assert.Null(result.LastValue);
            Assert.Equal(2, result.ExpressionDepth);

            result = repl.Eval(")");
            Assert.Null(result.LastValue);
            Assert.Equal(1, result.ExpressionDepth);

            result = repl.Eval(")");
            Assert.Equal(new LispInteger(10), result.LastValue);
            Assert.Equal(0, result.ExpressionDepth);
        }

        [Fact]
        public void CompleteAndIncompleteSubmission()
        {
            var repl = new LispRepl();
            var result = repl.Eval("(+ 1 2)(+ 5 6");
            Assert.Equal(new LispInteger(3), result.LastValue);
            Assert.Equal(1, result.ExpressionDepth);

            result = repl.Eval(")");
            Assert.Equal(new LispInteger(11), result.LastValue);
            Assert.Equal(0, result.ExpressionDepth);
        }

        [Fact]
        public void ReplErrorsArePropagated()
        {
            var repl = new LispRepl();
            var result = repl.Eval("(+ 1 abcd)(+ 2 3)");
            var error = (LispError)result.LastValue;
            Assert.Equal("Symbol 'abcd' not found", error.Message);
            Assert.Equal(1, error.SourceLocation?.Line);
            Assert.Equal(6, error.SourceLocation?.Column);
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
            var actual = NormalizeNewlines(traceWriter.ToString().Trim());
            var expected = NormalizeNewlines(@"
0: (average 3 7)
 1: (half 3)
 1: returned 1.5
 1: (half 7)
 1: returned 3.5
0: returned 5
".Trim());
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

> (dribble)
".Trim());
                Assert.Equal(expectedLogContents, logContents);
            }
        }
    }
}
