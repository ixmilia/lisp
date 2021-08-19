using System.IO;
using IxMilia.Lisp.Repl;
using Xunit;

namespace IxMilia.Lisp.Test
{
    [CollectionDefinition("Live repl tests", DisableParallelization = true)]
    public class ReplConsoleTests : TestBase
    {
        [Fact(Timeout = 3000)]
        public void ReplConsoleBreakEvaluateAndContinue()
        {
            var input = new StringReader(@"
; submit a function with a `break`
(defun f ()
    (setf one 1)
    (format t ""~&about to break"")
    (break ""~&one = ~S"" one)
    (format t ""~&let's go~%""))
(f)

; we're in the debugger here; evaluate something
(+ one 3)
continue
#quit
");
            var output = new StringWriter();
            var error = new StringWriter();
            var replConsole = new ReplConsole("*test*", input, output, error);
            replConsole.Run();
            var expectedOutput = NormalizeNewlines(@"
_> _> _> (_> (_> (_> (_> ()
_> 
about to break
one = 1
Non-fatal break.  Type 'continue' to resume evaluation.
DEBUG:> DEBUG:> DEBUG:> 4
DEBUG:> 
let's go
_> 
".Trim('\r', '\n'));
            var actualOutput = NormalizeNewlines(output.ToString());
            Assert.Empty(error.ToString());
            Assert.Equal(expectedOutput, actualOutput);
        }
    }
}
