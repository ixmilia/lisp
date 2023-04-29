using System.IO;
using System.Threading.Tasks;
using IxMilia.Lisp.Repl;
using Xunit;

namespace IxMilia.Lisp.Test
{
    [CollectionDefinition("Live repl tests", DisableParallelization = true)]
    public class ReplConsoleTests : TestBase
    {
        [Fact(Timeout = 3000, Skip = "Needs a lot of rework")]
        public async Task ReplConsoleBreakEvaluateAndContinue()
        {
            var input = new StringReader(@"
; evaluate code with a `break`
(progn
    (setf one 1)
    (format t ""~&about to break"")
    (break ""~&one = ~S"" one)
    (format t ""~&let's go~%""))

; we're in the debugger here; evaluate something
(+ one 3)
continue
#quit
");
            var output = new StringWriter();
            var error = new StringWriter();
            var replConsole = new ReplConsole("*test*", input, output, error);
            await replConsole.RunAsync();
            var expectedOutput = NormalizeNewlines(@"
_> _> _> (_> (_> (_> (_> 
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

        [Fact(Timeout = 3000, Skip = "Needs a lot of work")]
        public async Task NoBreakOnFatalError()
        {
            var input = new StringReader(@"
; evaluate code with an error
(+ 1 asdf)
#quit
");
            var output = new StringWriter();
            var error = new StringWriter();
            var replConsole = new ReplConsole("*test*", input, output, error);
            await replConsole.RunAsync();
            var expectedOutput = NormalizeNewlines(@"
_> _> _> Symbol 'ASDF' not found:
  at (ROOT) in '*test*': (2, 6)

_> 
".Trim('\r', '\n'));
            var actualOutput = NormalizeNewlines(output.ToString());
            Assert.Empty(error.ToString());
            Assert.Equal(expectedOutput, actualOutput);
        }
    }
}
