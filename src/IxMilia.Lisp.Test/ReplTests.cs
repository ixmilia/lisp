using Xunit;

namespace IxMilia.Lisp.Test
{
    public class ReplTests
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
            Assert.Equal(new LispNumber(10.0), result.LastValue);
            Assert.Equal(0, result.ExpressionDepth);
        }

        [Fact]
        public void CompleteAndIncompleteSubmission()
        {
            var repl = new LispRepl();
            var result = repl.Eval("(+ 1 2)(+ 5 6");
            Assert.Equal(new LispNumber(3.0), result.LastValue);
            Assert.Equal(1, result.ExpressionDepth);

            result = repl.Eval(")");
            Assert.Equal(new LispNumber(11.0), result.LastValue);
            Assert.Equal(0, result.ExpressionDepth);
        }
    }
}
