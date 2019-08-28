using Xunit;

namespace IxMilia.Lisp.Test
{
    public class EvalTests
    {
        [Fact]
        public void SingleItem()
        {
            var host = new LispHost();
            Assert.Equal(new LispNumber(3.0), host.Eval("3.0"));
            Assert.Equal(new LispString("a"), host.Eval("\"a\""));
            Assert.Equal(new LispList(new LispString("a"), new LispNumber(3.0)), host.Eval("'(\"a\" 3.0)"));
        }

        [Fact]
        public void ExternalFunction()
        {
            var host = new LispHost();
            host.AddFunction("+", (h, args) => (LispNumber)h.Eval(args[0]) + (LispNumber)h.Eval(args[1]));
            Assert.Equal(new LispNumber(3.0), host.Eval("(+ 1 2)"));
        }

        [Fact]
        public void Variables()
        {
            var host = new LispHost();
            Assert.Equal(new LispNumber(3.0), host.Eval("(setq x 3) x"));
        }

        [Fact]
        public void Functions()
        {
            var host = new LispHost();
            host.AddFunction("+", (h, args) => (LispNumber)h.Eval(args[0]) + (LispNumber)h.Eval(args[1]));
            var code = @"
(defun inc (x)
    (+ x 1))
(inc 2)
";
            Assert.Equal(new LispNumber(3.0), host.Eval(code));
        }
    }
}
