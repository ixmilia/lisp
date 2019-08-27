using Xunit;

namespace IxMilia.Lisp.Test
{
    public class EvalTests
    {
        [Fact]
        public void SingleItem()
        {
            var host = new LispHost();
            Assert.Equal(new LispAtom("a"), host.Eval("a"));
            Assert.Equal(new LispNumber(3.0), host.Eval("3.0"));
            Assert.Equal(new LispString("a"), host.Eval("\"a\""));
            Assert.Equal(new LispList(new LispAtom("a"), new LispNumber(3.0)), host.Eval("'(a 3.0)"));
        }

        [Fact]
        public void ExternalFunction()
        {
            var host = new LispHost();
            host.AddFunction("+", args => (LispNumber)args[0] + (LispNumber)args[1]);
            Assert.Equal(new LispNumber(3.0), host.Eval("(+ 1 2)"));
        }
    }
}
