using Xunit;

namespace IxMilia.Lisp.Test
{
    public class EvalTests
    {
        private static readonly LispHost host = new LispHost();

        private static LispObject Eval(string code)
        {
            return host.Eval(code);
        }

        [Fact]
        public void SingleItem()
        {
            Assert.Equal(new LispAtom("a"), Eval("a"));
            Assert.Equal(new LispNumber(3.0), Eval("3.0"));
            Assert.Equal(new LispString("a"), Eval("\"a\""));
            Assert.Equal(new LispList(new LispAtom("a"), new LispNumber(3.0)), Eval("'(a 3.0)"));
        }
    }
}
