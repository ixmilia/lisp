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

        [Fact]
        public void ErrorPropagation()
        {
            var host = new LispHost();
            var result = (LispError)host.Eval(@"
(defun inc (x)
    (add x 1))
(inc 2)
");
            Assert.Equal(3, result.StackFrame.Line); // inc: (add x 1)
            Assert.Equal(6, result.StackFrame.Column);
            Assert.Equal("inc", result.StackFrame.FunctionName);
            Assert.Equal(4, result.StackFrame.Parent.Line); // <root>: (inc 2)
            Assert.Equal(2, result.StackFrame.Parent.Column);
            Assert.Equal("<root>", result.StackFrame.Parent.FunctionName);
            Assert.Null(result.StackFrame.Parent.Parent);
            Assert.Equal("Undefined function 'add'", result.Message);
        }

        [Fact]
        public void Conditional()
        {
            var host = new LispHost();

            // 'true' branch
            var result = (LispString)host.Eval(@"
(if (< 1 2)
    (""one"")
    (""two""))");
            Assert.Equal("one", result.Value);

            // 'false' branch
            result = (LispString)host.Eval(@"
(if (< 2 1)
    (""one"")
    (""two""))");
            Assert.Equal("two", result.Value);
        }
    }
}
