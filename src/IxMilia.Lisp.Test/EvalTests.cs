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
        }

        [Fact]
        public void ExternalFunction()
        {
            var host = new LispHost();
            host.AddFunction("add", (h, args) => (LispNumber)args[0] + (LispNumber)args[1]);
            Assert.Equal(new LispNumber(3.0), host.Eval("(add 1 2)"));
        }

        [Fact]
        public void Quoted()
        {
            var host = new LispHost();
            Assert.Equal(new LispSymbol("a"), host.Eval("'a"));
            Assert.Equal(new LispQuotedObject(new LispSymbol("a")), host.Eval("''a"));
            Assert.Equal(LispList.FromItems(new LispNumber(1.0)), host.Eval("'(1)"));
            Assert.Equal("'a", host.Eval("(eval '''a)").ToString());
        }

        [Fact]
        public void Variables()
        {
            var host = new LispHost();
            Assert.Equal(new LispNumber(3.0), host.Eval("(setq x 3) x"));
        }

        [Fact]
        public void Macros()
        {
            var host = new LispHost();
            var code = @"
(defmacro if2 (pred tv fv)
    (cond (pred tv)
          (t fv)))
(if2 (= 1 1) ""one"" ""two"")
";
            var result = host.Eval(code);
            Assert.Equal("one", ((LispString)result).Value);
        }

        [Fact]
        public void Functions()
        {
            var host = new LispHost();
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
            Assert.Equal("Undefined macro/function 'add'", result.Message);
        }

        [Fact]
        public void Conditional()
        {
            var host = new LispHost();

            // 'true' branch
            var result = (LispString)host.Eval(@"
(if (< 1 2)
    ""one""
    ""two"")");
            Assert.Equal("one", result.Value);

            // 'false' branch
            result = (LispString)host.Eval(@"
(if (< 2 1)
    ""one""
    ""two"")");
            Assert.Equal("two", result.Value);
        }

        [Fact]
        public void CircularLists()
        {
            var host = new LispHost();
            var list = (LispList)host.Eval("#1=(3 4 5 . #1#)");
            Assert.False(list.IsProperList);
            Assert.Equal(-3, list.Length); // not dictated anywhere, simply convention
            Assert.Equal(new LispNumber(3), list.Value);
            Assert.Equal("#1=(3 4 5 . #1#)", list.ToString());

            list = (LispList)host.Eval("#1=(#1# . 2)");
            Assert.False(list.IsProperList);
            Assert.Equal(-1, list.Length);
            Assert.Equal(new LispNumber(2), list.Next);
            Assert.True(ReferenceEquals(list, list.Value));
            Assert.Equal("#1=(#1# . 2)", list.ToString());

            list = (LispList)host.Eval("#1=(2 3 #1#)");
            Assert.True(list.IsProperList);
            Assert.Equal(-3, list.Length);
            Assert.Equal("#1=(2 3 #1#)", list.ToString());
        }

        [Fact]
        public void LetTest()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(defun avg (x y)
    (let ((sum (+ x y)))
        (/ sum 2)))
(avg 3 7)
");
            Assert.Equal(new LispNumber(5.0), result);
        }
    }
}
