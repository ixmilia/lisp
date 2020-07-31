using Xunit;

namespace IxMilia.Lisp.Test
{
    public class EvalTests
    {
        [Fact]
        public void SingleItem()
        {
            var host = new LispHost();
            Assert.Equal(new LispInteger(3), host.Eval("3"));
            Assert.Equal(new LispFloat(3.0), host.Eval("3.0"));
            Assert.Equal(new LispString("a"), host.Eval("\"a\""));
        }

        [Fact]
        public void ExternalFunction()
        {
            var host = new LispHost();
            host.AddFunction("add", (h, args) => (LispInteger)args[0] + (LispInteger)args[1]);
            Assert.Equal(new LispInteger(3), host.Eval("(add 1 2)"));
        }

        [Fact]
        public void Quoted()
        {
            var host = new LispHost();
            Assert.Equal(new LispSymbol("a"), host.Eval("'a"));
            Assert.Equal(new LispQuotedObject(new LispSymbol("a")), host.Eval("''a"));
            Assert.Equal(LispList.FromItems(new LispInteger(1)), host.Eval("'(1)"));
            Assert.Equal("'a", host.Eval("(eval '''a)").ToString());
        }

        [Fact]
        public void Variables()
        {
            var host = new LispHost();
            Assert.Equal(new LispInteger(3), host.Eval("(setq x 3) x"));
        }

        [Fact]
        public void IntegerNumericFolding()
        {
            var host = new LispHost();
            Assert.Equal(new LispInteger(-4), host.Eval("(- 4)"));
            Assert.Equal(new LispInteger(10), host.Eval("(+ 1 2 3 4)"));
            Assert.Equal(new LispInteger(1), host.Eval("(- 10 4 3 2)"));
            Assert.Equal(new LispInteger(24), host.Eval("(* 1 2 3 4)"));
            Assert.Equal(new LispInteger(4), host.Eval("(/ 24 3 2)"));
        }

        [Fact]
        public void FloatNumericFolding()
        {
            var host = new LispHost();
            Assert.Equal(new LispFloat(-4.0), host.Eval("(- 4.0)"));
            Assert.Equal(new LispFloat(10.0), host.Eval("(+ 1.0 2.0 3.0 4.0)"));
            Assert.Equal(new LispFloat(1.0), host.Eval("(- 10.0 4.0 3.0 2.0)"));
            Assert.Equal(new LispFloat(24.0), host.Eval("(* 1.0 2.0 3.0 4.0)"));
            Assert.Equal(new LispFloat(4.0), host.Eval("(/ 24.0 3.0 2.0)"));
        }

        [Fact]
        public void MixedNumericFolding()
        {
            var host = new LispHost();
            Assert.Equal(new LispFloat(10.0), host.Eval("(+ 1.0 2 3 4)"));
            Assert.Equal(new LispFloat(10.0), host.Eval("(+ 1 2.0 3 4)"));
        }

        [Fact]
        public void MixedNumericComparisons()
        {
            var host = new LispHost();
            Assert.Equal(host.T, host.Eval("(< 1 2.0)"));
            Assert.Equal(host.T, host.Eval("(< 1.0 2)"));
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
            Assert.Equal(new LispInteger(3), host.Eval(code));
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
            Assert.Equal(3, result.StackFrame.Line); // inc: (add x 1), but tailcall inlined
            Assert.Equal(6, result.StackFrame.Column);
            Assert.Equal("<root>", result.StackFrame.FunctionName);
            Assert.Null(result.StackFrame.Parent);
            Assert.Equal("Undefined macro/function 'add', found '<null>'", result.Message);
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
            Assert.Equal(new LispInteger(3), list.Value);
            Assert.Equal("#1=(3 4 5 . #1#)", list.ToString());

            list = (LispList)host.Eval("#1=(#1# . 2)");
            Assert.False(list.IsProperList);
            Assert.Equal(-1, list.Length);
            Assert.Equal(new LispInteger(2), list.Next);
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
        (/ sum 2.0)))
(avg 3.0 7.0)
");
            Assert.Equal(new LispFloat(5.0), result);
        }

        [Fact]
        public void MacroFunctionVariableNames()
        {
            // redefined function variable names shadow previous macro expansion values
            var host = new LispHost();
            var result = host.Eval(@"
(defmacro if2 (pred tv fv)
    (cond (pred tv)
          (t fv)))

; argument `pred` shadows macro expansion with the same name
; used to cause stack overflow in evaluating arguments
(defun asrt (pred msg)
    (if pred t msg))
(asrt t ""not hit"")
");
            Assert.Equal(host.T, result);
        }

        [Fact]
        public void TailCallWithCond()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(defun count-to-one-million (val)
    (cond ((= 1000000 val) val)
          (t  (count-to-one-million (+ val 1)))))
(count-to-one-million 0)
");
            Assert.Equal(new LispInteger(1000000), result);
        }

        [Fact]
        public void TailCallWithIf()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(defun count-to-one-million (val)
    (if (= 1000000 val)
        val
        (count-to-one-million (+ val 1))))
(count-to-one-million 0)
");
            Assert.Equal(new LispInteger(1000000), result);
        }
    }
}
