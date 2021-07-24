using System;
using System.Diagnostics;
using System.IO;
using System.Text;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class EvalTests : TestBase
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
            Assert.Equal(new LispRatio(5, 4), host.Eval("(+ (/ 1 4) 1)"));
            Assert.Equal(new LispRatio(5, 4), host.Eval("(+ 1 (/ 1 4))"));
        }

        [Fact]
        public void MixedNumericComparisons()
        {
            var host = new LispHost();
            Assert.Equal(host.T, host.Eval("(< 1 2.0)"));
            Assert.Equal(host.T, host.Eval("(< 1.0 2)"));
            Assert.Equal(host.T, host.Eval("(< 1 (/ 5 4))"));
            Assert.Equal(host.T, host.Eval("(< 1.0 (/ 5 4))"));
            Assert.Equal(host.T, host.Eval("(< (/ 3 4) 1)"));
            Assert.Equal(host.T, host.Eval("(< (/ 3 4) 1.0)"));
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
            Assert.Equal("(root)", result.StackFrame.FunctionName);
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
            var result = host.Eval("#1=(3 4 5 . #1#)");
            var list = (LispList)result;
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
            var lastInterpreterStackDepth = 0;
            var lastNativeStackDepth = 0;
            var invocationCount = 0;
            var maxInvocationCount = 10;
            host.AddFunction("record-stack-depth", (frame, args) =>
            {
                if (invocationCount++ > maxInvocationCount)
                {
                    throw new Exception($"Executed more than {maxInvocationCount} times; probably not going to tailcall");
                }

                var currentInterpreterStackDepth = frame.Depth;
                var currentNativeStackDepth = new StackTrace().FrameCount;
                if (currentInterpreterStackDepth == lastInterpreterStackDepth &&
                    currentNativeStackDepth == lastNativeStackDepth)
                {
                    // done
                    return frame.T;
                }
                else
                {
                    // haven't reached a stable stack depth; keep going
                    lastInterpreterStackDepth = currentInterpreterStackDepth;
                    lastNativeStackDepth = currentNativeStackDepth;
                    return frame.Nil;
                }
            });
            var result = host.Eval(@"
(defun do-lots-of-tail-calls-with-cond ()
    (cond ((record-stack-depth) t)                                      ; done
          (t                    (do-lots-of-tail-calls-with-cond))))    ; keep going
(do-lots-of-tail-calls-with-cond)
");
            Assert.True(invocationCount >= 2, "Must have been invoked at least twice");
            Assert.Equal(host.T, result);
        }

        [Fact]
        public void TailCallWithIf()
        {
            var host = new LispHost();
            var lastInterpreterStackDepth = 0;
            var lastNativeStackDepth = 0;
            var invocationCount = 0;
            var maxInvocationCount = 10;
            host.AddFunction("record-stack-depth", (frame, args) =>
            {
                if (invocationCount++ > 10)
                {
                    throw new Exception($"Executed more than {maxInvocationCount} times; probably not going to tailcall");
                }

                var currentInterpreterStackDepth = frame.Depth;
                var currentNativeStackDepth = new StackTrace().FrameCount;
                if (currentInterpreterStackDepth == lastInterpreterStackDepth &&
                    currentNativeStackDepth == lastNativeStackDepth)
                {
                    // done
                    return frame.T;
                }
                else
                {
                    // haven't reached a stable stack depth; keep going
                    lastInterpreterStackDepth = currentInterpreterStackDepth;
                    lastNativeStackDepth = currentNativeStackDepth;
                    return frame.Nil;
                }
            });
            var result = host.Eval(@"
(defun do-lots-of-tail-calls-with-if ()
    (if (record-stack-depth)
        t                                   ; done
        (do-lots-of-tail-calls-with-if)))   ; keep going
(do-lots-of-tail-calls-with-if)
");
            Assert.True(invocationCount >= 2, "Must have been invoked at least twice");
            Assert.Equal(host.T, result);
        }

        [Fact]
        public void InvokeBuiltInNamedFunctionReference()
        {
            var host = new LispHost();
            var result = host.Eval(@"(funcall #'cons 'a 'b)");
            var expected = LispList.FromItemsImproper(new LispSymbol("a"), new LispSymbol("b"));
            Assert.Equal(expected, result);
        }

        [Fact]
        public void InvokeUserDefinedNamedFunctionReference()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(defun add (a b)
    (+ a b))
(funcall #'add 2 3)
");
            Assert.Equal(new LispInteger(5), result);
        }

        [Fact]
        public void InvokeNamedFunctionFromSymbol()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(setf plus-function #'+)
(funcall plus-function 2 3)
");
            Assert.Equal(new LispInteger(5), result);
        }

        [Fact]
        public void InvokeLambdaFromReference()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(funcall #'(lambda (n) (+ 1 n)) 2)
");
            Assert.Equal(new LispInteger(3), result);
        }

        [Fact]
        public void InvokeLambdaFromSymbol()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(setf inc-function #'(lambda (n) (+ 1 n)))
(funcall inc-function 2)
");
            Assert.Equal(new LispInteger(3), result);
        }

        [Fact]
        public void EnterAndReturnFunctionEvent()
        {
            var host = new LispHost();
            var sb = new StringBuilder();
            host.RootFrame.FunctionEntered += (sender, e) => sb.AppendLine($"entered {e.Frame.FunctionName}");
            host.RootFrame.FunctionReturned += (sender, e) => sb.AppendLine($"returned from {e.Frame.FunctionName}");
            host.Eval(@"
(defun half (n) (* n 0.5))
(defun average (x y)
    (+ (half x) (half y)))
");
            host.Eval("(average 3 7)");
            var actual = NormalizeNewlines(sb.ToString().Trim());
            var expected = NormalizeNewlines(@"
entered average
entered half
entered *
returned from *
returned from half
entered half
entered *
returned from *
returned from half
entered +
returned from +
returned from average
".Trim());
            Assert.Equal(expected, actual);
        }

        [Fact]
        public void TraceEnterAndReturnFunctionEvent()
        {
            var host = new LispHost();
            var sb = new StringBuilder();
            host.RootFrame.TraceFunctionEntered += (sender, args) => sb.AppendLine($"entered {args.Frame.FunctionName}");
            host.RootFrame.TraceFunctionReturned += (sender, e) => sb.AppendLine($"returned from {e.Frame.FunctionName}");
            host.Eval(@"
(defun half (n) (* n 0.5))
(defun average (x y)
    (+ (half x) (half y)))
(trace half average)
");
            host.Eval("(average 3 7)");
            var actual = NormalizeNewlines(sb.ToString().Trim());
            var expected = NormalizeNewlines(@"
entered average
entered half
returned from half
entered half
returned from half
returned from average
".Trim());
            Assert.Equal(expected, actual);
        }

        [Fact]
        public void LambdaWithLexicalClosure()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(setf words '((one uno) (two dos) (three tres)))
(defun my-assoc (key table)
  (find-if #'(lambda (entry)
    (equal key (first entry)))
  table))
(my-assoc 'two words)
");
            var expected = LispList.FromItems(new LispSymbol("two"), new LispSymbol("dos"));
            Assert.Equal(expected, result);
        }

        [Fact]
        public void LambdaCapture()
        {
            var host = new LispHost();
            host.Eval(@"
(defun make-greater-p (n)
    #'(lambda (x) (> x n)))
(setf pred (make-greater-p 3))
");
            Assert.Equal(host.Nil, host.Eval("(funcall pred 2)"));
            Assert.Equal(host.T, host.Eval("(funcall pred 5)"));
            Assert.Equal(new LispInteger(4), host.Eval("(find-if pred '(2 3 4 5 6 7 8 9))"));
        }

        [Fact]
        public void LabelsFunctionDefinition()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(labels ((increment-by-one (n)
             (+ n 1))
         (increment-by-two (n)
             (increment-by-one (increment-by-one n)))
        )
    (+ (increment-by-one 1) (increment-by-two 4)) ; (1 + 1) + (4 + 2) = 8
)
");
            Assert.Equal(new LispInteger(8), result);

            // ensure nothing leaked
            Assert.Null(host.GetValue("increment-by-one"));
            Assert.Null(host.GetValue("increment-by-two"));
            Assert.Null(host.GetValue("n"));
        }

        [Fact]
        public void FormatOutput()
        {
            var output = new StringWriter();
            var host = new LispHost(output: output);
            host.Eval(@"(format t ""hello"")");
            var result = NormalizeNewlines(output.ToString());
            Assert.Equal("hello", result);
        }

        [Fact]
        public void FormatOutputWithArgument()
        {
            var output = new StringWriter();
            var host = new LispHost(output: output);
            host.Eval(@"(format t ""hello ~S"" ""world"")");
            var result = NormalizeNewlines(output.ToString());
            Assert.Equal("hello \"world\"", result);
        }

        [Fact]
        public void MultipleCallsToFormat()
        {
            var output = new StringWriter();
            var host = new LispHost(output: output);
            host.Eval(@"
(format t ""1"")
(format t ""2"")
(format t ""3"")
");
            var result = NormalizeNewlines(output.ToString());
            Assert.Equal("123", result);
        }

        [Fact]
        public void FormatToStream()
        {
            var output = new StringWriter();
            var testStream = new LispTerminalStream(TextReader.Null, output);
            var host = new LispHost();
            host.SetValue("test-stream", testStream);
            var result = host.Eval(@"
(format test-stream ""~S~%"" ""just a string"")
(format test-stream ""~S~%"" '(+ 2 3))
");
            Assert.IsNotType<LispError>(result);
            var actual = NormalizeNewlines(output.ToString());
            Assert.Equal("\"just a string\"\r\n(+ 2 3)\r\n", actual);
        }
    }
}
