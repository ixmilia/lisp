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
            Assert.Equal(new LispInteger(3), host.Eval("3").LastResult);
            Assert.Equal(new LispFloat(3.0), host.Eval("3.0").LastResult);
            Assert.Equal(new LispString("a"), host.Eval("\"a\"").LastResult);
        }

        [Fact]
        public void ExternalFunction()
        {
            var host = new LispHost();
            host.AddFunction("add", (h, args) => (LispInteger)args[0] + (LispInteger)args[1]);
            Assert.Equal(new LispInteger(3), host.Eval("(add 1 2)").LastResult);
        }

        [Fact]
        public void Quoted()
        {
            var host = new LispHost();
            Assert.Equal(new LispSymbol("a"), host.Eval("'a").LastResult);
            Assert.Equal(new LispQuotedObject(new LispSymbol("a")), host.Eval("''a").LastResult);
            Assert.Equal(LispList.FromItems(new LispInteger(1)), host.Eval("'(1)").LastResult);
            Assert.Equal("'a", host.Eval("(eval '''a)").LastResult.ToString());
        }

        [Fact]
        public void Variables()
        {
            var host = new LispHost();
            Assert.Equal(new LispInteger(3), host.Eval("(setq x 3) x").LastResult);
        }

        [Fact]
        public void IntegerNumericFolding()
        {
            var host = new LispHost();
            Assert.Equal(new LispInteger(-4), host.Eval("(- 4)").LastResult);
            Assert.Equal(new LispInteger(10), host.Eval("(+ 1 2 3 4)").LastResult);
            Assert.Equal(new LispInteger(1), host.Eval("(- 10 4 3 2)").LastResult);
            Assert.Equal(new LispInteger(24), host.Eval("(* 1 2 3 4)").LastResult);
            Assert.Equal(new LispInteger(4), host.Eval("(/ 24 3 2)").LastResult);
        }

        [Fact]
        public void FloatNumericFolding()
        {
            var host = new LispHost();
            Assert.Equal(new LispFloat(-4.0), host.Eval("(- 4.0)").LastResult);
            Assert.Equal(new LispFloat(10.0), host.Eval("(+ 1.0 2.0 3.0 4.0)").LastResult);
            Assert.Equal(new LispFloat(1.0), host.Eval("(- 10.0 4.0 3.0 2.0)").LastResult);
            Assert.Equal(new LispFloat(24.0), host.Eval("(* 1.0 2.0 3.0 4.0)").LastResult);
            Assert.Equal(new LispFloat(4.0), host.Eval("(/ 24.0 3.0 2.0)").LastResult);
        }

        [Fact]
        public void MixedNumericFolding()
        {
            var host = new LispHost();
            Assert.Equal(new LispFloat(10.0), host.Eval("(+ 1.0 2 3 4)").LastResult);
            Assert.Equal(new LispFloat(10.0), host.Eval("(+ 1 2.0 3 4)").LastResult);
            Assert.Equal(new LispRatio(5, 4), host.Eval("(+ (/ 1 4) 1)").LastResult);
            Assert.Equal(new LispRatio(5, 4), host.Eval("(+ 1 (/ 1 4))").LastResult);
        }

        [Fact]
        public void MixedNumericComparisons()
        {
            var host = new LispHost();
            Assert.Equal(host.T, host.Eval("(< 1 2.0)").LastResult);
            Assert.Equal(host.T, host.Eval("(< 1.0 2)").LastResult);
            Assert.Equal(host.T, host.Eval("(< 1 (/ 5 4))").LastResult);
            Assert.Equal(host.T, host.Eval("(< 1.0 (/ 5 4))").LastResult);
            Assert.Equal(host.T, host.Eval("(< (/ 3 4) 1)").LastResult);
            Assert.Equal(host.T, host.Eval("(< (/ 3 4) 1.0)").LastResult);
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
            var result = host.Eval(code).LastResult;
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
            Assert.Equal(new LispInteger(3), host.Eval(code).LastResult);
        }

        [Fact]
        public void ErrorGeneration()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(error ""Expected '~s' but got '~s'"" 1 2)
").LastResult;
            Assert.Equal("Expected '1' but got '2'", ((LispError)result).Message);
        }

        [Fact]
        public void ErrorPropagationFromCodeFunctionBody()
        {
            var host = new LispHost();
            var executionState = host.Eval("test-file.lisp", @"
(defun inc (x)
    (add x 1))
(inc 2)
");
            var error = (LispError)executionState.LastResult;
            Assert.Equal("test-file.lisp", error.SourceLocation.Value.FilePath);
            Assert.Equal(3, error.SourceLocation.Value.Line);
            Assert.Equal(6, error.SourceLocation.Value.Column);
            Assert.Equal("(root)", error.StackFrame.FunctionName);
            Assert.Null(error.StackFrame.Parent);
            Assert.Equal("Undefined macro/function 'add', found '<null>'", error.Message);
        }

        [Fact]
        public void ErrorPropagationFromCodeFunctionArgument()
        {
            var host = new LispHost();
            var executionState = host.Eval("test-file.lisp", @"
(defun inc (x)
    (add x 1))
(inc two)
");
            var error = (LispError)executionState.LastResult;
            Assert.Equal("test-file.lisp", error.SourceLocation.Value.FilePath);
            Assert.Equal(4, error.SourceLocation.Value.Line);
            Assert.Equal(6, error.SourceLocation.Value.Column);
            Assert.Equal("(root)", error.StackFrame.FunctionName);
            Assert.Null(error.StackFrame.Parent);
            Assert.Equal("Symbol 'two' not found", error.Message);
        }

        [Fact]
        public void ErrorPropagationFromMultipleExpressions()
        {
            var host = new LispHost();
            var executionState = host.Eval("test-file.lisp", @"
(+ 1 1)
(* 2 2)
(+ one 2)
(+ 3 3)
");
            var error = (LispError)executionState.LastResult;
            Assert.Equal("test-file.lisp", error.SourceLocation.Value.FilePath);
            Assert.Equal(4, error.SourceLocation.Value.Line);
            Assert.Equal(4, error.SourceLocation.Value.Column);
            Assert.Equal("(root)", error.StackFrame.FunctionName);
            Assert.Null(error.StackFrame.Parent);
            Assert.Equal("Symbol 'one' not found", error.Message);
        }

        [Fact]
        public void SourcePropagationFromHostInit()
        {
            var host = new LispHost("test-file.lisp");
            var result = host.Eval("(+ 1 2)").LastResult;
            Assert.Equal(3, ((LispInteger)result).Value);
            Assert.Equal("test-file.lisp", result.SourceLocation.Value.FilePath);
        }

        [Fact]
        public void SourcePropagationFromHostEval()
        {
            var host = new LispHost("host.lisp");
            var result = host.Eval("some-other-location.lisp", "(+ 1 2)").LastResult;
            Assert.Equal(3, ((LispInteger)result).Value);
            Assert.Equal("some-other-location.lisp", result.SourceLocation.Value.FilePath);
        }

        [Fact]
        public void Conditional()
        {
            var host = new LispHost();

            // 'true' branch
            var result = (LispString)host.Eval(@"
(if (< 1 2)
    ""one""
    ""two"")").LastResult;
            Assert.Equal("one", result.Value);

            // 'false' branch
            result = (LispString)host.Eval(@"
(if (< 2 1)
    ""one""
    ""two"")").LastResult;
            Assert.Equal("two", result.Value);
        }

        [Fact]
        public void DotNotationLists()
        {
            var host = new LispHost();
            var result = host.Eval("'(a . (b . (c)))").LastResult;
            var actual = result.ToString();
            Assert.Equal("(a b c)", actual);
        }

        [Fact]
        public void CircularLists()
        {
            var host = new LispHost();
            var result = host.Eval("#1=(3 4 5 . #1#)").LastResult;
            var list = (LispList)result;
            Assert.False(list.IsProperList);
            Assert.Equal(-3, list.Length); // not dictated anywhere, simply convention
            Assert.Equal(new LispInteger(3), list.Value);
            Assert.Equal("#1=(3 4 5 . #1#)", list.ToString());

            list = (LispList)host.Eval("#1=(#1# . 2)").LastResult;
            Assert.False(list.IsProperList);
            Assert.Equal(-1, list.Length);
            Assert.Equal(new LispInteger(2), list.Next);
            Assert.True(ReferenceEquals(list, list.Value));
            Assert.Equal("#1=(#1# . 2)", list.ToString());

            list = (LispList)host.Eval("#1=(2 3 #1#)").LastResult;
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
").LastResult;
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
").LastResult;
            Assert.Equal(host.T, result);
        }

        [Fact]
        public void TailCallWithCond()
        {
            var host = new LispHost();
            var lastInterpreterStackDepth = 0;
            var lastDotNetStackDepth = 0;
            var invocationCount = 0;
            var maxInvocationCount = 10;
            host.AddFunction("record-stack-depth", (frame, args) =>
            {
                var currentInterpreterStackDepth = frame.Depth;
                var currentDotNetStackDepth = new StackTrace().FrameCount;

                if (invocationCount++ > maxInvocationCount)
                {
                    throw new Exception($@"Executed more than {maxInvocationCount} times; probably not going to tailcall.
Last/current interpreter stack depth = {lastInterpreterStackDepth}/{currentInterpreterStackDepth}.
Last/current .NET stack depth = {lastDotNetStackDepth}/{currentDotNetStackDepth}");
                }

                if (currentInterpreterStackDepth == lastInterpreterStackDepth &&
                    currentDotNetStackDepth == lastDotNetStackDepth)
                {
                    // done
                    return frame.T;
                }
                else
                {
                    // haven't reached a stable stack depth; keep going
                    lastInterpreterStackDepth = currentInterpreterStackDepth;
                    lastDotNetStackDepth = currentDotNetStackDepth;
                    return frame.Nil;
                }
            });
            var result = host.Eval(@"
(defun do-lots-of-tail-calls-with-cond ()
    (cond ((record-stack-depth) t)                                      ; done
          (t                    (do-lots-of-tail-calls-with-cond))))    ; keep going
(do-lots-of-tail-calls-with-cond)
").LastResult;
            Assert.True(invocationCount >= 2, $"Must have been invoked at least twice, but was only invoked {invocationCount} time(s).");
            Assert.Equal(host.T, result);
        }

        [Fact]
        public void TailCallWithIf()
        {
            var host = new LispHost();
            var lastInterpreterStackDepth = 0;
            var lastDotNetStackDepth = 0;
            var invocationCount = 0;
            var maxInvocationCount = 10;
            host.AddFunction("record-stack-depth", (frame, args) =>
            {
                var currentInterpreterStackDepth = frame.Depth;
                var currentDotNetStackDepth = new StackTrace().FrameCount;

                if (invocationCount++ > maxInvocationCount)
                {
                    throw new Exception($@"Executed more than {maxInvocationCount} times; probably not going to tailcall.
Last/current interpreter stack depth = {lastInterpreterStackDepth}/{currentInterpreterStackDepth}.
Last/current .NET stack depth = {lastDotNetStackDepth}/{currentDotNetStackDepth}");
                }

                if (currentInterpreterStackDepth == lastInterpreterStackDepth &&
                    currentDotNetStackDepth == lastDotNetStackDepth)
                {
                    // done
                    return frame.T;
                }
                else
                {
                    // haven't reached a stable stack depth; keep going
                    lastInterpreterStackDepth = currentInterpreterStackDepth;
                    lastDotNetStackDepth = currentDotNetStackDepth;
                    return frame.Nil;
                }
            });
            var result = host.Eval(@"
(defun do-lots-of-tail-calls-with-if ()
    (if (record-stack-depth)
        t                                   ; done
        (do-lots-of-tail-calls-with-if)))   ; keep going
(do-lots-of-tail-calls-with-if)
").LastResult;
            Assert.True(invocationCount >= 2, $"Must have been invoked at least twice, but was only invoked {invocationCount} time(s).");
            Assert.Equal(host.T, result);
        }

        [Fact]
        public void InvokeBuiltInNamedFunctionReference()
        {
            var host = new LispHost();
            var result = host.Eval(@"(funcall #'cons 'a 'b)").LastResult;
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
").LastResult;
            Assert.Equal(new LispInteger(5), result);
        }

        [Fact]
        public void InvokeNamedFunctionFromSymbol()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(setf plus-function #'+)
(funcall plus-function 2 3)
").LastResult;
            Assert.Equal(new LispInteger(5), result);
        }

        [Fact]
        public void InvokeLambdaFromReference()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(funcall #'(lambda (n) (+ 1 n)) 2)
").LastResult;
            Assert.Equal(new LispInteger(3), result);
        }

        [Fact]
        public void InvokeLambdaFromSymbol()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(setf inc-function #'(lambda (n) (+ 1 n)))
(funcall inc-function 2)
").LastResult;
            Assert.Equal(new LispInteger(3), result);
        }

        [Fact]
        public void ApplyFunctionReference()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(apply #'+ '(2 3))
").LastResult;
            Assert.Equal(5, ((LispInteger)result).Value);
        }

        [Fact]
        public void EnterAndReturnFunctionEvent()
        {
            var host = new LispHost();
            var sb = new StringBuilder();
            host.Eval(@"
(defun half (n) (* n 0.5))
(defun average (x y)
    (+ (half x) (half y)))
");
            host.RootFrame.FunctionEntered += (sender, e) => sb.AppendLine($"entered {e.Frame.FunctionName}");
            host.RootFrame.FunctionReturned += (sender, e) => sb.AppendLine($"returned from {e.InvocationObject.Name} with {e.ReturnValue}");
            host.Eval("(average 3 7)");
            var actual = NormalizeNewlines(sb.ToString().Trim());
            var expected = NormalizeNewlines(@"
entered average
entered half
entered *
entered cons
returned from cons with (1 3 0.5)
entered reduce
entered kernel:two-arg-*
returned from kernel:two-arg-* with 3
entered kernel:two-arg-*
returned from kernel:two-arg-* with 1.5
returned from reduce with 1.5
returned from * with 1.5
returned from half with 1.5
entered half
entered *
entered cons
returned from cons with (1 7 0.5)
entered reduce
entered kernel:two-arg-*
returned from kernel:two-arg-* with 7
entered kernel:two-arg-*
returned from kernel:two-arg-* with 3.5
returned from reduce with 3.5
returned from * with 3.5
returned from half with 3.5
entered +
entered cons
returned from cons with (0 1.5 3.5)
entered reduce
entered kernel:two-arg-+
returned from kernel:two-arg-+ with 1.5
entered kernel:two-arg-+
returned from kernel:two-arg-+ with 5
returned from reduce with 5
returned from + with 5
returned from average with 5
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
").LastResult;
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
            Assert.Equal(host.Nil, host.Eval("(funcall pred 2)").LastResult);
            Assert.Equal(host.T, host.Eval("(funcall pred 5)").LastResult);
            Assert.Equal(new LispInteger(4), host.Eval("(find-if pred '(2 3 4 5 6 7 8 9))").LastResult);
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
").LastResult;
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
            var testStream = new LispStream("test-stream", TextReader.Null, output);
            var host = new LispHost();
            host.SetValue("test-stream", testStream);
            var result = host.Eval(@"
(format test-stream ""~S~%"" ""just a string"")
(format test-stream ""~S~%"" '(+ 2 3))
");
            Assert.IsNotType<LispError>(result);
            var actual = NormalizeNewlines(output.ToString());
            Assert.Equal("\"just a string\"\n(+ 2 3)\n", actual);
        }

        [Fact]
        public void TerPriFunction()
        {
            var output = new StringWriter();
            var host = new LispHost(output: output);
            var result = host.Eval(@"
(terpri)
").LastResult;
            Assert.True(result.IsNil());
            Assert.Equal("\n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public void TerPriFunctionWithStream()
        {
            var output = new StringWriter();
            var testStream = new LispStream("test-stream", TextReader.Null, output);
            var host = new LispHost();
            host.SetValue("test-stream", testStream);
            var result = host.Eval(@"
(terpri test-stream)
").LastResult;
            Assert.True(result.IsNil());
            Assert.Equal("\n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public void Prin1Function()
        {
            var output = new StringWriter();
            var host = new LispHost(output: output);
            var result = host.Eval(@"
(prin1 ""abc"")
").LastResult;
            Assert.Equal("abc", ((LispString)result).Value);
            Assert.Equal("\"abc\"\n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public void Prin1FunctionWithStream()
        {
            var output = new StringWriter();
            var testStream = new LispStream("test-stream", TextReader.Null, output);
            var host = new LispHost();
            host.SetValue("test-stream", testStream);
            var result = host.Eval(@"
(prin1 ""abc"" test-stream)
").LastResult;
            Assert.Equal("abc", ((LispString)result).Value);
            Assert.Equal("\"abc\"\n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public void PrinCFunction()
        {
            var output = new StringWriter();
            var host = new LispHost(output: output);
            var result = host.Eval(@"
(princ ""abc"")
").LastResult;
            Assert.Equal("abc", ((LispString)result).Value);
            Assert.Equal("abc\n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public void PrinCFunctionWithStream()
        {
            var output = new StringWriter();
            var testStream = new LispStream("test-stream", TextReader.Null, output);
            var host = new LispHost();
            host.SetValue("test-stream", testStream);
            var result = host.Eval(@"
(princ ""abc"" test-stream)
").LastResult;
            Assert.Equal("abc", ((LispString)result).Value);
            Assert.Equal("abc\n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public void PrintFunction()
        {
            var output = new StringWriter();
            var host = new LispHost(output: output);
            var result = host.Eval(@"
(print ""abc"")
").LastResult;
            Assert.Equal("abc", ((LispString)result).Value);
            Assert.Equal("\n\"abc\"\n \n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public void PrintFunctionWithStream()
        {
            var output = new StringWriter();
            var testStream = new LispStream("test-stream", TextReader.Null, output);
            var host = new LispHost();
            host.SetValue("test-stream", testStream);
            var result = host.Eval(@"
(print ""abc"" test-stream)
").LastResult;
            Assert.Equal("abc", ((LispString)result).Value);
            Assert.Equal("\n\"abc\"\n \n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public void LetBlocksEvaluateManyStatements()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(let ((x 1))
    x
    (+ x 2)) ; this is the real result
").LastResult;
            Assert.Equal(3, ((LispInteger)result).Value);
        }

        [Fact]
        public void BindRestArgumentsInFunction()
        {
            var host = new LispHost();
            host.Eval(@"
(defun test (a b &rest the-rest)
    the-rest)
(setf result-a (test 1 2)
      result-b (test 1 2 3))
");
            var resultA = host.GetValue<LispList>("result-a");
            var resultB = host.GetValue<LispList>("result-b");
            Assert.True(resultA.IsNil());
            Assert.Equal(1, resultB.Length);
            Assert.Equal(3, ((LispInteger)resultB.Value).Value);
        }

        [Fact]
        public void BindOptionalArgumentsInFunction()
        {
            var host = new LispHost();
            host.Eval(@"
(defun test (a &optional b (c 14))
    (format nil ""~a:~a:~a"" a b c))
(setf result-a (test 11)
      result-b (test 22 33)
      result-c (test 44 55 66))
");
            var resultA = host.GetValue<LispString>("result-a");
            var resultB = host.GetValue<LispString>("result-b");
            var resultC = host.GetValue<LispString>("result-c");
            Assert.Equal("11:():14", resultA.Value);
            Assert.Equal("22:33:14", resultB.Value);
            Assert.Equal("44:55:66", resultC.Value);
        }

        [Fact]
        public void BindOptionalAndRestArguments()
        {
            var host = new LispHost();
            host.Eval(@"
(defun test (a &optional b &rest the-rest)
    (format nil ""~a:~a:~a"" a b the-rest))
(setf result-a (test 11)
      result-b (test 22 33)
      result-c (test 44 55 66))
");
            var resultA = host.GetValue<LispString>("result-a");
            var resultB = host.GetValue<LispString>("result-b");
            var resultC = host.GetValue<LispString>("result-c");
            Assert.Equal("11:():()", resultA.Value);
            Assert.Equal("22:33:()", resultB.Value);
            Assert.Equal("44:55:(66)", resultC.Value);
        }

        [Fact]
        public void OptionalArgumentDefaultValuesAreEvaluated()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(defun test (&optional (value (+ 1 1)))
    (+ 1 value))
(test)
").LastResult;
            Assert.Equal(3, ((LispInteger)result).Value);
        }

        [Fact]
        public void KeywordArgumentDefaultValuesAreEvaluated()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(defun test (&key (value (+ 1 1)))
    (+ 1 value))
(test)
").LastResult;
            Assert.Equal(3, ((LispInteger)result).Value);
        }

        [Fact]
        public void AuxiliaryArgumentsAreComputed()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(defun test (the-list &aux (len (length the-list)))
    (+ 1 len))
(test '(1 2))
").LastResult;
            Assert.Equal(3, ((LispInteger)result).Value);
        }

        [Fact]
        public void IntermediateValuesAreRemovedFromTheArgumentStack()
        {
            var host = new LispHost();
            var executionState = host.Eval(@"
(+ 1 1)
(+ 2 2)
");
            Assert.Equal(4, ((LispInteger)executionState.LastResult).Value);
            Assert.True(executionState.TryPopArgument(out var lastResult));
            Assert.Equal(4, ((LispInteger)lastResult).Value);
            Assert.False(executionState.TryPopArgument(out var shouldNotBeHere), $"Expected no more arguments, but found [{shouldNotBeHere}]");
        }
    }
}
