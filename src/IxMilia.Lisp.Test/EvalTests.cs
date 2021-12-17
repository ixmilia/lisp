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
        public void SimpleEvalNoInit()
        {
            var host = new LispHost(useInitScript: false);
            var result = host.Eval(@"(defmacro if (pred tv fv)
    (cond (pred tv)
          (t fv)))  (if (> 1 2) ""one"" ""two"")").LastResult;
        }

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
            host.AddFunction("ADD", (host, executionState, args) => (LispInteger)args[0] + (LispInteger)args[1]);
            Assert.Equal(new LispInteger(3), host.Eval("(add 1 2)").LastResult);
        }

        [Fact]
        public void Quoted()
        {
            var host = new LispHost();
            Assert.Equal(new LispSymbol("A"), host.Eval("'a").LastResult);
            Assert.Equal(LispList.FromItems(new LispSymbol("QUOTE"), new LispSymbol("A")), host.Eval("''a").LastResult);
            Assert.Equal(LispList.FromItems(new LispInteger(1)), host.Eval("'(1)").LastResult);
            Assert.Equal("(QUOTE A)", host.Eval("(eval '''a)").LastResult.ToString());
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
            var evalResult = host.Eval("test-file.lisp", @"
(defun inc (x)
    (add x 1))
(inc 2)
");
            var error = (LispError)evalResult.LastResult;
            Assert.Equal("test-file.lisp", error.SourceLocation.Value.FilePath);
            Assert.Equal(3, error.SourceLocation.Value.Line);
            Assert.Equal(6, error.SourceLocation.Value.Column);
            Assert.Equal("INC", error.StackFrame.FunctionName);
            Assert.Equal("(ROOT)", error.StackFrame.Parent.FunctionName);
            Assert.Equal("Undefined macro/function 'ADD', found '<null>'", error.Message);
        }

        [Fact]
        public void ErrorPropagationFromCodeFunctionArgument()
        {
            var host = new LispHost();
            var evalResult = host.Eval("test-file.lisp", @"
(defun inc (x)
    (add x 1))
(inc two)
");
            var error = (LispError)evalResult.LastResult;
            Assert.Equal("test-file.lisp", error.SourceLocation.Value.FilePath);
            Assert.Equal(4, error.SourceLocation.Value.Line);
            Assert.Equal(6, error.SourceLocation.Value.Column);
            Assert.Equal("(ROOT)", error.StackFrame.FunctionName);
            Assert.Null(error.StackFrame.Parent);
            Assert.Equal("Symbol 'TWO' not found", error.Message);
        }

        [Fact]
        public void ErrorPropagationFromMultipleExpressions()
        {
            var host = new LispHost();
            var evalResult = host.Eval("test-file.lisp", @"
(+ 1 1)
(* 2 2)
(+ one 2)
(+ 3 3)
");
            var error = (LispError)evalResult.LastResult;
            Assert.Equal("test-file.lisp", error.SourceLocation.Value.FilePath);
            Assert.Equal(4, error.SourceLocation.Value.Line);
            Assert.Equal(4, error.SourceLocation.Value.Column);
            Assert.Equal("(ROOT)", error.StackFrame.FunctionName);
            Assert.Null(error.StackFrame.Parent);
            Assert.Equal("Symbol 'ONE' not found", error.Message);
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
            Assert.Equal("(A B C)", actual);
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
            var host = new LispHost(useTailCalls: true);
            var lastInterpreterStackDepth = 0;
            var lastDotNetStackDepth = 0;
            var invocationCount = 0;
            var maxInvocationCount = 10;
            host.AddFunction("RECORD-STACK-DEPTH", (host, executionState, args) =>
            {
                var currentInterpreterStackDepth = executionState.StackFrame.Depth;
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
                    return host.T;
                }
                else
                {
                    // haven't reached a stable stack depth; keep going
                    lastInterpreterStackDepth = currentInterpreterStackDepth;
                    lastDotNetStackDepth = currentDotNetStackDepth;
                    return host.Nil;
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
            var host = new LispHost(useTailCalls: true);
            var lastInterpreterStackDepth = 0;
            var lastDotNetStackDepth = 0;
            var invocationCount = 0;
            var maxInvocationCount = 10;
            host.AddFunction("RECORD-STACK-DEPTH", (host, executionState, args) =>
            {
                var currentInterpreterStackDepth = executionState.StackFrame.Depth;
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
                    return host.T;
                }
                else
                {
                    // haven't reached a stable stack depth; keep going
                    lastInterpreterStackDepth = currentInterpreterStackDepth;
                    lastDotNetStackDepth = currentDotNetStackDepth;
                    return host.Nil;
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
            var expected = LispList.FromItemsImproper(new LispSymbol("A"), new LispSymbol("B"));
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
            host.RootFrame.FunctionReturned += (sender, e) => sb.AppendLine($"returned from {e.Function.Name} with {e.ReturnValue}");
            host.Eval("(average 3 7)");
            var actual = NormalizeNewlines(sb.ToString().Trim());
            var expected = NormalizeNewlines(@"
entered AVERAGE
entered HALF
entered *
entered CONS
returned from CONS with (1 3 0.5)
entered REDUCE
entered KERNEL:*/2
returned from KERNEL:*/2 with 3
entered KERNEL:*/2
returned from KERNEL:*/2 with 1.5
returned from REDUCE with 1.5
returned from * with 1.5
returned from HALF with 1.5
entered HALF
entered *
entered CONS
returned from CONS with (1 7 0.5)
entered REDUCE
entered KERNEL:*/2
returned from KERNEL:*/2 with 7
entered KERNEL:*/2
returned from KERNEL:*/2 with 3.5
returned from REDUCE with 3.5
returned from * with 3.5
returned from HALF with 3.5
entered +
entered CONS
returned from CONS with (0 1.5 3.5)
entered REDUCE
entered KERNEL:+/2
returned from KERNEL:+/2 with 1.5
entered KERNEL:+/2
returned from KERNEL:+/2 with 5
returned from REDUCE with 5
returned from + with 5
returned from AVERAGE with 5
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
            var expected = LispList.FromItems(new LispSymbol("TWO"), new LispSymbol("DOS"));
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
        public void LabelsRecursivelyCalled()
        {
            var host = new LispHost();
            var evalResult = host.Eval(@"
(labels ((fact (n acc)
            (if (<= n 0)
                acc
                (fact (- n 1) (* n acc)))))
        (fact 5 1))
");
            EnsureNotError(evalResult.LastResult);
            Assert.Equal(120, ((LispInteger)evalResult.LastResult).Value);
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
            var testStream = new LispStream("TEST-STREAM", TextReader.Null, output);
            var host = new LispHost();
            host.SetValue("TEST-STREAM", testStream);
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
            var testStream = new LispStream("TEST-STREAM", TextReader.Null, output);
            var host = new LispHost();
            host.SetValue("TEST-STREAM", testStream);
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
            var testStream = new LispStream("TEST-STREAM", TextReader.Null, output);
            var host = new LispHost();
            host.SetValue("TEST-STREAM", testStream);
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
            var testStream = new LispStream("TEST-STREAM", TextReader.Null, output);
            var host = new LispHost();
            host.SetValue("TEST-STREAM", testStream);
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
            var testStream = new LispStream("TEST-STREAM", TextReader.Null, output);
            var host = new LispHost();
            host.SetValue("TEST-STREAM", testStream);
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
            var resultA = host.GetValue<LispList>("RESULT-A");
            var resultB = host.GetValue<LispList>("RESULT-B");
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
            var resultA = host.GetValue<LispString>("RESULT-A");
            var resultB = host.GetValue<LispString>("RESULT-B");
            var resultC = host.GetValue<LispString>("RESULT-C");
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
            var resultA = host.GetValue<LispString>("RESULT-A");
            var resultB = host.GetValue<LispString>("RESULT-B");
            var resultC = host.GetValue<LispString>("RESULT-C");
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
            var evalResult = host.Eval(@"
(+ 1 1)
(+ 2 2)
");
            Assert.Equal(4, ((LispInteger)evalResult.LastResult).Value);
            Assert.True(evalResult.ExecutionState.TryPopArgument(out var lastResult));
            Assert.Equal(4, ((LispInteger)lastResult).Value);
            Assert.False(evalResult.ExecutionState.TryPopArgument(out var shouldNotBeHere), $"Expected no more arguments, but found [{shouldNotBeHere}]");
        }

        [Fact]
        public void MacroExpansionWithFunctionInvocation()
        {
            var host = new LispHost();
            var evalResult = host.Eval(@"
(labels ((square (x) (* x x)))
    (square 2))
");
            Assert.Equal(4, ((LispInteger)evalResult.LastResult).Value);
            Assert.Null(evalResult.ExecutionState.StackFrame.GetValue("square")); // no leakage
        }

        [Fact]
        public void MacroExpansionWithFunctionReferences()
        {
            var host = new LispHost();
            var evalResult = host.Eval(@"
(labels ((square (x) (* x x)))
    (car (mapcar #'square '(2))))
");
            Assert.Equal(4, ((LispInteger)evalResult.LastResult).Value);
            Assert.Null(evalResult.ExecutionState.StackFrame.GetValue("square")); // no leakage
        }

        [Fact]
        public void IncFMacro()
        {
            var host = new LispHost();
            var evalResult = host.Eval(@"
(setf total 0)
(incf total) ; = 1
(incf total 10) ; = 11
total
");
            Assert.Equal(11, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public void DecFMacro()
        {
            var host = new LispHost();
            var evalResult = host.Eval(@"
(setf total 20)
(decf total) ; = 19
(decf total 10) ; = 9
total
");
            Assert.Equal(9, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public void LetParallelBinding()
        {
            var rawCode = @"
(let ((a 10)
      (b (+ 2 $$a)))
    ;         ^ ---- error occurs here
    (+ a b))
";
            GetCodeAndPosition(rawCode, out var code, out var line, out var column);
            var host = new LispHost();
            var evalResult = host.Eval(code);
            var error = (LispError)evalResult.LastResult;
            Assert.Equal("Symbol 'A' not found", error.Message);
            Assert.Equal(line, error.SourceLocation.Value.Line);
            Assert.Equal(column, error.SourceLocation.Value.Column);
        }

        [Fact]
        public void LetSequentialBinding()
        {
            var host = new LispHost();
            var evalResult = host.Eval(@"
(let* ((a 10)
       (b (+ 2 a)))
    (+ a b))
");
            Assert.Equal(22, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public void Push()
        {
            var host = new LispHost();
            var evalResult = host.Eval(@"
(setf my-stack ())
(setf a (push 1 my-stack))
(setf b (push 2 my-stack))
");
            Assert.Equal("(2 1)", evalResult.ExecutionState.StackFrame.GetValue("MY-STACK").ToString());
            Assert.Equal("(1)", evalResult.ExecutionState.StackFrame.GetValue("A").ToString());
            Assert.Equal("(2 1)", evalResult.ExecutionState.StackFrame.GetValue("B").ToString());
        }

        [Fact]
        public void Pop()
        {
            var host = new LispHost();
            var evalResult = host.Eval(@"
(setf my-stack '(2 1))
(setf a (pop my-stack))
(setf b (pop my-stack))
");
            Assert.Equal("()", evalResult.ExecutionState.StackFrame.GetValue("MY-STACK").ToString());
            Assert.Equal("2", evalResult.ExecutionState.StackFrame.GetValue("A").ToString());
            Assert.Equal("1", evalResult.ExecutionState.StackFrame.GetValue("B").ToString());
        }

        [Fact]
        public void WhenT()
        {
            var host = new LispHost();
            var evalResult = host.Eval(@"
(defun evals-to-t () t)
(when (evals-to-t)
    (+ 1 1)
    (+ 3 3))
");
            EnsureNotError(evalResult.LastResult);
            Assert.Equal(6, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public void WhenNil()
        {
            var host = new LispHost();
            var evalResult = host.Eval(@"
(defun evals-to-nil () ())
(when (evals-to-nil)
    (+ 1 1)
    (+ 3 3))
");
            EnsureNotError(evalResult.LastResult);
            Assert.True(evalResult.LastResult.IsNil(), $"Expected nil, but got: {evalResult.LastResult}");
        }

        [Fact]
        public void UnlessT()
        {
            var host = new LispHost();
            var evalResult = host.Eval(@"
(defun evals-to-t () t)
(unless (evals-to-t)
    (+ 1 1)
    (+ 3 3))
");
            EnsureNotError(evalResult.LastResult);
            Assert.True(evalResult.LastResult.IsNil(), $"Expected nil, but got: {evalResult.LastResult}");
        }

        [Fact]
        public void UnlessNil()
        {
            var host = new LispHost();
            var evalResult = host.Eval(@"
(defun evals-to-nil () ())
(unless (evals-to-nil)
    (+ 1 1)
    (+ 3 3))
");
            EnsureNotError(evalResult.LastResult);
            Assert.Equal(6, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public void GeneralizedVariablesWithSetF()
        {
            var result = Eval(@"
(setf the-list '(1 2 3))
(setf (car the-list) 11)
the-list
");
            Assert.Equal("(11 2 3)", result.ToString());
        }

        [Fact]
        public void DeepGeneralizedVariablesWithSetF()
        {
            var result = Eval(@"
(setf the-list '(1 2 3))
(setf (car (cdr the-list)) 22)
the-list
");
            Assert.Equal("(1 22 3)", result.ToString());
        }

        [Fact]
        public void GeneralizedVariableSetterDoesNotLivePastSetF()
        {
            var result = Eval(@"
(setf the-list '(1 2 3))
(setf head-value (car the-list))
(setf head-value 11)
(cons head-value the-list)
");
            Assert.Equal("(11 1 2 3)", result.ToString());
        }

        [Fact]
        public void GeneralizedVariablesWithIncF()
        {
            var result = Eval(@"
(setf the-list '(1 2 3))
(incf (car the-list) 11)
the-list
");
            Assert.Equal("(12 2 3)", result.ToString());
        }

        [Fact]
        public void GeneralizedVariableSetterDoesNotLivePastIncF()
        {
            var result = Eval(@"
(setf the-list '(1 2 3))
(setf head-value (car the-list))
(incf head-value 11)
(cons head-value the-list)
");
            Assert.Equal("(12 1 2 3)", result.ToString());
        }

        [Fact]
        public void GeneralizedVariablesWithDecF()
        {
            var result = Eval(@"
(setf the-list '(1 2 3))
(decf (car the-list) 11)
the-list
");
            Assert.Equal("(-10 2 3)", result.ToString());
        }

        [Fact]
        public void GeneralizedVariableSetterDoesNotLivePastDecF()
        {
            var result = Eval(@"
(setf the-list '(1 2 3))
(setf head-value (car the-list))
(decf head-value 11)
(cons head-value the-list)
");
            Assert.Equal("(-10 1 2 3)", result.ToString());
        }

        [Fact]
        public void GeneralizedVariablesWithPush()
        {
            var result = Eval(@"
(setf the-list '((a b c) 2 3))
(push 11 (car the-list))
the-list
");
            Assert.Equal("((11 A B C) 2 3)", result.ToString());
        }

        [Fact]
        public void GeneralizedVariableSetterDoesNotLivePastPush()
        {
            var result = Eval(@"
(setf the-list '((a b c) 2 3))
(setf head-value (car the-list))
(push 11 head-value)
(cons head-value the-list)
");
            Assert.Equal("((11 A B C) (A B C) 2 3)", result.ToString());
        }

        [Fact]
        public void GeneralizedVariablesWithPop()
        {
            var result = Eval(@"
(setf the-list '((a b c) 1 2 3))
(pop (car the-list))
the-list
");
            Assert.Equal("((B C) 1 2 3)", result.ToString());
        }

        [Fact]
        public void GeneralizedVariableSetterDoesNotLivePastPop()
        {
            var result = Eval(@"
(setf the-list '((a b c) 1 2 3))
(setf head-value (car the-list))
(pop head-value)
(cons head-value the-list)
");
            Assert.Equal("((B C) (A B C) 1 2 3)", result.ToString());
        }

        [Fact]
        public void NconcWithFirstParameterAsAList()
        {
            // also testing more than 2 parameters
            var result = Eval(@"
(setf x '(a b c))
(setf y '(d e f))
(setf z '(g h i))
(nconc x y z)
(list x y z)
");
            Assert.Equal("((A B C D E F G H I) (D E F G H I) (G H I))", result.ToString());
        }

        [Fact]
        public void NconcWithFirstParameterNil()
        {
            var result = Eval(@"
(setf x nil)
(setf y '(d e f))
(nconc x y)
(list x y)
");
            Assert.Equal("(() (D E F))", result.ToString());
        }

        [Fact]
        public void Nsubst()
        {
            var result = Eval(@"
(setf l '(a b c d a b c d))
(nsubst 'bee 'b l)
l
");
            Assert.Equal("(A BEE C D A BEE C D)", result.ToString());
        }
    }
}
