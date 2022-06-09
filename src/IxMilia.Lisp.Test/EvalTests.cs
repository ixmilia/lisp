using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class EvalTests : TestBase
    {
        [Fact]
        public async Task SimpleEvalNoInit()
        {
            var host = await LispHost.CreateAsync(useInitScript: false);
            var result = await host.EvalAsync(@"
(defmacro if (pred tv fv)
    (cond (pred tv)
          (t fv)))
(if (> 1 2) 11 22)");
            Assert.Equal(new LispInteger(22), result.LastResult);
        }

        [Fact]
        public async Task SingleItem()
        {
            var host = await LispHost.CreateAsync();
            Assert.Equal(new LispInteger(3), (await host.EvalAsync("3")).LastResult);
            Assert.Equal(new LispFloat(3.0), (await host.EvalAsync("3.0")).LastResult);
            Assert.Equal(new LispString("a"), (await host.EvalAsync("\"a\"")).LastResult);
        }

        [Fact]
        public async Task ExternalFunction()
        {
            var host = await LispHost.CreateAsync();
            host.AddFunction("ADD", (host, executionState, args, _cancellationToken) => Task.FromResult<LispObject>((LispInteger)args[0] + (LispInteger)args[1]));
            Assert.Equal(new LispInteger(3), (await host.EvalAsync("(add 1 2)")).LastResult);
        }

        [Fact]
        public async Task Quoted()
        {
            var host = await LispHost.CreateAsync();
            Assert.Equal(LispSymbol.CreateFromString("A"), (await host.EvalAsync("'a")).LastResult);
            Assert.Equal(LispList.FromItems(new LispUnresolvedSymbol("QUOTE"), LispSymbol.CreateFromString("A")), (await host.EvalAsync("''a")).LastResult);
            Assert.Equal(LispList.FromItems(new LispInteger(1)), (await host.EvalAsync("'(1)")).LastResult);
            Assert.Equal("(QUOTE A)", (await host.EvalAsync("(eval '''a)")).LastResult.ToString());
        }

        [Theory]
        [InlineData("`(1 ,(+ 2 3) (b))", "(1 5 (B))")]
        [InlineData("`,(+ 1 2)", "3")]
        [InlineData("`#(1 2 ,(+ 1 2) ,#(4))", "#(1 2 3 #(4))")]
        [InlineData("`a", "A")]
        public async Task BackQuoteEval(string code, string expected)
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(code);
            Assert.Null(evalResult.ReadError);
            Assert.Equal(expected, evalResult.LastResult.ToString());
        }

        [Fact]
        public async Task Variables()
        {
            var host = await LispHost.CreateAsync();
            Assert.Equal(new LispInteger(3), (await host.EvalAsync("(setq x 3) x")).LastResult);
        }

        [Fact]
        public async Task LogicalFoldingWithAnd()
        {
            var host = await LispHost.CreateAsync();
            Assert.Equal(host.Nil, (await host.EvalAsync("(and t nil)")).LastResult);
        }

        [Fact]
        public async Task LogicalFoldingWithOr()
        {
            var host = await LispHost.CreateAsync();
            Assert.Equal(host.Nil, (await host.EvalAsync("(or nil nil)")).LastResult);
        }

        [Theory]
        [InlineData("(- 4)", -4)]
        [InlineData("(+ 1 2 3 4)", 10)]
        [InlineData("(- 10 4 3 2)", 1)]
        [InlineData("(* 1 2 3 4)", 24)]
        [InlineData("(/ 24 3 2)", 4)]
        public async Task IntegerNumericFolding(string code, int expected)
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(code);
            var result = evalResult.LastResult;
            Assert.Equal(new LispInteger(expected), result);
        }

        [Theory]
        [InlineData("(- 4.0)", -4.0)]
        [InlineData("(+ 1.0 2.0 3.0 4.0)", 10.0)]
        [InlineData("(- 10.0 4.0 3.0 2.0)", 1.0)]
        [InlineData("(* 1.0 2.0 3.0 4.0)", 24.0)]
        [InlineData("(/ 24.0 3.0 2.0)", 4.0)]
        public async Task FloatNumericFolding(string code, double expected)
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(code);
            var result = evalResult.LastResult;
            Assert.Equal(new LispFloat(expected), result);
        }

        [Fact]
        public async Task MixedNumericFolding()
        {
            var host = await LispHost.CreateAsync();
            Assert.Equal(new LispFloat(10.0), (await host.EvalAsync("(+ 1.0 2 3 4)")).LastResult);
            Assert.Equal(new LispFloat(10.0), (await host.EvalAsync("(+ 1 2.0 3 4)")).LastResult);
            Assert.Equal(new LispRatio(5, 4), (await host.EvalAsync("(+ (/ 1 4) 1)")).LastResult);
            Assert.Equal(new LispRatio(5, 4), (await host.EvalAsync("(+ 1 (/ 1 4))")).LastResult);
        }

        [Theory]
        [InlineData("(< 1 2.0)")]
        [InlineData("(< 1.0 2)")]
        [InlineData("(< 1 (/ 5 4))")]
        [InlineData("(< 1.0 (/ 5 4))")]
        [InlineData("(< (/ 3 4) 1)")]
        [InlineData("(< (/ 3 4) 1.0)")]
        public async Task MixedNumericComparisons(string code)
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(code);
            Assert.Equal(host.T, evalResult.LastResult);
        }

        [Fact]
        public async Task Macros()
        {
            var host = await LispHost.CreateAsync();
            var code = @"
(defmacro if2 (pred tv fv)
    (cond (pred tv)
          (t fv)))
(if2 (= 1 1) ""one"" ""two"")
";
            var evalResult = await host.EvalAsync(code);
            var result = evalResult.LastResult;
            Assert.Equal("one", ((LispString)result).Value);
        }

        [Fact]
        public async Task Functions()
        {
            var host = await LispHost.CreateAsync();
            var code = @"
(defun inc (x)
    (+ x 1))
(inc 2)
";
            var evalResult = await host.EvalAsync(code);
            var result = evalResult.LastResult;
            Assert.Equal(new LispInteger(3), result);
        }

        [Fact]
        public async Task ErrorGeneration()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(error ""Expected '~s' but got '~s'"" 1 2)
");
            var result = evalResult.LastResult;
            Assert.Equal("Expected '1' but got '2'", ((LispError)result).Message);
        }

        [Fact]
        public async Task ErrorPropagationFromCodeFunctionBody()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync("test-file.lisp", @"
(defun inc (x)
    (add x 1))
(inc 2)
");
            var error = (LispError)evalResult.LastResult;
            Assert.Equal("test-file.lisp", error.SourceLocation.Value.FilePath);
            Assert.Equal(3, error.SourceLocation.Value.Start.Line);
            Assert.Equal(6, error.SourceLocation.Value.Start.Column);
            Assert.Equal("INC", error.StackFrame.FunctionSymbol.LocalName);
            Assert.Equal("(ROOT)", error.StackFrame.Parent.FunctionSymbol.LocalName);
            Assert.Equal("Undefined macro/function 'ADD', found '<null>'", error.Message);
        }

        [Fact]
        public async Task ErrorPropagationFromCodeFunctionArgument()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync("test-file.lisp", @"
(defun inc (x)
    (add x 1))
(inc two)
");
            var error = (LispError)evalResult.LastResult;
            Assert.Equal("test-file.lisp", error.SourceLocation.Value.FilePath);
            Assert.Equal(4, error.SourceLocation.Value.Start.Line);
            Assert.Equal(6, error.SourceLocation.Value.Start.Column);
            Assert.Equal("(ROOT)", error.StackFrame.FunctionSymbol.LocalName);
            Assert.Null(error.StackFrame.Parent);
            Assert.Equal("Symbol 'TWO' not found", error.Message);
        }

        [Fact]
        public async Task ErrorPropagationFromMultipleExpressions()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync("test-file.lisp", @"
(+ 1 1)
(* 2 2)
(+ one 2)
(+ 3 3)
");
            var error = (LispError)evalResult.LastResult;
            Assert.Equal("test-file.lisp", error.SourceLocation.Value.FilePath);
            Assert.Equal(4, error.SourceLocation.Value.Start.Line);
            Assert.Equal(4, error.SourceLocation.Value.Start.Column);
            Assert.Equal("(ROOT)", error.StackFrame.FunctionSymbol.LocalName);
            Assert.Null(error.StackFrame.Parent);
            Assert.Equal("Symbol 'ONE' not found", error.Message);
        }

        [Fact]
        public async Task ErrorStackPropagationFromInitScript()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync("*REPL*", "(+ 1 \"two\" 3)");
            var error = (LispError)evalResult.LastResult;
            var errorStackFrame = error.StackFrame;

            Assert.Equal("KERNEL:+/2", errorStackFrame.FunctionSymbol.Value);
            Assert.Null(errorStackFrame.SourceLocation); // in native code

            Assert.Equal("REDUCE", errorStackFrame.Parent.FunctionSymbol.LocalName);
            Assert.Null(errorStackFrame.Parent.SourceLocation); // in native code

            // this function can move around, but the body is one line below the definition
            var plusFunction = host.GetValue<LispFunction>("+");
            var plusFunctionLocation = plusFunction.SourceLocation.Value;
            Assert.Equal("+", errorStackFrame.Parent.Parent.FunctionSymbol.LocalName);
            Assert.Equal(new LispSourceLocation("init.lisp", new LispSourcePosition(plusFunctionLocation.Start.Line + 1, 34), new LispSourcePosition(plusFunctionLocation.Start.Line + 1, 40)), errorStackFrame.Parent.Parent.SourceLocation);

            Assert.Equal("(ROOT)", errorStackFrame.Parent.Parent.Parent.FunctionSymbol.LocalName);
            Assert.Equal(new LispSourceLocation("*REPL*", new LispSourcePosition(1, 12), new LispSourcePosition(1, 13)), errorStackFrame.Parent.Parent.Parent.SourceLocation);
            // TODO: [(1, 6)-(1, 11)) is better since that argument isn't a number

            Assert.Null(errorStackFrame.Parent.Parent.Parent.Parent);
        }

        [Fact]
        public async Task SourceLocationIsNotPropagatedFromComputedValues1()
        {
            var host = await LispHost.CreateAsync("test-file.lisp");
            var evalResult = await host.EvalAsync("(+ 1 2)");
            var result = evalResult.LastResult;
            Assert.Equal(3, ((LispInteger)result).Value);
            Assert.Null(result.SourceLocation);
        }

        [Fact]
        public async Task SourceLocationIsNotPropagatedFromComputedValues2()
        {
            var host = await LispHost.CreateAsync("host.lisp");
            var evalResult = await host.EvalAsync("some-other-location.lisp", "(+ 1 2)");
            var result = evalResult.LastResult;
            Assert.Equal(3, ((LispInteger)result).Value);
            Assert.Null(result.SourceLocation);
        }

        [Fact]
        public async Task Conditional()
        {
            var host = await LispHost.CreateAsync();

            // 'true' branch
            var evalResult = await host.EvalAsync(@"
(if (< 1 2)
    ""one""
    ""two"")");
            var result = (LispString)evalResult.LastResult;
            Assert.Equal("one", result.Value);

            // 'false' branch
            evalResult = await host.EvalAsync(@"
(if (< 2 1)
    ""one""
    ""two"")");
            result = (LispString)evalResult.LastResult;
            Assert.Equal("two", result.Value);
        }

        [Fact]
        public async Task DotNotationLists()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync("'(a . (b . (c)))");
            var result = evalResult.LastResult;
            var actual = result.ToString();
            Assert.Equal("(A B C)", actual);
        }

        [Fact]
        public async Task CircularLists()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync("#1=(3 4 5 . #1#)");
            var result = evalResult.LastResult;
            var list = (LispList)result;
            Assert.False(list.IsProperList);
            Assert.Equal(-3, list.Length); // not dictated anywhere, simply convention
            Assert.Equal(new LispInteger(3), list.Value);
            Assert.Equal("#1=(3 4 5 . #1#)", list.ToString());

            list = (LispList)(await host.EvalAsync("#1=(#1# . 2)")).LastResult;
            Assert.False(list.IsProperList);
            Assert.Equal(-1, list.Length);
            Assert.Equal(new LispInteger(2), list.Next);
            Assert.True(ReferenceEquals(list, list.Value));
            Assert.Equal("#1=(#1# . 2)", list.ToString());

            list = (LispList)(await host.EvalAsync("#1=(2 3 #1#)")).LastResult;
            Assert.True(list.IsProperList);
            Assert.Equal(-3, list.Length);
            Assert.Equal("#1=(2 3 #1#)", list.ToString());
        }

        [Fact]
        public async Task LetTest()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(defun avg (x y)
    (let ((sum (+ x y)))
        (/ sum 2.0)))
(avg 3.0 7.0)
");
            var result = evalResult.LastResult;
            Assert.Equal(new LispFloat(5.0), result);
        }

        [Fact]
        public async Task MacroFunctionVariableNames()
        {
            // redefined function variable names shadow previous macro expansion values
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(defmacro if2 (pred tv fv)
    (cond (pred tv)
          (t fv)))

; argument `pred` shadows macro expansion with the same name
; used to cause stack overflow in evaluating arguments
(defun asrt (pred msg)
    (if pred t msg))
(asrt t ""not hit"")
");
            var result = evalResult.LastResult;
            Assert.Equal(host.T, result);
        }

        [Fact]
        public async Task TailCallWithCond()
        {
            var host = await LispHost.CreateAsync(useTailCalls: true);
            var lastInterpreterStackDepth = 0;
            var lastDotNetStackDepth = 0;
            var invocationCount = 0;
            var maxInvocationCount = 10;
            host.AddFunction("RECORD-STACK-DEPTH", (host, executionState, args, _cancellationToken) =>
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
                    return Task.FromResult(host.T);
                }
                else
                {
                    // haven't reached a stable stack depth; keep going
                    lastInterpreterStackDepth = currentInterpreterStackDepth;
                    lastDotNetStackDepth = currentDotNetStackDepth;
                    return Task.FromResult(host.Nil);
                }
            });
            var evalResult = await host.EvalAsync(@"
(defun do-lots-of-tail-calls-with-cond ()
    (cond ((record-stack-depth) t)                                      ; done
          (t                    (do-lots-of-tail-calls-with-cond))))    ; keep going
(do-lots-of-tail-calls-with-cond)
");
            var result = evalResult.LastResult;
            Assert.True(invocationCount >= 2, $"Must have been invoked at least twice, but was only invoked {invocationCount} time(s).");
            Assert.Equal(host.T, result);
        }

        [Fact]
        public async Task TailCallWithIf()
        {
            var host = await LispHost.CreateAsync(useTailCalls: true);
            var lastInterpreterStackDepth = 0;
            var lastDotNetStackDepth = 0;
            var invocationCount = 0;
            var maxInvocationCount = 10;
            host.AddFunction("RECORD-STACK-DEPTH", (host, executionState, args, _cancellationToken) =>
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
                    return Task.FromResult(host.T);
                }
                else
                {
                    // haven't reached a stable stack depth; keep going
                    lastInterpreterStackDepth = currentInterpreterStackDepth;
                    lastDotNetStackDepth = currentDotNetStackDepth;
                    return Task.FromResult(host.Nil);
                }
            });
            var evalResult = await host.EvalAsync(@"
(defun do-lots-of-tail-calls-with-if ()
    (if (record-stack-depth)
        t                                   ; done
        (do-lots-of-tail-calls-with-if)))   ; keep going
(do-lots-of-tail-calls-with-if)
");
            var result = evalResult.LastResult;
            Assert.True(invocationCount >= 2, $"Must have been invoked at least twice, but was only invoked {invocationCount} time(s).");
            Assert.Equal(host.T, result);
        }

        [Fact]
        public async Task InvokeBuiltInNamedFunctionReference()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"(funcall #'cons 'a 'b)");
            var result = evalResult.LastResult;
            var expected = LispList.FromItemsImproper(LispSymbol.CreateFromString("A"), LispSymbol.CreateFromString("B"));
            Assert.Equal(expected, result);
        }

        [Fact]
        public async Task InvokeUserDefinedNamedFunctionReference()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(defun add (a b)
    (+ a b))
(funcall #'add 2 3)
");
            var result = evalResult.LastResult;
            Assert.Equal(new LispInteger(5), result);
        }

        [Fact]
        public async Task InvokeNamedFunctionFromSymbol()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(setf plus-function #'+)
(funcall plus-function 2 3)
");
            var result = evalResult.LastResult;
            Assert.Equal(new LispInteger(5), result);
        }

        [Fact]
        public async Task InvokeLambdaFromReference()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(funcall #'(lambda (n) (+ 1 n)) 2)
");
            var result = evalResult.LastResult;
            Assert.Equal(new LispInteger(3), result);
        }

        [Fact]
        public async Task InvokeLambdaFromSymbol()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(setf inc-function #'(lambda (n) (+ 1 n)))
(funcall inc-function 2)
");
            var result = evalResult.LastResult;
            Assert.Equal(new LispInteger(3), result);
        }

        [Fact]
        public async Task ApplyFunctionReference()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(apply #'+ '(2 3))
");
            var result = evalResult.LastResult;
            Assert.Equal(5, ((LispInteger)result).Value);
        }

        [Fact]
        public async Task EnterAndReturnFunctionEvent()
        {
            var host = await LispHost.CreateAsync();
            var sb = new StringBuilder();
            await host.EvalAsync(@"
(defun half (n) (* n 0.5))
(defun average (x y)
    (+ (half x) (half y)))
");
            host.RootFrame.FunctionEntered += (sender, e) => sb.AppendLine($"entered {e.Frame.FunctionSymbol.ToDisplayString(host.CurrentPackage)}");
            host.RootFrame.FunctionReturned += (sender, e) => sb.AppendLine($"returned from {e.Function.NameSymbol.ToDisplayString(host.CurrentPackage)} with {e.ReturnValue}");
            await host.EvalAsync("(average 3 7)");
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
        public async Task LambdaWithLexicalClosure()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(setf words '((one uno) (two dos) (three tres)))
(defun my-assoc (key table)
  (find-if #'(lambda (entry)
    (equal key (first entry)))
  table))
(my-assoc 'two words)
");
            var result = evalResult.LastResult;
            var expected = LispList.FromItems(LispSymbol.CreateFromString("TWO"), LispSymbol.CreateFromString("DOS"));
            Assert.Equal(expected, result);
        }

        [Fact]
        public async Task LambdaCapture()
        {
            var host = await LispHost.CreateAsync();
            await host.EvalAsync(@"
(defun make-greater-p (n)
    #'(lambda (x) (> x n)))
(setf pred (make-greater-p 3))
");
            Assert.Equal(host.Nil, (await host.EvalAsync("(funcall pred 2)")).LastResult);
            Assert.Equal(host.T, (await host.EvalAsync("(funcall pred 5)")).LastResult);
            Assert.Equal(new LispInteger(4), (await host.EvalAsync("(find-if pred '(2 3 4 5 6 7 8 9))")).LastResult);
        }

        [Fact]
        public async Task LabelsFunctionDefinition()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(labels ((increment-by-one (n)
             (+ n 1))
         (increment-by-two (n)
             (increment-by-one (increment-by-one n)))
        )
    (+ (increment-by-one 1) (increment-by-two 4)) ; (1 + 1) + (4 + 2) = 8
)
");
            var result = evalResult.LastResult;
            Assert.Equal(new LispInteger(8), result);

            // ensure nothing leaked
            Assert.Null(host.GetValue("increment-by-one"));
            Assert.Null(host.GetValue("increment-by-two"));
            Assert.Null(host.GetValue("n"));
        }

        [Fact]
        public async Task LabelsRecursivelyCalled()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
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
        public async Task FormatOutput()
        {
            var output = new StringWriter();
            var host = await LispHost.CreateAsync(output: output);
            await host.EvalAsync(@"(format t ""hello"")");
            var result = NormalizeNewlines(output.ToString());
            Assert.Equal("hello", result);
        }

        [Fact]
        public async Task FormatOutputWithArgument()
        {
            var output = new StringWriter();
            var host = await LispHost.CreateAsync(output: output);
            await host.EvalAsync(@"(format t ""hello ~S"" ""world"")");
            var result = NormalizeNewlines(output.ToString());
            Assert.Equal("hello \"world\"", result);
        }

        [Fact]
        public async Task MultipleCallsToFormat()
        {
            var output = new StringWriter();
            var host = await LispHost.CreateAsync(output: output);
            await host.EvalAsync(@"
(format t ""1"")
(format t ""2"")
(format t ""3"")
");
            var result = NormalizeNewlines(output.ToString());
            Assert.Equal("123", result);
        }

        [Fact]
        public async Task FormatToStream()
        {
            var output = new StringWriter();
            var testStream = new LispTextStream("TEST-STREAM", TextReader.Null, output);
            var host = await LispHost.CreateAsync();
            host.SetValue("TEST-STREAM", testStream);
            var result = await host.EvalAsync(@"
(format test-stream ""~S~%"" ""just a string"")
(format test-stream ""~S~%"" '(+ 2 3))
");
            Assert.IsNotType<LispError>(result);
            var actual = NormalizeNewlines(output.ToString());
            Assert.Equal("\"just a string\"\n(+ 2 3)\n", actual);
        }

        [Fact]
        public async Task TerPriFunction()
        {
            var output = new StringWriter();
            var host = await LispHost.CreateAsync(output: output);
            var evalResult = await host.EvalAsync(@"
(terpri)
");
            var result = evalResult.LastResult;
            Assert.True(result.IsNil());
            Assert.Equal("\n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public async Task TerPriFunctionWithStream()
        {
            var output = new StringWriter();
            var testStream = new LispTextStream("TEST-STREAM", TextReader.Null, output);
            var host = await LispHost.CreateAsync();
            host.SetValue("TEST-STREAM", testStream);
            var evalResult = await host.EvalAsync(@"
(terpri test-stream)
");
            var result = evalResult.LastResult;
            Assert.True(result.IsNil());
            Assert.Equal("\n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public async Task Prin1Function()
        {
            var output = new StringWriter();
            var host = await LispHost.CreateAsync(output: output);
            var evalResult = await host.EvalAsync(@"
(prin1 ""abc"")
");
            var result = evalResult.LastResult;
            Assert.Equal("abc", ((LispString)result).Value);
            Assert.Equal("\"abc\"\n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public async Task Prin1FunctionWithStream()
        {
            var output = new StringWriter();
            var testStream = new LispTextStream("TEST-STREAM", TextReader.Null, output);
            var host = await LispHost.CreateAsync();
            host.SetValue("TEST-STREAM", testStream);
            var evalResult = await host.EvalAsync(@"
(prin1 ""abc"" test-stream)
");
            var result = evalResult.LastResult;
            Assert.Equal("abc", ((LispString)result).Value);
            Assert.Equal("\"abc\"\n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public async Task PrinCFunction()
        {
            var output = new StringWriter();
            var host = await LispHost.CreateAsync(output: output);
            var evalResult = await host.EvalAsync(@"
(princ ""abc"")
");
            var result = evalResult.LastResult;
            Assert.Equal("abc", ((LispString)result).Value);
            Assert.Equal("abc\n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public async Task PrinCFunctionWithStream()
        {
            var output = new StringWriter();
            var testStream = new LispTextStream("TEST-STREAM", TextReader.Null, output);
            var host = await LispHost.CreateAsync();
            host.SetValue("TEST-STREAM", testStream);
            var evalResult = await host.EvalAsync(@"
(princ ""abc"" test-stream)
");
            var result = evalResult.LastResult;
            Assert.Equal("abc", ((LispString)result).Value);
            Assert.Equal("abc\n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public async Task PrintFunction()
        {
            var output = new StringWriter();
            var host = await LispHost.CreateAsync(output: output);
            var evalResult = await host.EvalAsync(@"
(print ""abc"")
");
            var result = evalResult.LastResult;
            Assert.Equal("abc", ((LispString)result).Value);
            Assert.Equal("\n\"abc\"\n \n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public async Task PrintFunctionWithStream()
        {
            var output = new StringWriter();
            var testStream = new LispTextStream("TEST-STREAM", TextReader.Null, output);
            var host = await LispHost.CreateAsync();
            host.SetValue("TEST-STREAM", testStream);
            var evalResult = await host.EvalAsync(@"
(print ""abc"" test-stream)
");
            var result = evalResult.LastResult;
            Assert.Equal("abc", ((LispString)result).Value);
            Assert.Equal("\n\"abc\"\n \n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public async Task LetBlocksEvaluateManyStatements()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(let ((x 1))
    x
    (+ x 2)) ; this is the real result
");
            var result = evalResult.LastResult;
            Assert.Equal(3, ((LispInteger)result).Value);
        }

        [Fact]
        public async Task BindRestArgumentsInFunction()
        {
            var host = await LispHost.CreateAsync();
            await host.EvalAsync(@"
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
        public async Task BindOptionalArgumentsInFunction()
        {
            var host = await LispHost.CreateAsync();
            await host.EvalAsync(@"
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
        public async Task BindOptionalAndRestArguments()
        {
            var host = await LispHost.CreateAsync();
            await host.EvalAsync(@"
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
        public async Task OptionalArgumentDefaultValuesAreEvaluated()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(defun test (&optional (value (+ 1 1)))
    (+ 1 value))
(test)
");
            var result = evalResult.LastResult;
            Assert.Equal(3, ((LispInteger)result).Value);
        }

        [Fact]
        public async Task KeywordSymbolsImmediatelyResolveToThemselves()
        {
            var host = await LispHost.CreateAsync(useInitScript: false);
            var result = await host.EvalAsync(":some-keyword");
            Assert.Equal(new LispResolvedSymbol("KEYWORD", "SOME-KEYWORD", isPublic: true), result.LastResult);
        }

        [Fact]
        public async Task KeywordArgumentDefaultValuesAreEvaluated()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(defun test (&key (value (+ 1 1)))
    (+ 1 value))
(test)
");
            var result = evalResult.LastResult;
            Assert.Equal(3, ((LispInteger)result).Value);
        }

        [Fact]
        public async Task AuxiliaryArgumentsAreComputed()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(defun test (the-list &aux (len (length the-list)))
    (+ 1 len))
(test '(1 2))
");
            var result = evalResult.LastResult;
            Assert.Equal(3, ((LispInteger)result).Value);
        }

        [Fact]
        public async Task IntermediateValuesAreRemovedFromTheArgumentStack()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(+ 1 1)
(+ 2 2)
");
            Assert.Equal(4, ((LispInteger)evalResult.LastResult).Value);
            Assert.True(evalResult.ExecutionState.TryPopArgument(out var lastResult));
            Assert.Equal(4, ((LispInteger)lastResult).Value);
            Assert.False(evalResult.ExecutionState.TryPopArgument(out var shouldNotBeHere), $"Expected no more arguments, but found [{shouldNotBeHere}]");
        }

        [Fact]
        public async Task MacroExpansionWithFunctionInvocation()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(labels ((square (x) (* x x)))
    (square 2))
");
            Assert.Equal(4, ((LispInteger)evalResult.LastResult).Value);
            Assert.Null(evalResult.ExecutionState.StackFrame.GetValue(LispSymbol.CreateFromString("SQUARE").Resolve(host.CurrentPackage))); // no leakage
        }

        [Fact]
        public async Task MacroExpansionWithFunctionReferences()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(labels ((square (x) (* x x)))
    (car (mapcar #'square '(2))))
");
            Assert.Equal(4, ((LispInteger)evalResult.LastResult).Value);
            Assert.Null(evalResult.ExecutionState.StackFrame.GetValue(LispSymbol.CreateFromString("SQUARE").Resolve(host.CurrentPackage))); // no leakage
        }

        [Fact]
        public async Task IncFMacro()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(setf total 0)
(incf total) ; = 1
(incf total 10) ; = 11
total
");
            Assert.Equal(11, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public async Task DecFMacro()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(setf total 20)
(decf total) ; = 19
(decf total 10) ; = 9
total
");
            Assert.Equal(9, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public async Task LetParallelBinding()
        {
            var rawCode = @"
(let ((a 10)
      (b (+ 2 $$a)))
    ;         ^ ---- error occurs here
    (+ a b))
";
            GetCodeAndPosition(rawCode, out var code, out var position);
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(code);
            var error = (LispError)evalResult.LastResult;
            Assert.Equal("Symbol 'A' not found", error.Message);
            Assert.Equal(position, error.SourceLocation.Value.Start);
        }

        [Fact]
        public async Task LetSequentialBinding()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(let* ((a 10)
       (b (+ 2 a)))
    (+ a b))
");
            Assert.Equal(22, ((LispInteger)evalResult.LastResult).Value);
        }

        [Theory]
        [InlineData("(let ((x 42) (y 43)) (+ x y))", "(APPLY (LAMBDA (X Y) (+ X Y)) (LIST (QUOTE 42) (QUOTE 43)))")]
        [InlineData("(let* ((x 42) (y 43)) (+ x y))", "(APPLY (LAMBDA () (SETF X 42) (SETF Y 43) (+ X Y)) (LIST))")]
        public async Task LetMacroExpansion(string code, string expected)
        {
            var host = await LispHost.CreateAsync();
            var input = new LispTextStream("", new StringReader(code), TextWriter.Null);
            host.ObjectReader.SetReaderStream(input);
            var readResult = await host.ObjectReader.ReadAsync(false, new LispError("EOF"), false);
            Assert.IsType<LispList>(readResult.LastResult);
            var list = ((LispList)readResult.LastResult).ToList();
            var args = list.Skip(1).ToArray();
            var bindSequentially = list[0].ToString() == "LET*";
            var result = LispDefaultContext.Let(args, bindSequentially);
            var actual = result.ToString();
            Assert.Equal(expected, actual);
        }

        [Fact]
        public async Task Push()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(setf my-stack ())
(setf a (push 1 my-stack))
(setf b (push 2 my-stack))
");
            Assert.Equal("(2 1)", evalResult.ExecutionState.StackFrame.GetValue(LispSymbol.CreateFromString("MY-STACK").Resolve(host.CurrentPackage)).ToString());
            Assert.Equal("(1)", evalResult.ExecutionState.StackFrame.GetValue(LispSymbol.CreateFromString("A").Resolve(host.CurrentPackage)).ToString());
            Assert.Equal("(2 1)", evalResult.ExecutionState.StackFrame.GetValue(LispSymbol.CreateFromString("B").Resolve(host.CurrentPackage)).ToString());
        }

        [Fact]
        public async Task Pop()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(setf my-stack '(2 1))
(setf a (pop my-stack))
(setf b (pop my-stack))
");
            Assert.Equal("()", evalResult.ExecutionState.StackFrame.GetValue(LispSymbol.CreateFromString("MY-STACK").Resolve(host.CurrentPackage)).ToString());
            Assert.Equal("2", evalResult.ExecutionState.StackFrame.GetValue(LispSymbol.CreateFromString("A").Resolve(host.CurrentPackage)).ToString());
            Assert.Equal("1", evalResult.ExecutionState.StackFrame.GetValue(LispSymbol.CreateFromString("B").Resolve(host.CurrentPackage)).ToString());
        }

        [Fact]
        public async Task WhenT()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(defun evals-to-t () t)
(when (evals-to-t)
    (+ 1 1)
    (+ 3 3))
");
            EnsureNotError(evalResult.LastResult);
            Assert.Equal(6, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public async Task WhenNil()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(defun evals-to-nil () ())
(when (evals-to-nil)
    (+ 1 1)
    (+ 3 3))
");
            EnsureNotError(evalResult.LastResult);
            Assert.True(evalResult.LastResult.IsNil(), $"Expected nil, but got: {evalResult.LastResult}");
        }

        [Fact]
        public async Task UnlessT()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(defun evals-to-t () t)
(unless (evals-to-t)
    (+ 1 1)
    (+ 3 3))
");
            EnsureNotError(evalResult.LastResult);
            Assert.True(evalResult.LastResult.IsNil(), $"Expected nil, but got: {evalResult.LastResult}");
        }

        [Fact]
        public async Task UnlessNil()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
(defun evals-to-nil () ())
(unless (evals-to-nil)
    (+ 1 1)
    (+ 3 3))
");
            EnsureNotError(evalResult.LastResult);
            Assert.Equal(6, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public async Task GeneralizedVariablesWithSetF()
        {
            var result = await EvalAsync(@"
(setf the-list '(1 2 3))
(setf (car the-list) 11)
the-list
");
            Assert.Equal("(11 2 3)", result.ToString());
        }

        [Fact]
        public async Task DeepGeneralizedVariablesWithSetF()
        {
            var result = await EvalAsync(@"
(setf the-list '(1 2 3))
(setf (car (cdr the-list)) 22)
the-list
");
            Assert.Equal("(1 22 3)", result.ToString());
        }

        [Fact]
        public async Task GeneralizedVariableSetterDoesNotLivePastSetF()
        {
            var result = await EvalAsync(@"
(setf the-list '(1 2 3))
(setf head-value (car the-list))
(setf head-value 11)
(cons head-value the-list)
");
            Assert.Equal("(11 1 2 3)", result.ToString());
        }

        [Fact]
        public async Task GeneralizedVariablesWithIncF()
        {
            var result = await EvalAsync(@"
(setf the-list '(1 2 3))
(incf (car the-list) 11)
the-list
");
            Assert.Equal("(12 2 3)", result.ToString());
        }

        [Fact]
        public async Task GeneralizedVariableSetterDoesNotLivePastIncF()
        {
            var result = await EvalAsync(@"
(setf the-list '(1 2 3))
(setf head-value (car the-list))
(incf head-value 11)
(cons head-value the-list)
");
            Assert.Equal("(12 1 2 3)", result.ToString());
        }

        [Fact]
        public async Task GeneralizedVariablesWithDecF()
        {
            var result = await EvalAsync(@"
(setf the-list '(1 2 3))
(decf (car the-list) 11)
the-list
");
            Assert.Equal("(-10 2 3)", result.ToString());
        }

        [Fact]
        public async Task GeneralizedVariableSetterDoesNotLivePastDecF()
        {
            var result = await EvalAsync(@"
(setf the-list '(1 2 3))
(setf head-value (car the-list))
(decf head-value 11)
(cons head-value the-list)
");
            Assert.Equal("(-10 1 2 3)", result.ToString());
        }

        [Fact]
        public async Task GeneralizedVariablesWithPush()
        {
            var result = await EvalAsync(@"
(setf the-list '((a b c) 2 3))
(push 11 (car the-list))
the-list
");
            Assert.Equal("((11 A B C) 2 3)", result.ToString());
        }

        [Fact]
        public async Task GeneralizedVariableSetterDoesNotLivePastPush()
        {
            var result = await EvalAsync(@"
(setf the-list '((a b c) 2 3))
(setf head-value (car the-list))
(push 11 head-value)
(cons head-value the-list)
");
            Assert.Equal("((11 A B C) (A B C) 2 3)", result.ToString());
        }

        [Fact]
        public async Task GeneralizedVariablesWithPop()
        {
            var result = await EvalAsync(@"
(setf the-list '((a b c) 1 2 3))
(pop (car the-list))
the-list
");
            Assert.Equal("((B C) 1 2 3)", result.ToString());
        }

        [Fact]
        public async Task GeneralizedVariableSetterDoesNotLivePastPop()
        {
            var result = await EvalAsync(@"
(setf the-list '((a b c) 1 2 3))
(setf head-value (car the-list))
(pop head-value)
(cons head-value the-list)
");
            Assert.Equal("((B C) (A B C) 1 2 3)", result.ToString());
        }

        [Fact]
        public async Task NconcWithFirstParameterAsAList()
        {
            // also testing more than 2 parameters
            var result = await EvalAsync(@"
(setf x '(a b c))
(setf y '(d e f))
(setf z '(g h i))
(nconc x y z)
(list x y z)
");
            Assert.Equal("((A B C D E F G H I) (D E F G H I) (G H I))", result.ToString());
        }

        [Fact]
        public async Task NconcWithFirstParameterNil()
        {
            var result = await EvalAsync(@"
(setf x nil)
(setf y '(d e f))
(nconc x y)
(list x y)
");
            Assert.Equal("(() (D E F))", result.ToString());
        }

        [Fact]
        public async Task Nsubst()
        {
            var result = await EvalAsync(@"
(setf l '(a b c d a b c d))
(nsubst 'bee 'b l)
l
");
            Assert.Equal("(A BEE C D A BEE C D)", result.ToString());
        }

        [Fact]
        public async Task PackageGlobalVariableIsUpdatedWithHostProperty()
        {
            var host = await LispHost.CreateAsync(useInitScript: false);
            var testPackage = host.AddPackage("TEST-PACKAGE", new[] { host.CurrentPackage });
            var package = host.GetValue<LispPackage>("*PACKAGE*");
            Assert.Equal("COMMON-LISP", package.Name);

            host.CurrentPackage = testPackage;
            package = host.GetValue<LispPackage>("*PACKAGE*");
            Assert.Equal("TEST-PACKAGE", package.Name);
        }

        [Fact]
        public async Task CurrentPackageIsUpdatedWithInPackageFunctionWithKeyword()
        {
            var host = await LispHost.CreateAsync(useInitScript: false);
            host.AddPackage("TEST-PACKAGE", new[] { host.CurrentPackage });
            var package = host.GetValue<LispPackage>("*PACKAGE*");
            Assert.Equal("COMMON-LISP", package.Name);

            await host.EvalAsync("(in-package :test-package)");
            package = host.GetValue<LispPackage>("*PACKAGE*");
            Assert.Equal("TEST-PACKAGE", package.Name);
        }

        [Fact]
        public async Task CurrentPackageIsUpdatedWithInPackageFunctionWithString()
        {
            var host = await LispHost.CreateAsync();
            host.AddPackage("TEST-PACKAGE", new[] { host.CurrentPackage });
            var package = host.GetValue<LispPackage>("*PACKAGE*");
            Assert.Equal("COMMON-LISP-USER", package.Name);

            await host.EvalAsync("(in-package \"TEST-PACKAGE\")");
            package = host.GetValue<LispPackage>("*PACKAGE*");
            Assert.Equal("TEST-PACKAGE", package.Name);
        }

        [Fact]
        public async Task PackageIsCreatedWithDefPackageWithKeyword()
        {
            var host = await LispHost.CreateAsync(useInitScript: false);
            await host.EvalAsync("(defpackage :test-package) (in-package :test-package)");
            var package = host.GetValue<LispPackage>("*PACKAGE*");
            Assert.Equal("TEST-PACKAGE", package.Name);
        }

        [Fact]
        public async Task PackageIsCreatedWithDefPackageWithString()
        {
            var host = await LispHost.CreateAsync();
            await host.EvalAsync("(defpackage \"TEST-PACKAGE\") (in-package :test-package)");
            var package = host.GetValue<LispPackage>("*PACKAGE*");
            Assert.Equal("TEST-PACKAGE", package.Name);
        }

        [Fact]
        public async Task PackagesCanInheritSymbols()
        {
            var host = await LispHost.CreateAsync(useInitScript: false);
            var result = await host.EvalAsync(@"
(defpackage :a)
(in-package :a)
(setf aa 1)

(defpackage :b
    (:use :a))
(in-package :b)
(setf bb 2)

(defpackage :x)
(in-package :x)
(setf xx 3)

(defpackage :c
    (:use :b :x))
(in-package :c)
(list aa  ; a:aa => 1
      bb  ; b:bb => 2
      xx) ; x:xx => 3
");
            Assert.Equal("(1 2 3)", result.LastResult.ToString());
        }

        [Fact]
        public async Task UnsetSymbolCanBeRetrievedFromNonExistantPackage()
        {
            var gotUnsetSymbol = false;
            var host = await LispHost.CreateAsync(useInitScript: false, getUnsetSymbol: resolvedSymbol =>
            {
                if (resolvedSymbol.Value == "not-a-package:not-a-symbol")
                {
                    gotUnsetSymbol = true;
                    return new LispInteger(42);
                }

                return null;
            });
            var result = host.GetValue("not-a-package:not-a-symbol");
            Assert.True(gotUnsetSymbol);
            Assert.Equal(new LispInteger(42), result);
        }

        [Fact]
        public async Task UnsetSymbolCanBeRetrievedFromExistingPackage()
        {
            var gotUnsetSymbol = false;
            LispHost host = null;
            host = await LispHost.CreateAsync(useInitScript: false, getUnsetSymbol: resolvedSymbol =>
            {
                if (resolvedSymbol.Value == $"{host.CurrentPackage.Name}:not-a-symbol")
                {
                    gotUnsetSymbol = true;
                    return new LispInteger(42);
                }

                return null;
            });
            var result = host.GetValue($"{host.CurrentPackage.Name}:not-a-symbol");
            Assert.True(gotUnsetSymbol);
            Assert.Equal(new LispInteger(42), result);
        }

        [Fact]
        public async Task UnsetSymbolFunctionIsNotCalledWhenAnExistingValueIsFound()
        {
            var gotUnsetSymbol = false;
            var host = await LispHost.CreateAsync(useInitScript: false, getUnsetSymbol: resolvedSymbol =>
            {
                gotUnsetSymbol = true;
                return null;
            });
            host.SetValue("some-int", new LispInteger(42));
            var result = host.GetValue("some-int");
            Assert.False(gotUnsetSymbol);
            Assert.Equal(new LispInteger(42), result);
        }
    }
}
