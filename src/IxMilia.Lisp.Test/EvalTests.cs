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
        private static async Task<(LispHost Host, LispObject Result)> EvalAndGetHostAndResultAsync(string code, bool useInitScript = true, bool useTailCalls = false)
        {
            var host = await CreateHostAsync(useTailCalls: useTailCalls, useInitScript: useInitScript);
            var result = await EvalAsync(host, code);
            return (host, result);
        }

        private static async Task<LispObject> EvalAsync(string code, bool useInitScript = true, bool useTailCalls = false)
        {
            var (_host, result) = await EvalAndGetHostAndResultAsync(code, useInitScript, useTailCalls);
            return result;
        }

        private static async Task<string> EvalAndGetDisplayStringAsync(string code, bool useInitScript = true, bool useTailCalls = false)
        {
            var (host, result) = await EvalAndGetHostAndResultAsync(code, useInitScript, useTailCalls);
            var stringResult = result.ToDisplayString(host.CurrentPackage);
            return stringResult;
        }

        private static async Task<LispObject> EvalAsync(LispHost host, string code)
        {
            var executionState = LispExecutionState.CreateExecutionState(host.RootFrame, allowHalting: false);
            var result = await host.EvalAsync("test.lisp", code, executionState);
            return result.Value;
        }

        [Theory]
        [InlineData("3", "3")]
        [InlineData(" 3 ", "3")]
        [InlineData("3 4", "4")]
        [InlineData(" 3 4 ", "4")]
        public async Task SingleEvalNoInit(string code, string expectedResult)
        {
            var result = await EvalAsync(code, useInitScript: false);
            var actual = result?.ToString();
            Assert.Equal(expectedResult, actual);
        }

        [Fact]
        public async Task MacroEvalNoInit()
        {
            var result = await EvalAsync(@"
(defmacro if (pred tv fv)
    (cond (pred tv)
          (t fv)))
(if (> 1 2) 11 22)", useInitScript: false);
            Assert.Equal(new LispInteger(22), result);
        }

        [Fact]
        public async Task SingleItem()
        {
            Assert.Equal(new LispInteger(3), await EvalAsync("3"));
            Assert.Equal(new LispFloat(3.0), await EvalAsync("3.0"));
            Assert.Equal(new LispString("a"), await EvalAsync("\"a\""));
        }

        [Fact]
        public async Task ExternalFunction()
        {
            var host = await CreateHostAsync();
            host.AddFunction("ADD", (host, executionState, args, _cancellationToken) => Task.FromResult<LispObject>((LispInteger)args[0] + (LispInteger)args[1]));
            Assert.Equal(new LispInteger(3), await EvalAsync(host, "(add 1 2)"));
        }

        [Theory]
        [InlineData("'a", "A")]
        [InlineData("''a", "(QUOTE A)")]
        [InlineData("'(1)", "(1)")]
        [InlineData("(eval '''a)", "(QUOTE A)")]
        public async Task Quoted(string code, string expected)
        {
            var result = await EvalAndGetDisplayStringAsync(code);
            Assert.Equal(expected, result);
        }

        [Theory]
        [InlineData("`(1 ,(+ 2 3) (b))", "(1 5 (B))")]
        [InlineData("`,(+ 1 2)", "3")]
        [InlineData("`#(1 2 ,(+ 1 2) ,#(4))", "#(1 2 3 #(4))")]
        [InlineData("`a", "A")]
        public async Task BackQuoteEval(string code, string expected)
        {
            var evalResult = await EvalAndGetDisplayStringAsync(code);
            Assert.Equal(expected, evalResult);
        }

        [Fact]
        public async Task Variables()
        {
            Assert.Equal(new LispInteger(3), await EvalAsync("(setq x 3) x"));
        }

        [Fact]
        public async Task LogicalFoldingWithAnd()
        {
            var host = await CreateHostAsync();
            Assert.Equal(host.Nil, await EvalAsync(host, "(and t nil)"));
        }

        [Fact]
        public async Task LogicalFoldingWithOr()
        {
            var host = await CreateHostAsync();
            Assert.Equal(host.Nil, await EvalAsync(host, "(or nil nil)"));
        }

        [Theory]
        [InlineData("(- 4)", -4)]
        [InlineData("(+ 1 2 3 4)", 10)]
        [InlineData("(- 10 4 3 2)", 1)]
        [InlineData("(* 1 2 3 4)", 24)]
        [InlineData("(/ 24 3 2)", 4)]
        public async Task IntegerNumericFolding(string code, int expected)
        {
            var result = await EvalAsync(code);
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
            var result = await EvalAsync(code);
            Assert.Equal(new LispFloat(expected), result);
        }

        [Fact]
        public async Task MixedNumericFolding()
        {
            Assert.Equal(new LispFloat(10.0), await EvalAsync("(+ 1.0 2 3 4)"));
            Assert.Equal(new LispFloat(10.0), await EvalAsync("(+ 1 2.0 3 4)"));
            Assert.Equal(new LispRatio(5, 4), await EvalAsync("(+ (/ 1 4) 1)"));
            Assert.Equal(new LispRatio(5, 4), await EvalAsync("(+ 1 (/ 1 4))"));
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
            var host = await CreateHostAsync();
            var result = await EvalAsync(host, code);
            Assert.Equal(host.T, result);
        }

        [Fact]
        public async Task Macros()
        {
            var code = @"
(defmacro if2 (pred tv fv)
    (cond (pred tv)
          (t fv)))
(if2 (= 1 1) ""one"" ""two"")
";
            var result = await EvalAsync(code);
            Assert.Equal("one", ((LispString)result).Value);
        }

        [Fact]
        public async Task Functions()
        {
            var code = @"
(defun inc (x)
    (+ x 1))
(inc 2)
";
            var result = await EvalAsync(code);
            Assert.Equal(new LispInteger(3), result);
        }

        [Fact]
        public async Task ErrorGeneration()
        {
            var result = await EvalAsync(@"
(error ""Expected '~s' but got '~s'"" 1 2)
");
            Assert.Equal("Expected '1' but got '2'", ((LispError)result).Message);
        }

        [Fact]
        public async Task ErrorPropagationFromCodeFunctionBody()
        {
            var result = await EvalAsync(@"
(defun inc (x)
    (add x 1))
(inc 2)
");
            var error = (LispError)result;
            Assert.Equal("test.lisp", error.SourceLocation.Value.FilePath);
            Assert.Equal(3, error.SourceLocation.Value.Start.Line);
            Assert.Equal(6, error.SourceLocation.Value.Start.Column);
            Assert.Equal("INC", error.StackFrame.FunctionSymbol.LocalName);
            Assert.Equal("(ROOT)", error.StackFrame.Parent.FunctionSymbol.LocalName);
            Assert.Equal("Undefined macro/function 'ADD', found '<null>'", error.Message);
        }

        [Fact]
        public async Task ErrorPropagationFromCodeFunctionArgument()
        {
            var result = await EvalAsync(@"
(defun inc (x)
    (add x 1))
(inc two)
");
            var error = (LispError)result;
            Assert.Equal("test.lisp", error.SourceLocation.Value.FilePath);
            Assert.Equal(4, error.SourceLocation.Value.Start.Line);
            Assert.Equal(6, error.SourceLocation.Value.Start.Column);
            Assert.Equal("(ROOT)", error.StackFrame.FunctionSymbol.LocalName);
            Assert.Null(error.StackFrame.Parent);
            Assert.Equal("Symbol 'TWO' not found", error.Message);
        }

        [Fact]
        public async Task ErrorPropagationFromMultipleExpressions()
        {
            var result = await EvalAsync(@"
(+ 1 1)
(* 2 2)
(+ one 2)
(+ 3 3)
");
            var error = (LispError)result;
            Assert.Equal("test.lisp", error.SourceLocation.Value.FilePath);
            Assert.Equal(4, error.SourceLocation.Value.Start.Line);
            Assert.Equal(4, error.SourceLocation.Value.Start.Column);
            Assert.Equal("(ROOT)", error.StackFrame.FunctionSymbol.LocalName);
            Assert.Null(error.StackFrame.Parent);
            Assert.Equal("Symbol 'ONE' not found", error.Message);
        }

        [Fact]
        public async Task ErrorStackPropagationFromInitScript()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            var evalResult = await host.EvalAsync("*REPL*", "(+ 1 \"two\" 3)", executionState);
            var error = (LispError)evalResult.Value;
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
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            var evalResult = await host.EvalAsync("test-file.lisp", "(+ 1 2)", executionState);
            var result = evalResult.Value;
            Assert.Equal(3, ((LispInteger)result).Value);
            Assert.Null(result.SourceLocation);
        }

        [Fact]
        public async Task SourceLocationIsNotPropagatedFromComputedValues2()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            var evalResult = await host.EvalAsync("some-other-location.lisp", "(+ 1 2)", executionState);
            var result = evalResult.Value;
            Assert.Equal(3, ((LispInteger)result).Value);
            Assert.Null(result.SourceLocation);
        }

        [Fact]
        public async Task ErrorSourceLocationIsSetForAllStackFramesWhenItOccursInIfPredicate()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            LispError error = null;
            host.RootFrame.ErrorOccurred += (_, e) => error = e.Error;
            var evalResult = await host.EvalAsync("test-file.lisp", @"
(defun throw-error ()
    (error ""some-error""))
(if (throw-error) () ())
", executionState);
            Assert.NotNull(error);
            var stackTrace = error.StackFrame.ToString().Trim().Replace("\r", "");
            var expected = @"
  at ERROR: (, )
  at THROW-ERROR in 'test-file.lisp': (3, 12)
  at (ROOT) in 'init.lisp':
".Trim().Replace("\r", "");
            Assert.Contains(expected, stackTrace);
        }

        [Fact]
        public async Task ErrorSourceLocationIsSetForAllStackFramesWhenItOccursInCondPredicate()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            LispError error = null;
            host.RootFrame.ErrorOccurred += (_, e) => error = e.Error;
            var evalResult = await host.EvalAsync("test-file.lisp", @"
(defun throw-error ()
    (error ""some-error""))
(cond
    ((throw-error)  ())
    (t              ()))
", executionState);
            Assert.NotNull(error);
            var stackTrace = error.StackFrame.ToString().Trim().Replace("\r", "");
            var expected = @"
  at ERROR: (, )
  at THROW-ERROR in 'test-file.lisp': (3, 12)
  at (ROOT) in 'test-file.lisp': (5, 5)
".Trim().Replace("\r", "");
            Assert.Equal(expected, stackTrace);
        }

        [Fact]
        public async Task Conditional()
        {
            var host = await CreateHostAsync();

            // 'true' branch
            var result = await EvalAsync(@"
(if (< 1 2)
    ""one""
    ""two"")");
            Assert.Equal("one", ((LispString)result).Value);

            // 'false' branch
            result = await EvalAsync(@"
(if (< 2 1)
    ""one""
    ""two"")");
            Assert.Equal("two", ((LispString)result).Value);
        }

        [Fact]
        public async Task DotNotationLists()
        {
            var result = await EvalAndGetDisplayStringAsync("'(a . (b . (c)))");
            Assert.Equal("(A B C)", result);
        }

        [Fact]
        public async Task CircularLists()
        {
            var result = await EvalAsync("#1=(3 4 5 . #1#)");
            var list = (LispList)result;
            Assert.False(list.IsProperList);
            Assert.Equal(-3, list.Length); // not dictated anywhere, simply convention
            Assert.Equal(new LispInteger(3), list.Value);
            Assert.Equal("#1=(3 4 5 . #1#)", list.ToString());

            list = (LispList)(await EvalAsync("#1=(#1# . 2)"));
            Assert.False(list.IsProperList);
            Assert.Equal(-1, list.Length);
            Assert.Equal(new LispInteger(2), list.Next);
            Assert.True(ReferenceEquals(list, list.Value));
            Assert.Equal("#1=(#1# . 2)", list.ToString());

            list = (LispList)(await EvalAsync("#1=(2 3 #1#)"));
            Assert.True(list.IsProperList);
            Assert.Equal(-3, list.Length);
            Assert.Equal("#1=(2 3 #1#)", list.ToString());
        }

        [Fact]
        public async Task LetTest()
        {
            var result = await EvalAsync(@"
(defun avg (x y)
    (let ((sum (+ x y)))
        (/ sum 2.0)))
(avg 3.0 7.0)
");
            Assert.Equal(new LispFloat(5.0), result);
        }

        [Fact]
        public async Task MacroFunctionVariableNames()
        {
            // redefined function variable names shadow previous macro expansion values
            var host = await CreateHostAsync();
            var result = await EvalAsync(host, @"
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

        [Theory]
        [InlineData(true)]
        [InlineData(false)]
        public async Task SourceDefinedFunctionInvocationWithDiscardedBodyResult(bool useTailCalls)
        {
            var host = await CreateHostAsync(useTailCalls: useTailCalls, useInitScript: false);
            var executionState = LispExecutionState.CreateExecutionState(host.RootFrame, allowHalting: false);
            var result = await host.EvalAsync("test-input.lisp", @"
(defun test-method ()
    (kernel:+/2 1 1) ; the result of this expression should be discarded
    (kernel:+/2 2 2))
(test-method)
", executionState);
            Assert.Equal(LispEvaluationState.NonFatalHalt, result.State);
            Assert.Equal(new LispInteger(4), result.Value);
        }

        [Fact]
        public async Task TailCallWithCond()
        {
            var host = await CreateHostAsync(useTailCalls: true);
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
            var result = await EvalAsync(host, @"
(defun do-lots-of-tail-calls-with-cond ()
    (cond ((record-stack-depth) t)                                      ; done
          (t                    (do-lots-of-tail-calls-with-cond))))    ; keep going
(do-lots-of-tail-calls-with-cond)
");
            Assert.True(invocationCount >= 2, $"Must have been invoked at least twice, but was only invoked {invocationCount} time(s).");
            Assert.Equal(host.T, result);
        }

        [Fact]
        public async Task TailCallWithIf()
        {
            var host = await CreateHostAsync(useTailCalls: true);
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
            var result = await EvalAsync(host, @"
(defun do-lots-of-tail-calls-with-if ()
    (if (record-stack-depth)
        t                                   ; done
        (do-lots-of-tail-calls-with-if)))   ; keep going
(do-lots-of-tail-calls-with-if)
");
            Assert.True(invocationCount >= 2, $"Must have been invoked at least twice, but was only invoked {invocationCount} time(s).");
            Assert.Equal(host.T, result);
        }

        [Fact]
        public async Task InvokeBuiltInNamedFunctionReference()
        {
            var result = await EvalAsync(@"(funcall #'cons 'a 'b)");
            var expected = LispList.FromItemsImproper(LispSymbol.CreateFromString("COMMON-LISP-USER:A"), LispSymbol.CreateFromString("COMMON-LISP-USER:B"));
            Assert.Equal(expected, result);
        }

        [Fact]
        public async Task InvokeUserDefinedNamedFunctionReferenceThroughReaderMacro()
        {
            var result = await EvalAsync(@"
(defun add (a b)
    (+ a b))
(funcall #'add 2 3)
");
            Assert.Equal(new LispInteger(5), result);
        }

        [Fact]
        public async Task InvokeUserDefinedNamedFunctionReferenceThroughFunctionWrapper()
        {
            var result = await EvalAsync(@"
(defun add (a b)
    (+ a b))
(funcall (function add) 2 3)
");
            Assert.Equal(new LispInteger(5), result);
        }

        [Fact]
        public async Task InvokeNamedFunctionFromSymbol()
        {
            var result = await EvalAsync(@"
(setf plus-function #'+)
(funcall plus-function 2 3)
");
            Assert.Equal(new LispInteger(5), result);
        }

        [Fact]
        public async Task InvokeLambdaFromReference()
        {
            var result = await EvalAsync(@"
(funcall #'(lambda (n) (+ 1 n)) 2)
");
            Assert.Equal(new LispInteger(3), result);
        }

        [Fact]
        public async Task InvokeLambdaFromSymbol()
        {
            var result = await EvalAsync(@"
(setf inc-function #'(lambda (n) (+ 1 n)))
(funcall inc-function 2)
");
            Assert.Equal(new LispInteger(3), result);
        }

        [Fact]
        public async Task ApplyFunctionReference()
        {
            var result = await EvalAsync(@"
(apply #'+ '(2 3))
");
            Assert.Equal(5, ((LispInteger)result).Value);
        }

        [Fact]
        public async Task EnterAndReturnFunctionEvent()
        {
            var host = await CreateHostAsync();
            var sb = new StringBuilder();
            await EvalAsync(host, @"
(defun half (n) (* n 0.5))
(defun average (x y)
    (+ (half x) (half y)))
");
            host.RootFrame.FunctionEntered += (sender, e) => sb.AppendLine($"entered {e.Frame.FunctionSymbol.ToDisplayString(host.CurrentPackage)}");
            host.RootFrame.FunctionReturned += (sender, e) => sb.AppendLine($"returned from {e.Function.NameSymbol.ToDisplayString(host.CurrentPackage)} with {e.ReturnValue}");
            await EvalAsync(host, "(average 3 7)");
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
            Assert.Contains(expected, actual);
        }

        [Fact]
        public async Task LambdaWithLexicalClosure()
        {
            var result = await EvalAndGetDisplayStringAsync(@"
(setf words '((one uno) (two dos) (three tres)))
(defun my-assoc (key table)
  (find-if #'(lambda (entry)
    (equal key (first entry)))
  table))
(my-assoc 'two words)
");
            Assert.Equal("(TWO DOS)", result);
        }

        [Fact]
        public async Task LambdaCapture()
        {
            var host = await CreateHostAsync();
            await EvalAsync(host, @"
(defun make-greater-p (n)
    #'(lambda (x) (> x n)))
(setf pred (make-greater-p 3))
");
            Assert.Equal(host.Nil, await EvalAsync(host, "(funcall pred 2)"));
            Assert.Equal(host.T, await EvalAsync(host, "(funcall pred 5)"));
            Assert.Equal(new LispInteger(4), await EvalAsync(host, "(find-if pred '(2 3 4 5 6 7 8 9))"));
        }

        [Fact]
        public async Task LabelsFunctionDefinition()
        {
            var host = await CreateHostAsync();
            var result = await EvalAsync(host, @"
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
        public async Task LabelsRecursivelyCalled()
        {
            var result = await EvalAsync(@"
(labels ((fact (n acc)
            (if (<= n 0)
                acc
                (fact (- n 1) (* n acc)))))
        (fact 5 1))
");
            Assert.Equal(120, ((LispInteger)result).Value);
        }

        [Fact]
        public async Task FormatOutput()
        {
            var output = new StringWriter();
            var host = await CreateHostAsync(output: output);
            await EvalAsync(host, @"(format t ""hello"")");
            var result = NormalizeNewlines(output.ToString());
            Assert.Equal("hello", result);
        }

        [Fact]
        public async Task FormatOutputWithArgument()
        {
            var output = new StringWriter();
            var host = await CreateHostAsync(output: output);
            await EvalAsync(host, @"(format t ""hello ~S"" ""world"")");
            var result = NormalizeNewlines(output.ToString());
            Assert.Equal("hello \"world\"", result);
        }

        [Fact]
        public async Task MultipleCallsToFormat()
        {
            var output = new StringWriter();
            var host = await CreateHostAsync(output: output);
            await EvalAsync(host, @"
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
            var host = await CreateHostAsync();
            host.SetValue("TEST-STREAM", testStream);
            var result = await EvalAsync(host, @"
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
            var host = await CreateHostAsync(output: output);
            var result = await EvalAsync(host, @"
(terpri)
");
            Assert.True(result.IsNil());
            Assert.Equal("\n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public async Task TerPriFunctionWithStream()
        {
            var output = new StringWriter();
            var testStream = new LispTextStream("TEST-STREAM", TextReader.Null, output);
            var host = await CreateHostAsync();
            host.SetValue("TEST-STREAM", testStream);
            var result = await EvalAsync(host, @"
(terpri test-stream)
");
            Assert.True(result.IsNil());
            Assert.Equal("\n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public async Task Prin1Function()
        {
            var output = new StringWriter();
            var host = await CreateHostAsync(output: output);
            var result = await EvalAsync(host, @"
(prin1 ""abc"")
");
            Assert.Equal("abc", ((LispString)result).Value);
            Assert.Equal("\"abc\"\n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public async Task Prin1FunctionWithStream()
        {
            var output = new StringWriter();
            var testStream = new LispTextStream("TEST-STREAM", TextReader.Null, output);
            var host = await CreateHostAsync();
            host.SetValue("TEST-STREAM", testStream);
            var result = await EvalAsync(host, @"
(prin1 ""abc"" test-stream)
");
            Assert.Equal("abc", ((LispString)result).Value);
            Assert.Equal("\"abc\"\n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public async Task PrinCFunction()
        {
            var output = new StringWriter();
            var host = await CreateHostAsync(output: output);
            var result = await EvalAsync(host, @"
(princ ""abc"")
");
            Assert.Equal("abc", ((LispString)result).Value);
            Assert.Equal("abc\n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public async Task PrinCFunctionWithStream()
        {
            var output = new StringWriter();
            var testStream = new LispTextStream("TEST-STREAM", TextReader.Null, output);
            var host = await CreateHostAsync();
            host.SetValue("TEST-STREAM", testStream);
            var result = await EvalAsync(host, @"
(princ ""abc"" test-stream)
");
            Assert.Equal("abc", ((LispString)result).Value);
            Assert.Equal("abc\n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public async Task PrintFunction()
        {
            var output = new StringWriter();
            var host = await CreateHostAsync(output: output);
            var result = await EvalAsync(host, @"
(print ""abc"")
");
            Assert.Equal("abc", ((LispString)result).Value);
            Assert.Equal("\n\"abc\"\n \n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public async Task PrintFunctionWithStream()
        {
            var output = new StringWriter();
            var testStream = new LispTextStream("TEST-STREAM", TextReader.Null, output);
            var host = await CreateHostAsync();
            host.SetValue("TEST-STREAM", testStream);
            var result = await EvalAsync(host, @"
(print ""abc"" test-stream)
");
            Assert.Equal("abc", ((LispString)result).Value);
            Assert.Equal("\n\"abc\"\n \n", NormalizeNewlines(output.ToString()));
        }

        [Fact]
        public async Task LetBlocksEvaluateManyStatements()
        {
            var host = await CreateHostAsync();
            var result = await EvalAsync(host, @"
(let ((x 1))
    x
    (+ x 2)) ; this is the real result
");
            Assert.Equal(3, ((LispInteger)result).Value);
        }

        [Fact]
        public async Task BindRestArgumentsInFunction()
        {
            var host = await CreateHostAsync();
            await EvalAsync(host, @"
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
            var host = await CreateHostAsync();
            await EvalAsync(host, @"
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
            var host = await CreateHostAsync();
            await EvalAsync(host, @"
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
            var result = await EvalAsync(@"
(defun test (&optional (value (+ 1 1)))
    (+ 1 value))
(test)
");
            Assert.Equal(3, ((LispInteger)result).Value);
        }

        [Fact]
        public async Task KeywordSymbolsImmediatelyResolveToThemselves()
        {
            var host = await CreateHostAsync(useInitScript: false);
            var result = await EvalAsync(host, ":some-keyword");
            Assert.Equal(new LispResolvedSymbol("KEYWORD", "SOME-KEYWORD", isPublic: true), result);
        }

        [Fact]
        public async Task KeywordArgumentDefaultValuesAreEvaluated()
        {
            var result = await EvalAsync(@"
(defun test (&key (value (+ 1 1)))
    (+ 1 value))
(test)
");
            Assert.Equal(3, ((LispInteger)result).Value);
        }

        [Fact]
        public async Task AuxiliaryArgumentsAreComputed()
        {
            var result = await EvalAsync(@"
(defun test (the-list &aux (len (length the-list)))
    (+ 1 len))
(test '(1 2))
");
            Assert.Equal(3, ((LispInteger)result).Value);
        }

        [Fact]
        public async Task IntermediateValuesAreRemovedFromTheArgumentStack()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            var evalResult = await host.EvalAsync("test.lisp", @"
(+ 1 1)
(+ 2 2)
", executionState);
            Assert.Equal(4, ((LispInteger)evalResult.Value).Value);
            Assert.True(executionState.TryPopArgument(out var lastResult));
            Assert.Equal(4, ((LispInteger)lastResult).Value);
            Assert.False(executionState.TryPopArgument(out var shouldNotBeHere), $"Expected no more arguments, but found [{shouldNotBeHere}]");
        }

        [Fact]
        public async Task MacroExpansionWithFunctionInvocation()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            var evalResult = await host.EvalAsync("test.lisp", @"
(labels ((square (x) (* x x)))
    (square 2))
", executionState);
            Assert.Equal(4, ((LispInteger)evalResult.Value).Value);
            Assert.Null(executionState.StackFrame.GetValue(LispSymbol.CreateFromString("SQUARE").Resolve(host.CurrentPackage))); // no leakage
        }

        [Fact]
        public async Task MacroExpansionWithFunctionReferences()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            var evalResult = await host.EvalAsync("test.lisp", @"
(labels ((square (x) (* x x)))
    (car (mapcar #'square '(2))))
", executionState);
            Assert.Equal(4, ((LispInteger)evalResult.Value).Value);
            Assert.Null(executionState.StackFrame.GetValue(LispSymbol.CreateFromString("SQUARE").Resolve(host.CurrentPackage))); // no leakage
        }

        [Fact]
        public async Task IncFMacro()
        {
            var result = await EvalAsync(@"
(setf total 0)
(incf total) ; = 1
(incf total 10) ; = 11
total
");
            Assert.Equal(11, ((LispInteger)result).Value);
        }

        [Fact]
        public async Task DecFMacro()
        {
            var result = await EvalAsync(@"
(setf total 20)
(decf total) ; = 19
(decf total 10) ; = 9
total
");
            Assert.Equal(9, ((LispInteger)result).Value);
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
            var result = await EvalAsync(code);
            var error = (LispError)result;
            Assert.Equal("Symbol 'A' not found", error.Message);
            Assert.Equal(position, error.SourceLocation.Value.Start);
        }

        [Fact]
        public async Task LetSequentialBinding()
        {
            var result = await EvalAsync(@"
(let* ((a 10)
       (b (+ 2 a)))
    (+ a b))
");
            Assert.Equal(22, ((LispInteger)result).Value);
        }

        [Theory]
        [InlineData("(let ((x 42) (y 43)) (+ x y))", "(APPLY (LAMBDA (X Y) (+ X Y)) (LIST (QUOTE 42) (QUOTE 43)))")]
        [InlineData("(let* ((x 42) (y 43)) (+ x y))", "(APPLY (LAMBDA () (SETF X 42) (SETF Y 43) (+ X Y)) (LIST))")]
        public async Task LetMacroExpansion(string code, string expected)
        {
            var host = await CreateHostAsync();
            var items = await host.ParseAllAsync(code);
            var letBlock = items.Single();
            var letList = Assert.IsType<LispList>(letBlock);
            var list = letList.ToList();
            var args = list.Skip(1).ToArray();
            var bindSequentially = list[0].ToString() == "LET*";
            var result = LispDefaultContext.Let(args, bindSequentially);
            var actual = result.ToString();
            Assert.Equal(expected, actual);
        }

        [Fact]
        public async Task ReadStreamObjectsThenDefaultEofMarker()
        {
            var input = new StringReader("(abc 2)\n14");
            var stream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            var obj = LispList.FromItems(new LispUnresolvedSymbol("READ"), stream);

            var evalResult = await host.EvalAsync(obj, executionState);
            var list = ((LispList)evalResult.Value).ToList();
            Assert.Equal(2, list.Count);
            Assert.Equal("ABC", ((LispSymbol)list[0]).LocalName);
            Assert.Equal(2, ((LispInteger)list[1]).Value);

            evalResult = await host.EvalAsync(obj, executionState);
            var number = (LispInteger)evalResult.Value;
            Assert.Equal(14, number.Value);

            evalResult = await host.EvalAsync(obj, executionState);
            var eof = (LispError)evalResult.Value;
            Assert.Equal("EOF", eof.Message);
        }

        [Fact]
        public async Task ReadStreamObjectsThenCustomEofMarker()
        {
            var input = new StringReader("14");
            var stream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();

            var evalResult = await host.EvalAsync(LispList.FromItems(new LispUnresolvedSymbol("READ"), stream), executionState);
            var number = (LispInteger)evalResult.Value;
            Assert.Equal(14, number.Value);

            evalResult = await host.EvalAsync(LispList.FromItems(new LispUnresolvedSymbol("READ"), stream, host.Nil, new LispInteger(-54), host.Nil), executionState);
            var eof = (LispInteger)evalResult.Value;
            Assert.Equal(-54, eof.Value);
        }

        [Fact]
        public async Task ReadFunctionDefaultEofMarker()
        {
            var input = new StringReader("14");
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();

            host.SetValue(testStream.Name, testStream);
            var evalResult = await host.EvalAsync("test.lisp", "(list (read test-stream) (read test-stream))", executionState);
            var result = evalResult.Value; // EOF propagates to the top
            Assert.True(result.IsEof(), $"Expected EOF, found {result}");
        }

        [Fact]
        public async Task ReadFunctionCustomEofMarker()
        {
            var input = new StringReader("14");
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            host.SetValue(testStream.Name, testStream);
            var evalResult = await host.EvalAsync("test.lisp", "(list (read test-stream) (read test-stream nil -54))", executionState);
            var resultList = ((LispList)evalResult.Value).ToList();
            Assert.Equal(2, resultList.Count);
            Assert.Equal(14, ((LispInteger)resultList[0]).Value);
            Assert.Equal(-54, ((LispInteger)resultList[1]).Value);
        }

        [Fact]
        public async Task InterleavedReadOperationsUseTheirOwnTextStreams()
        {
            var host = await CreateHostAsync(useInitScript: false);
            var executionState = host.CreateExecutionState();
            LispObject innerObject = null;
            host.AddFunction(new LispResolvedSymbol("SOME-PACKAGE", "INNER-FUNCTION", isPublic: true), async (_host, _executionState, _args, _cancellationToken) =>
            {
                var innerReadResult = await host.EvalAsync("test.lisp", "123", executionState);
                innerObject = innerReadResult.Value;
                return host.Nil;
            });
            var outerReadResult = await host.EvalAsync("test.lisp", @"
(some-package:inner-function)
456
", executionState);
            var outerObject = outerReadResult.Value;
            Assert.Equal(new LispInteger(123), innerObject);
            Assert.Equal(new LispInteger(456), outerObject);
        }

        [Fact]
        public async Task Push()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            var result = await host.EvalAsync("test.lisp", @"
(setf my-stack ())
(setf a (push 1 my-stack))
(setf b (push 2 my-stack))
", executionState);
            
            Assert.Equal("(2 1)", executionState.StackFrame.GetValue(LispSymbol.CreateFromString("MY-STACK").Resolve(host.CurrentPackage)).ToString());
            Assert.Equal("(1)", executionState.StackFrame.GetValue(LispSymbol.CreateFromString("A").Resolve(host.CurrentPackage)).ToString());
            Assert.Equal("(2 1)", executionState.StackFrame.GetValue(LispSymbol.CreateFromString("B").Resolve(host.CurrentPackage)).ToString());
        }

        [Fact]
        public async Task Pop()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            var evalResult = await host.EvalAsync("test.lisp", @"
(setf my-stack '(2 1))
(setf a (pop my-stack))
(setf b (pop my-stack))
", executionState);
            Assert.Equal("()", executionState.StackFrame.GetValue(LispSymbol.CreateFromString("MY-STACK").Resolve(host.CurrentPackage)).ToString());
            Assert.Equal("2", executionState.StackFrame.GetValue(LispSymbol.CreateFromString("A").Resolve(host.CurrentPackage)).ToString());
            Assert.Equal("1", executionState.StackFrame.GetValue(LispSymbol.CreateFromString("B").Resolve(host.CurrentPackage)).ToString());
        }

        [Fact]
        public async Task WhenT()
        {
            var result = await EvalAsync(@"
(defun evals-to-t () t)
(when (evals-to-t)
    (+ 1 1)
    (+ 3 3))
");
            EnsureNotError(result);
            Assert.Equal(6, ((LispInteger)result).Value);
        }

        [Fact]
        public async Task WhenNil()
        {
            var result = await EvalAsync(@"
(defun evals-to-nil () ())
(when (evals-to-nil)
    (+ 1 1)
    (+ 3 3))
");
            EnsureNotError(result);
            Assert.True(result.IsNil(), $"Expected nil, but got: {result}");
        }

        [Fact]
        public async Task UnlessT()
        {
            var result = await EvalAsync(@"
(defun evals-to-t () t)
(unless (evals-to-t)
    (+ 1 1)
    (+ 3 3))
");
            EnsureNotError(result);
            Assert.True(result.IsNil(), $"Expected nil, but got: {result}");
        }

        [Fact]
        public async Task UnlessNil()
        {
            var result = await EvalAsync(@"
(defun evals-to-nil () ())
(unless (evals-to-nil)
    (+ 1 1)
    (+ 3 3))
");
            EnsureNotError(result);
            Assert.Equal(6, ((LispInteger)result).Value);
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
            var result = await EvalAndGetDisplayStringAsync(@"
(setf the-list '((a b c) 2 3))
(push 11 (car the-list))
the-list
");
            Assert.Equal("((11 A B C) 2 3)", result.ToString());
        }

        [Fact]
        public async Task GeneralizedVariableSetterDoesNotLivePastPush()
        {
            var result = await EvalAndGetDisplayStringAsync(@"
(setf the-list '((a b c) 2 3))
(setf head-value (car the-list))
(push 11 head-value)
(cons head-value the-list)
");
            Assert.Equal("((11 A B C) (A B C) 2 3)", result);
        }

        [Fact]
        public async Task GeneralizedVariablesWithPop()
        {
            var result = await EvalAndGetDisplayStringAsync(@"
(setf the-list '((a b c) 1 2 3))
(pop (car the-list))
the-list
");
            Assert.Equal("((B C) 1 2 3)", result.ToString());
        }

        [Fact]
        public async Task GeneralizedVariableSetterDoesNotLivePastPop()
        {
            var result = await EvalAndGetDisplayStringAsync(@"
(setf the-list '((a b c) 1 2 3))
(setf head-value (car the-list))
(pop head-value)
(cons head-value the-list)
");
            Assert.Equal("((B C) (A B C) 1 2 3)", result);
        }

        [Fact]
        public async Task NconcWithNoArguments()
        {
            var result = await EvalAndGetDisplayStringAsync("(nconc)");
            Assert.Equal("()", result);
        }

        [Fact]
        public async Task NconcWithFirstParameterAsAList()
        {
            // also testing more than 2 parameters
            var result = await EvalAndGetDisplayStringAsync(@"
(setf x '(a b c))
(setf y '(d e f))
(setf z '(g h i))
(nconc x y z)
(list x y z)
");
            Assert.Equal("((A B C D E F G H I) (D E F G H I) (G H I))", result);
        }

        [Fact]
        public async Task NconcWithFirstParameterNil()
        {
            var result = await EvalAndGetDisplayStringAsync(@"
(setf x nil)
(setf y '(d e f))
(nconc x y)
(list x y)
");
            Assert.Equal("(() (D E F))", result);
        }

        [Fact]
        public async Task Nsubst()
        {
            var result = await EvalAndGetDisplayStringAsync(@"
(setf l '(a b c d a b c d))
(nsubst 'bee 'b l)
l
");
            Assert.Equal("(A BEE C D A BEE C D)", result);
        }

        [Fact]
        public async Task PackageGlobalVariableIsUpdatedWithHostProperty()
        {
            var host = await CreateHostAsync(useInitScript: false);
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
            var host = await CreateHostAsync(useInitScript: false);
            host.AddPackage("TEST-PACKAGE", new[] { host.CurrentPackage });
            var package = host.GetValue<LispPackage>("*PACKAGE*");
            Assert.Equal("COMMON-LISP", package.Name);

            var executionState = host.CreateExecutionState();
            await host.EvalAsync("test.lisp", "(in-package :test-package)", executionState);
            package = host.GetValue<LispPackage>("*PACKAGE*");
            Assert.Equal("TEST-PACKAGE", package.Name);
        }

        [Fact]
        public async Task CurrentPackageIsUpdatedWithInPackageFunctionWithString()
        {
            var host = await CreateHostAsync();
            host.AddPackage("TEST-PACKAGE", new[] { host.CurrentPackage });
            var package = host.GetValue<LispPackage>("*PACKAGE*");
            Assert.Equal("COMMON-LISP-USER", package.Name);

            var executionState = host.CreateExecutionState();
            await host.EvalAsync("test.lisp", "(in-package \"TEST-PACKAGE\")", executionState);
            package = host.GetValue<LispPackage>("*PACKAGE*");
            Assert.Equal("TEST-PACKAGE", package.Name);
        }

        [Fact]
        public async Task PackageIsCreatedWithDefPackageWithKeyword()
        {
            var host = await CreateHostAsync(useInitScript: false);
            var executionState = host.CreateExecutionState();
            await host.EvalAsync("test.lisp", "(defpackage :test-package) (in-package :test-package)", executionState);
            var package = host.GetValue<LispPackage>("*PACKAGE*");
            Assert.Equal("TEST-PACKAGE", package.Name);
        }

        [Fact]
        public async Task PackageIsCreatedWithDefPackageWithString()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            await host.EvalAsync("test.lisp", "(defpackage \"TEST-PACKAGE\") (in-package :test-package)", executionState);
            var package = host.GetValue<LispPackage>("*PACKAGE*");
            Assert.Equal("TEST-PACKAGE", package.Name);
        }

        [Fact]
        public async Task PackagesCanInheritSymbols()
        {
            var host = await CreateHostAsync(useInitScript: false);
            var executionState = host.CreateExecutionState();
            var evalResult = await host.EvalAsync("test.lisp", @"
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
", executionState);
            Assert.Equal("(1 2 3)", evalResult.Value.ToString());
        }

        [Fact]
        public async Task UnsetSymbolCanBeRetrievedFromNonExistantPackage()
        {
            var gotUnsetSymbol = false;
            var host = await CreateHostAsync(useInitScript: false, getUntrackedValue: resolvedSymbol =>
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
            host = await CreateHostAsync(useInitScript: false, getUntrackedValue: resolvedSymbol =>
            {
                if (resolvedSymbol.Value == $"{host?.CurrentPackage.Name}:not-a-symbol")
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
            var host = await CreateHostAsync(useInitScript: false, getUntrackedValue: resolvedSymbol =>
            {
                if (resolvedSymbol.ToString().ToLowerInvariant().Contains("some-int"))
                {
                    gotUnsetSymbol = true;
                }

                return null;
            });
            host.SetValue("some-int", new LispInteger(42));
            var result = host.GetValue("some-int");
            Assert.False(gotUnsetSymbol);
            Assert.Equal(new LispInteger(42), result);
        }

        [Fact]
        public async Task SetUntrackedValueShortCircuitsValueSettingInNonExistantPackage()
        {
            var setValue = false;
            var host = await CreateHostAsync(useInitScript: false, trySetUntrackedValue: (resolvedSymbol, value) =>
            {
                if (resolvedSymbol.Value == "not-a-package:not-a-symbol" &&
                    value is LispInteger i &&
                    i == new LispInteger(42))
                {
                    setValue = true;
                    return true;
                }

                return false;
            });
            host.SetValue("not-a-package:not-a-symbol", new LispInteger(42));
            Assert.True(setValue);
            Assert.Null(host.RootFrame.GetPackage("not-a-package"));
            Assert.Null(host.GetValue("not-a-package:not-a-symbol"));
        }

        [Fact]
        public async Task SetUntrackedValueShortCircuitsValueSettingInExistingPackage()
        {
            var setValue = false;
            LispHost host = null;
            host = await CreateHostAsync(useInitScript: false, trySetUntrackedValue: (resolvedSymbol, value) =>
            {
                if (resolvedSymbol.Value == $"{host?.CurrentPackage.Name}:not-a-symbol" &&
                    value is LispInteger i &&
                    i == new LispInteger(42))
                {
                    setValue = true;
                    return true;
                }

                return false;
            });
            host.SetValue($"{host.CurrentPackage.Name}:not-a-symbol", new LispInteger(42));
            Assert.True(setValue);
            Assert.NotNull(host.RootFrame.GetPackage(host.CurrentPackage.Name));
            Assert.Null(host.GetValue($"{host.CurrentPackage.Name}:not-a-symbol"));
        }
    }
}
