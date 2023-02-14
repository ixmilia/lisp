using System;
using System.Linq;
using System.Threading.Tasks;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class ExecutionControlTests : TestBase
    {
        [Fact]
        public async Task HaltExecutionOnFunctionEnter()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            host.RootFrame.FunctionEntered += (s, e) =>
            {
                if (e.Frame.FunctionSymbol.Value == "COMMON-LISP-USER:INNER-FUNCTION")
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync("test.lisp", @"
(defun inner-function ()
    42)
(defun outer-function ()
    (inner-function))
(outer-function)
", executionState);
            Assert.False(executionState.IsExecutionComplete);
            evalResult = await host.EvalContinueAsync(executionState);
            Assert.Equal(42, ((LispInteger)evalResult.Value).Value);
        }

        [Fact]
        public async Task HaltExecutionOnFunctionReturn()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            bool sentinelHit = false;
            host.AddFunction("SENTINEL", (host, executionState, args, _cancellationToken) =>
            {
                sentinelHit = true;
                return Task.FromResult<LispObject>(new LispInteger(54));
            });
            LispObject capturedReturnValue = null;
            host.RootFrame.FunctionReturned += (s, e) =>
            {
                if (e.Frame.FunctionSymbol.Value == "COMMON-LISP-USER:INNER-FUNCTION")
                {
                    e.HaltExecution = true;
                    capturedReturnValue = e.ReturnValue;
                }
            };
            var evalResult = await host.EvalAsync("test.lisp", @"
(defun inner-function ()
    42)
(defun outer-function ()
    (inner-function)
    (sentinel))
(outer-function)
", executionState);
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal(42, ((LispInteger)capturedReturnValue).Value);
            Assert.Equal(42, ((LispInteger)evalResult.Value).Value);
            Assert.False(sentinelHit);
            evalResult = await host.EvalContinueAsync(executionState);
            Assert.Equal(54, ((LispInteger)evalResult.Value).Value);
            Assert.True(sentinelHit);
        }

        [Fact]
        public async Task HaltExecutionOnMacroExpansion()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            host.AddMacro("FOURTY-TWO", (host, executionState, args, _cancellationToken) =>
            {
                return Task.FromResult<LispObject>(new LispInteger(42));
            });
            host.RootFrame.MacroExpanded += (s, e) =>
            {
                if (e.Macro.NameSymbol.LocalName == "FOURTY-TWO")
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync("test.lisp", @"
(fourty-two)
(+ 1 2)
", executionState);
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: 42", executionState.PeekOperation().ToString());

            evalResult = await host.EvalContinueAsync(executionState);
            Assert.Equal(3, ((LispInteger)evalResult.Value).Value);
        }

        [Fact]
        public async Task HaltExecutionOnExpressionEvaluation()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            bool hitBreakpoint = false;
            bool sentinelHit = false;
            host.AddFunction("SENTINEL", (host, executionState, args, _cancellationToken) =>
            {
                sentinelHit = true;
                return Task.FromResult<LispObject>(new LispInteger(54));
            });
            host.RootFrame.EvaluatingExpression += (s, e) =>
            {
                if (!hitBreakpoint &&
                    e.Expression is LispList list &&
                    list.Value is LispSymbol symbol &&
                    symbol.LocalName == "+")
                {
                    hitBreakpoint = true;
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync("test.lisp", @"
(defun inner-function ()
    (+ 40 2))
(defun outer-function ()
    (inner-function)
    (sentinel))
(outer-function)
", executionState);
            Assert.True(hitBreakpoint);
            Assert.False(executionState.IsExecutionComplete);
            Assert.False(sentinelHit);
            evalResult = await host.EvalContinueAsync(executionState);
            Assert.Equal(54, ((LispInteger)evalResult.Value).Value);
            Assert.True(sentinelHit);
        }

        [Fact]
        public async Task HaltExecutionAfterSimpleExpressionEvaluation()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            host.RootFrame.EvaluatedExpression += (s, e) =>
            {
                if (e.Expression.ToString() == "2" &&
                    e.Result is LispInteger i &&
                    i.Value == 2)
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync("test.lisp", @"
(+ (* 2 3) (/ 12 4))
", executionState);
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal(2, ((LispInteger)evalResult.Value).Value);
            evalResult = await host.EvalContinueAsync(executionState);
            Assert.Equal(9, ((LispInteger)evalResult.Value).Value);
        }

        [Fact]
        public async Task HaltExecutionAfterInvokeExpressionEvaluation()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            host.RootFrame.EvaluatedExpression += (s, e) =>
            {
                if (e.Expression.ToString() == "(COMMON-LISP:* 2 3)" &&
                    e.Result is LispInteger i &&
                    i.Value == 6)
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync("test.lisp", @"
(+ (* 2 3) (/ 10 2))
", executionState);
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal(6, ((LispInteger)evalResult.Value).Value);
            evalResult = await host.EvalContinueAsync(executionState);
            Assert.Equal(11, ((LispInteger)evalResult.Value).Value);
        }

        [Fact]
        public async Task HaltExecutionOnEvaluatingFunctionArgument()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            var hitBreakpoint = false;
            host.RootFrame.EvaluatingExpression += (s, e) =>
            {
                if (!hitBreakpoint &&
                    e.Expression is LispList list &&
                    list.ToDisplayString(host.CurrentPackage) == "(* 2 2)")
                {
                    hitBreakpoint = true;
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync("test.lisp", @"
(+ (* 2 2) (* 3 3))
", executionState);
            Assert.True(hitBreakpoint);
            Assert.False(executionState.IsExecutionComplete);
            evalResult = await host.EvalContinueAsync(executionState);
            Assert.Equal(13, ((LispInteger)evalResult.Value).Value);
        }

        [Fact]
        public async Task ExecutionCannotBeHaltedWhenEvaluatingFromWithinANativeFunction()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            host.AddFunction("NATIVE-FUNCTION", async (host, executionState, args, _cancellationToken) =>
            {
                var result = await host.EvalAtStackFrameAsync(executionState.StackFrame, LispList.FromEnumerable(new LispObject[] { LispSymbol.CreateFromString("*"), new LispInteger(2), new LispInteger(2) }));
                return result;
            });
            var hitBreakpoint = false;
            host.RootFrame.EvaluatingExpression += (s, e) =>
            {
                if (!hitBreakpoint &&
                    e.Expression is LispList list &&
                    list.ToString() == "(* 2 2)")
                {
                    hitBreakpoint = true;
                    e.HaltExecution = true; // this should not be honored
                }
            };
            var evalResult = await host.EvalAsync("test.lisp", "(native-function)", executionState);
            Assert.True(hitBreakpoint);
            Assert.Equal(4, ((LispInteger)evalResult.Value).Value);
        }

        [Fact]
        public async Task ExecutionCannotBeHaltedWhenEvaluatingFromWithinANativeMacro()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            host.AddMacro("NATIVE-FUNCTION", async (host, executionState, args, _cancellationToken) =>
            {
                var result = await host.EvalAtStackFrameAsync(executionState.StackFrame, LispList.FromEnumerable(new LispObject[] { LispSymbol.CreateFromString("*"), new LispInteger(2), new LispInteger(2) }));
                return result;
            });
            var hitBreakpoint = false;
            host.RootFrame.EvaluatingExpression += (s, e) =>
            {
                if (!hitBreakpoint &&
                    e.Expression is LispList list &&
                    list.ToString() == "(* 2 2)")
                {
                    hitBreakpoint = true;
                    e.HaltExecution = true; // this should not be honored
                }
            };
            var evalResult = await host.EvalAsync("test.lisp", "(native-function)", executionState);
            Assert.True(hitBreakpoint);
            Assert.Equal(4, ((LispInteger)evalResult.Value).Value);
        }

        [Fact]
        public async Task ExecutionCanBeHaltedAfterSettingAValue()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            host.RootFrame.ValueSet += (s, e) =>
            {
                if (e.Symbol.Value == "COMMON-LISP-USER:THE-ANSWER" &&
                    e.Value is LispInteger i &&
                    i.Value == 42)
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync("test.lisp", @"
(+ 1 2)
(setf the-answer (+ 40 2))
(+ the-answer 2)
", executionState);
            evalResult = await host.EvalContinueAsync(executionState);
            Assert.Equal(44, ((LispInteger)evalResult.Value).Value);
        }

        [Fact]
        public async Task ExecutionCanBeHaltedWhenAnErrorIsExplicitlyRaised()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            LispError capturedError = null;
            var isExecutionCompleteAtError = false;
            host.RootFrame.ErrorOccurred += (s, e) =>
            {
                capturedError = e.Error;
                isExecutionCompleteAtError = executionState.IsExecutionComplete;
            };
            var evalResult = await host.EvalAsync("test.lisp", @"
(defun test-method ()
    (error ""test error""))
(test-method)
", executionState);
            // errors _always_ halt execution
            Assert.False(isExecutionCompleteAtError);
            Assert.Equal("test error", ((LispError)evalResult.Value).Message);
            Assert.Equal("test error", capturedError.Message);
            Assert.True(ReferenceEquals(capturedError, evalResult.Value));
        }

        [Fact]
        public async Task ExecutionCanBeHaltedWhenAnErrorIsNaturallyEncountered()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            LispError capturedError = null;
            var isExecutionCompleteAtError = false;
            host.RootFrame.ErrorOccurred += (s, e) =>
            {
                capturedError = e.Error;
                isExecutionCompleteAtError = executionState.IsExecutionComplete;
            };
            var evalResult = await host.EvalAsync("test.lisp", @"
(defun test-method ()
    (not-a-method))
(test-method)
", executionState);
            // errors _always_ halt execution
            Assert.False(isExecutionCompleteAtError);
            Assert.Equal("Undefined macro/function 'NOT-A-METHOD', found '<null>'", ((LispError)evalResult.Value).Message);
            Assert.Equal("Undefined macro/function 'NOT-A-METHOD', found '<null>'", capturedError.Message);
            Assert.True(ReferenceEquals(capturedError, evalResult.Value));
        }

        [Fact]
        public async Task HaltingOnErrorOccuredDoesNotHappenMoreThanOnceForTheSameError()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            LispError capturedError = null;
            var isExecutionCompleteAtError = false;
            host.RootFrame.ErrorOccurred += (s, e) =>
            {
                if (ReferenceEquals(capturedError, e.Error))
                {
                    throw new Exception($"Error has already been handled: {e.Error}");
                }

                capturedError = e.Error;
                isExecutionCompleteAtError = executionState.IsExecutionComplete;
            };
            var evalResult = await host.EvalAsync("test.lisp", @"
(defun test-method ()
    (error ""test error""))
(test-method)
", executionState);
            // ensure we halted the first time on the error
            Assert.False(isExecutionCompleteAtError);
            Assert.IsType<LispError>(evalResult.Value);
            Assert.True(ReferenceEquals(capturedError, evalResult.Value));

            // continue to the end (i.e., let the error bubble up, but don't halt again)
            await host.EvalContinueAsync(executionState);
            Assert.True(executionState.IsExecutionComplete);
        }

        [Theory]
        [InlineData(true)]
        [InlineData(false)]
        public async Task ExecutionCanStepOver(bool useTailCalls)
        {
            var host = await CreateHostAsync(useTailCalls: useTailCalls);
            var executionState = host.CreateExecutionState();
            var hasHalted = false;
            host.RootFrame.EvaluatingExpression += (s, e) =>
            {
                if (!hasHalted && e.Expression.ToString() == "(COMMON-LISP:+ 2 (COMMON-LISP:- 5 3))")
                {
                    e.HaltExecution = true;
                    hasHalted = true;
                }
            };
            var evalResult = await host.EvalAsync("test.lisp", @"
(defun test-method ()
    (+ 1 1)
    (+ 2 (- 5 3))       ; initial halt here
    (+ 3 3))            ; first step over gets here
(test-method)           ; second step over gets here
(+ 4 4)                 ; third step over (aka, step out) gets here
", executionState);
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: (COMMON-LISP:+ 2 (COMMON-LISP:- 5 3))", executionState.PeekOperation().ToString());

            await host.StepOverAsync(executionState);
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: (COMMON-LISP:+ 3 3)", executionState.PeekOperation().ToString());

            await host.StepOverAsync(executionState); // end of function, this was really a step out
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: (COMMON-LISP:+ 4 4)", executionState.PeekOperation().ToString());
            Assert.Equal("test.lisp: [(7, 1)-(7, 8))", executionState.PeekCurrentExpression().SourceLocation.ToString());

            evalResult = await host.EvalContinueAsync(executionState);
            Assert.Equal("a: pop", executionState.PeekOperation().ToString());
            Assert.Equal(8, ((LispInteger)evalResult.Value).Value);
            await host.EvalContinueAsync(executionState);
            Assert.True(executionState.IsExecutionComplete);
        }

        [Fact]
        public async Task ExecutionStepOverWhenNextOperationIsNotAnExpression()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            host.RootFrame.FunctionReturned += (s, e) =>
            {
                if (e.Frame.FunctionSymbol.Value == "COMMON-LISP-USER:TEST-METHOD")
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync("test.lisp", @"
(defun test-method ()
    (+ 1 2))
(test-method)
(+ 4 5)
", executionState);
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal(3, ((LispInteger)evalResult.Value).Value);
            Assert.Null(executionState.PeekCurrentExpression());

            await host.StepOverAsync(executionState); // nothing to step over; it's really a step out
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: (COMMON-LISP:+ 4 5)", executionState.PeekOperation().ToString());

            evalResult = await host.EvalContinueAsync(executionState);
            Assert.Equal(9, ((LispInteger)evalResult.Value).Value);
        }

        [Theory]
        [InlineData(true)]
        [InlineData(false)]
        public async Task ExecutionCanStepIn(bool useTailCalls)
        {
            var host = await CreateHostAsync(useTailCalls: useTailCalls);
            var executionState = host.CreateExecutionState();
            host.AddFunction("NATIVE-FUNCTION", (host, executionState, args, _cancellationToken) =>
            {
                return Task.FromResult(host.T);
            });
            var hasHalted = false;
            host.RootFrame.EvaluatingExpression += (s, e) =>
            {
                if (!hasHalted && e.Expression.ToString() == "(COMMON-LISP-USER:TEST-METHOD)")
                {
                    e.HaltExecution = true;
                    hasHalted = true;
                }
            };
            var evalResult = await host.EvalAsync("test.lisp", @"
(defun test-method ()
    (native-function)
    (+ 1 1))
(test-method) ; initial halt here
", executionState);
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: (COMMON-LISP-USER:TEST-METHOD)", executionState.PeekOperation().ToString());

            await host.StepInAsync(executionState);
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: (COMMON-LISP-USER:NATIVE-FUNCTION)", executionState.PeekOperation().ToString());

            await host.StepInAsync(executionState); // can't step in to a native function; this is really a step over
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: (COMMON-LISP:+ 1 1)", executionState.PeekOperation().ToString());
        }

        [Fact]
        public async Task ExecutionStepInWhenNextOperationIsNotAnExpression()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            host.RootFrame.FunctionReturned += (s, e) =>
            {
                if (e.Frame.FunctionSymbol.Value == "COMMON-LISP-USER:TEST-METHOD")
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync("test.lisp", @"
(defun test-method ()
    (+ 1 2))
(test-method)
(+ 4 5)
", executionState);
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal(3, ((LispInteger)evalResult.Value).Value);
            Assert.Null(executionState.PeekCurrentExpression());

            await host.StepInAsync(executionState); // nothing to step in to, so it's really step out _then_ step in
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: 4", executionState.PeekOperation().ToString());

            evalResult = await host.EvalContinueAsync(executionState);
            Assert.Equal(9, ((LispInteger)evalResult.Value).Value);
        }

        [Theory]
        [InlineData(true)]
        [InlineData(false)]
        public async Task ExecutionCanStepOut(bool useTailCalls)
        {
            var host = await CreateHostAsync(useTailCalls: useTailCalls);
            var executionState = host.CreateExecutionState();
            var hasHalted = false;
            var hitSomeOtherFunction = false;
            host.AddFunction("SOME-OTHER-FUNCTION", (_host, executionState, args, cancellationToken) =>
            {
                hitSomeOtherFunction = true;
                return Task.FromResult(_host.Nil);
            });
            host.RootFrame.EvaluatingExpression += (s, e) =>
            {
                if (!hasHalted && e.Expression.ToString() == "(COMMON-LISP:+ 1 1)")
                {
                    e.HaltExecution = true;
                    hasHalted = true;
                }
            };
            var evalResult = await host.EvalAsync("test.lisp", @"
(defun test-method ()
    (+ 1 1) ; initial halt here
    (+ 2 2))
(test-method)
(some-other-function)
8
", executionState);
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: (COMMON-LISP:+ 1 1)", executionState.PeekOperation().ToString());

            await host.StepOutAsync(executionState);
            Assert.False(executionState.IsExecutionComplete);
            Assert.False(hitSomeOtherFunction);
            Assert.Equal("s: (COMMON-LISP-USER:SOME-OTHER-FUNCTION)", executionState.PeekOperation().ToString());

            evalResult = await host.StepOutAsync(executionState); // can't step out at the root level; this was really a run to end
            Assert.True(hitSomeOtherFunction);
            Assert.Equal(8, ((LispInteger)evalResult.Value).Value);
        }

        [Fact]
        public async Task ExecutionStepOutWhenNextOperationIsNotAnExpression()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            host.RootFrame.FunctionReturned += (s, e) =>
            {
                if (e.Frame.FunctionSymbol.Value == "COMMON-LISP-USER:TEST-METHOD")
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync("test.lisp", @"
(defun test-method ()
    (+ 1 2))
(test-method)
(+ 4 5)
", executionState);
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal(3, ((LispInteger)evalResult.Value).Value);
            Assert.Null(executionState.PeekCurrentExpression());

            evalResult = await host.StepOutAsync(executionState); // stepping out here steps out of everything
            Assert.Equal(9, ((LispInteger)evalResult.Value).Value);
        }

        [Fact]
        public async Task EvaluationCanBeHaltedInsideSetFValue()
        {
            var enteredGetValue = false;
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            host.RootFrame.FunctionEntered += (s, e) =>
            {
                if (e.Frame.FunctionSymbol.Value == "COMMON-LISP-USER:GET-VALUE")
                {
                    enteredGetValue = true;
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync("test.lisp", @"
(defun get-value ()
    42)
(setf x (get-value))", executionState);
            Assert.False(executionState.IsExecutionComplete);
            Assert.True(enteredGetValue);
            Assert.Null(host.GetValue("X"));
        }

        [Fact]
        public async Task EvaluationCanBeHaltedInsideSetFDestination()
        {
            var enteredSetValue = false;
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            host.RootFrame.EvaluatedExpression += (s, e) =>
            {
                if (e.Expression.ToDisplayString(host.CurrentPackage) == "(CAR X)")
                {
                    enteredSetValue = true;
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync("test.lisp", @"
(setf x '(1 2 3))
(setf (car x) 11)", executionState);
            Assert.False(executionState.IsExecutionComplete);
            Assert.True(enteredSetValue);
            Assert.Equal("(1 2 3)", host.GetValue("X").ToString());
        }

        [Fact]
        public async Task HandlerCaseCanInterceptAnError()
        {
            LispError capturedError = null;
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            host.AddFunction("MY-FUNCTION", (_host, executionState, args, cancellationToken) =>
            {
                capturedError = (LispError)args.Single();
                return Task.FromResult(host.Nil);
            });
            var evalResult = await host.EvalAsync("test.lisp", @"
(handler-case (error ""some error"")
    (error (e) (progn (my-function e)
                      2)))
", executionState);
            EnsureNotError(evalResult.Value);
            Assert.Equal(2, ((LispInteger)evalResult.Value).Value);
            Assert.NotNull(capturedError);
            Assert.Equal("some error", capturedError.Message);
        }

        [Fact]
        public async Task HandlerCaseCanInterceptAnErrorAfterSkippingALevel()
        {
            LispError capturedError = null;
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            host.AddFunction("MY-FUNCTION", (_host, executionState, args, cancellationToken) =>
            {
                capturedError = (LispError)args.Single();
                return Task.FromResult(host.Nil);
            });
            var evalResult = await host.EvalAsync("test.lisp", @"
(handler-case
    (handler-case (error ""some error"")
        (not-handled-here () ()))
    (error (e) (progn (my-function e)
                      2)))
", executionState);
            EnsureNotError(evalResult.Value);
            Assert.Equal(2, ((LispInteger)evalResult.Value).Value);
            Assert.NotNull(capturedError);
            Assert.Equal("some error", capturedError.Message);
        }
    }
}
