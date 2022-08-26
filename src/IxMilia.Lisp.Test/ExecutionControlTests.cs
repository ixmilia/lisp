using System;
using System.Threading.Tasks;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class ExecutionControlTests : TestBase
    {
        [Fact]
        public async Task HaltExecutionOnFunctionEnter()
        {
            var host = await LispHost.CreateAsync();
            host.RootFrame.FunctionEntered += (s, e) =>
            {
                if (e.Frame.FunctionSymbol.Value == "COMMON-LISP-USER:INNER-FUNCTION")
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync(@"
(defun inner-function ()
    42)
(defun outer-function ()
    (inner-function))
(outer-function)
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Null(evalResult.LastResult);
            await host.EvalContinueAsync(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(42, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public async Task HaltExecutionOnFunctionReturn()
        {
            var host = await LispHost.CreateAsync();
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
            var evalResult = await host.EvalAsync(@"
(defun inner-function ()
    42)
(defun outer-function ()
    (inner-function)
    (sentinel))
(outer-function)
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(42, ((LispInteger)capturedReturnValue).Value);
            Assert.Equal(42, ((LispInteger)evalResult.LastResult).Value);
            Assert.False(sentinelHit);
            await host.EvalContinueAsync(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(54, ((LispInteger)evalResult.LastResult).Value);
            Assert.True(sentinelHit);
        }

        [Fact]
        public async Task HaltExecutionOnMacroExpansion()
        {
            var host = await LispHost.CreateAsync();
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
            var evalResult = await host.EvalAsync(@"
(fourty-two)
(+ 1 2)
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: 42", evalResult.ExecutionState.PeekOperation().ToString());

            await host.EvalContinueAsync(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(3, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public async Task HaltExecutionOnExpressionEvaluation()
        {
            var host = await LispHost.CreateAsync();
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
            var evalResult = await host.EvalAsync(@"
(defun inner-function ()
    (+ 40 2))
(defun outer-function ()
    (inner-function)
    (sentinel))
(outer-function)
");
            Assert.True(hitBreakpoint);
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Null(evalResult.LastResult);
            Assert.False(sentinelHit);
            await host.EvalContinueAsync(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(54, ((LispInteger)evalResult.LastResult).Value);
            Assert.True(sentinelHit);
        }

        [Fact]
        public async Task HaltExecutionAfterSimpleExpressionEvaluation()
        {
            var host = await LispHost.CreateAsync();
            host.RootFrame.EvaluatedExpression += (s, e) =>
            {
                if (e.Expression.ToString() == "2" &&
                    e.Result is LispInteger i &&
                    i.Value == 2)
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync(@"
(+ (* 2 3) (/ 12 4))
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(2, ((LispInteger)evalResult.LastResult).Value);
            await host.EvalContinueAsync(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(9, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public async Task HaltExecutionAfterInvokeExpressionEvaluation()
        {
            var host = await LispHost.CreateAsync();
            host.RootFrame.EvaluatedExpression += (s, e) =>
            {
                if (e.Expression.ToString() == "(* 2 3)" &&
                    e.Result is LispInteger i &&
                    i.Value == 6)
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync(@"
(+ (* 2 3) (/ 10 2))
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(6, ((LispInteger)evalResult.LastResult).Value);
            await host.EvalContinueAsync(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(11, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public async Task HaltExecutionOnEvaluatingFunctionArgument()
        {
            var host = await LispHost.CreateAsync();
            var hitBreakpoint = false;
            host.RootFrame.EvaluatingExpression += (s, e) =>
            {
                if (!hitBreakpoint &&
                    e.Expression is LispList list &&
                    list.ToString() == "(* 2 2)")
                {
                    hitBreakpoint = true;
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync(@"
(+ (* 2 2) (* 3 3))
");
            Assert.True(hitBreakpoint);
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Null(evalResult.LastResult);
            await host.EvalContinueAsync(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(13, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public async Task ExecutionCannotBeHaltedWhenEvaluatingFromWithinANativeFunction()
        {
            var host = await LispHost.CreateAsync();
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
            var evalResult = await host.EvalAsync("(native-function)");
            Assert.True(hitBreakpoint);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(4, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public async Task ExecutionCannotBeHaltedWhenEvaluatingFromWithinANativeMacro()
        {
            var host = await LispHost.CreateAsync();
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
            var evalResult = await host.EvalAsync("(native-function)");
            Assert.True(hitBreakpoint);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(4, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public async Task ExecutionCanBeHaltedAfterSettingAValue()
        {
            var host = await LispHost.CreateAsync();
            host.RootFrame.ValueSet += (s, e) =>
            {
                if (e.Symbol.Value == "COMMON-LISP-USER:THE-ANSWER" &&
                    e.Value is LispInteger i &&
                    i.Value == 42)
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync(@"
(+ 1 2)
(setf the-answer (+ 40 2))
(+ the-answer 2)
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            await host.EvalContinueAsync(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(44, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public async Task ExecutionCanBeHaltedWhenAnErrorIsExplicitlyRaised()
        {
            var host = await LispHost.CreateAsync();
            LispError capturedError = null;
            host.RootFrame.ErrorOccured += (s, e) =>
            {
                capturedError = e.Error;
            };
            var evalResult = await host.EvalAsync(@"
(defun test-method ()
    (error ""test error""))
(test-method)
");
            // errors _always_ halt execution
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("test error", ((LispError)evalResult.LastResult).Message);
            Assert.Equal("test error", capturedError.Message);
            Assert.True(ReferenceEquals(capturedError, evalResult.LastResult));

            // all future processing stops
            await host.EvalContinueAsync(evalResult.ExecutionState);
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
        }

        [Fact]
        public async Task ExecutionCanBeHaltedWhenAnErrorIsNaturallyEncountered()
        {
            var host = await LispHost.CreateAsync();
            LispError capturedError = null;
            host.RootFrame.ErrorOccured += (s, e) =>
            {
                capturedError = e.Error;
            };
            var evalResult = await host.EvalAsync(@"
(defun test-method ()
    (not-a-method))
(test-method)
");
            // errors _always_ halt execution
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("Undefined macro/function 'NOT-A-METHOD', found '<null>'", ((LispError)evalResult.LastResult).Message);
            Assert.Equal("Undefined macro/function 'NOT-A-METHOD', found '<null>'", capturedError.Message);
            Assert.True(ReferenceEquals(capturedError, evalResult.LastResult));

            // all future processing stops
            await host.EvalContinueAsync(evalResult.ExecutionState);
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
        }

        [Fact]
        public async Task HaltingOnErrorOccuredDoesNotHappenMoreThanOnceForTheSameError()
        {
            var host = await LispHost.CreateAsync();
            LispError capturedError = null;
            host.RootFrame.ErrorOccured += (s, e) =>
            {
                if (ReferenceEquals(capturedError, e.Error))
                {
                    throw new Exception($"Error has already been handled: {e.Error}");
                }

                capturedError = e.Error;
            };
            var evalResult = await host.EvalAsync(@"
(defun test-method ()
    (error ""test error""))
(test-method)
");
            // ensure we halted the first time on the error
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.IsType<LispError>(evalResult.LastResult);
            Assert.True(ReferenceEquals(capturedError, evalResult.LastResult));

            // continue to the end (i.e., let the error bubble up, but don't halt again)
            await host.EvalContinueAsync(evalResult.ExecutionState);
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.IsType<LispError>(evalResult.LastResult);
            Assert.True(ReferenceEquals(capturedError, evalResult.LastResult));
        }

        [Theory]
        [InlineData(true)]
        [InlineData(false)]
        public async Task ExecutionCanStepOver(bool useTailCalls)
        {
            var host = await LispHost.CreateAsync(useTailCalls: useTailCalls);
            var hasHalted = false;
            host.RootFrame.EvaluatingExpression += (s, e) =>
            {
                if (!hasHalted && e.Expression.ToString() == "(+ 2 (- 5 3))")
                {
                    e.HaltExecution = true;
                    hasHalted = true;
                }
            };
            var evalResult = await host.EvalAsync(@"
(defun test-method ()
    (+ 1 1)
    (+ 2 (- 5 3)) ; initial halt here
    (+ 3 3))
(test-method)
(+ 4 4)
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: (+ 2 (- 5 3))", evalResult.ExecutionState.PeekOperation().ToString());

            await host.StepOverAsync(evalResult.ExecutionState);
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: (+ 3 3)", evalResult.ExecutionState.PeekOperation().ToString());

            await host.StepOverAsync(evalResult.ExecutionState); // end of function, this was really a step out
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: (+ 4 4)", evalResult.ExecutionState.PeekOperation().ToString());

            await host.StepOverAsync(evalResult.ExecutionState); // execution complete; this was the last operation
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(8, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public async Task ExecutionStepOverWhenNextOperationIsNotAnExpression()
        {
            var host = await LispHost.CreateAsync();
            host.RootFrame.FunctionReturned += (s, e) =>
            {
                if (e.Frame.FunctionSymbol.Value == "COMMON-LISP-USER:TEST-METHOD")
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync(@"
(defun test-method ()
    (+ 1 2))
(test-method)
(+ 4 5)
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(3, ((LispInteger)evalResult.LastResult).Value);
            Assert.Null(evalResult.ExecutionState.PeekCurrentExpression());

            await host.StepOverAsync(evalResult.ExecutionState); // nothing to step over; it's really a step out
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: (+ 4 5)", evalResult.ExecutionState.PeekOperation().ToString());

            await host.EvalContinueAsync(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(9, ((LispInteger)evalResult.LastResult).Value); ;
        }

        [Theory]
        [InlineData(true)]
        [InlineData(false)]
        public async Task ExecutionCanStepIn(bool useTailCalls)
        {
            var host = await LispHost.CreateAsync(useTailCalls: useTailCalls);
            host.AddFunction("NATIVE-FUNCTION", (host, executionState, args, _cancellationToken) =>
            {
                return Task.FromResult(host.T);
            });
            var hasHalted = false;
            host.RootFrame.EvaluatingExpression += (s, e) =>
            {
                if (!hasHalted && e.Expression.ToString() == "(TEST-METHOD)")
                {
                    e.HaltExecution = true;
                    hasHalted = true;
                }
            };
            var evalResult = await host.EvalAsync(@"
(defun test-method ()
    (native-function)
    (+ 1 1))
(test-method) ; initial halt here
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: (TEST-METHOD)", evalResult.ExecutionState.PeekOperation().ToString());

            await host.StepInAsync(evalResult.ExecutionState);
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: (NATIVE-FUNCTION)", evalResult.ExecutionState.PeekOperation().ToString());

            await host.StepInAsync(evalResult.ExecutionState); // can't step in to a native function; this is really a step over
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: (+ 1 1)", evalResult.ExecutionState.PeekOperation().ToString());
        }

        [Fact]
        public async Task ExecutionStepInWhenNextOperationIsNotAnExpression()
        {
            var host = await LispHost.CreateAsync();
            host.RootFrame.FunctionReturned += (s, e) =>
            {
                if (e.Frame.FunctionSymbol.Value == "COMMON-LISP-USER:TEST-METHOD")
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync(@"
(defun test-method ()
    (+ 1 2))
(test-method)
(+ 4 5)
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(3, ((LispInteger)evalResult.LastResult).Value);
            Assert.Null(evalResult.ExecutionState.PeekCurrentExpression());

            await host.StepInAsync(evalResult.ExecutionState); // nothing to step in to, so it's really step out _then_ step in
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: 4", evalResult.ExecutionState.PeekOperation().ToString());

            await host.EvalContinueAsync(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(9, ((LispInteger)evalResult.LastResult).Value); ;
        }

        [Theory]
        [InlineData(true)]
        [InlineData(false)]
        public async Task ExecutionCanStepOut(bool useTailCalls)
        {
            var host = await LispHost.CreateAsync(useTailCalls: useTailCalls);
            var hasHalted = false;
            host.RootFrame.EvaluatingExpression += (s, e) =>
            {
                if (!hasHalted && e.Expression.ToString() == "(+ 1 1)")
                {
                    e.HaltExecution = true;
                    hasHalted = true;
                }
            };
            var evalResult = await host.EvalAsync(@"
(defun test-method ()
    (+ 1 1) ; initial halt here
    (+ 2 2))
(test-method)
(+ 3 3)
(+ 4 4)
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: (+ 1 1)", evalResult.ExecutionState.PeekOperation().ToString());

            await host.StepOutAsync(evalResult.ExecutionState);
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: (+ 3 3)", evalResult.ExecutionState.PeekOperation().ToString());

            await host.StepOutAsync(evalResult.ExecutionState); // can't step out at the root level; this was really a run to end
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(8, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public async Task ExecutionStepOutWhenNextOperationIsNotAnExpression()
        {
            var host = await LispHost.CreateAsync();
            host.RootFrame.FunctionReturned += (s, e) =>
            {
                if (e.Frame.FunctionSymbol.Value == "COMMON-LISP-USER:TEST-METHOD")
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync(@"
(defun test-method ()
    (+ 1 2))
(test-method)
(+ 4 5)
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(3, ((LispInteger)evalResult.LastResult).Value);
            Assert.Null(evalResult.ExecutionState.PeekCurrentExpression());

            await host.StepOutAsync(evalResult.ExecutionState); // stepping out here steps out of everything
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(9, ((LispInteger)evalResult.LastResult).Value); ;
        }

        [Fact]
        public async Task EvaluationCanBeHaltedInsideSetFValue()
        {
            var enteredGetValue = false;
            var host = await LispHost.CreateAsync();
            host.RootFrame.FunctionEntered += (s, e) =>
            {
                if (e.Frame.FunctionSymbol.Value == "COMMON-LISP-USER:GET-VALUE")
                {
                    enteredGetValue = true;
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync(@"
(defun get-value ()
    42)
(setf x (get-value))");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.True(enteredGetValue);
            Assert.Null(host.GetValue("X"));
        }

        [Fact]
        public async Task EvaluationCanBeHaltedInsideSetFDestination()
        {
            var enteredSetValue = false;
            var host = await LispHost.CreateAsync();
            host.RootFrame.EvaluatedExpression += (s, e) =>
            {
                if (e.Expression.ToString() == "(CAR X)")
                {
                    enteredSetValue = true;
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync(@"
(setf x '(1 2 3))
(setf (car x) 11)");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.True(enteredSetValue);
            Assert.Equal("(1 2 3)", host.GetValue("X").ToString());
        }

        [Fact]
        public async Task EvaluationCanBeHaltedInsideCondPredicate()
        {
            var enteredPredicate = false;
            var host = await LispHost.CreateAsync();
            host.RootFrame.EvaluatingExpression += (s, e) =>
            {
                if (e.Expression.ToString() == "(+ 1 2)")
                {
                    enteredPredicate = true;
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync(@"
(cond ((= (+ 1 2) 3)    1)
      (t                2))
(setf x 123) ; make sure we don't get here");
            EnsureNotError(evalResult.LastResult);
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.True(enteredPredicate);
            Assert.Null(host.GetValue("X"));
        }

        [Fact]
        public async Task EvaluationCanBeHaltedInsideCondResult()
        {
            var enteredResult = false;
            var host = await LispHost.CreateAsync();
            host.RootFrame.EvaluatingExpression += (s, e) =>
            {
                if (e.Expression.ToString() == "(+ 1 2)")
                {
                    enteredResult = true;
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync(@"
(cond (t    (+ 1 2)))
(setf x 123) ; make sure we don't get here");
            EnsureNotError(evalResult.LastResult);
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.True(enteredResult);
            Assert.Null(host.GetValue("X"));
        }

        [Fact]
        public async Task EvaluationCanBeHaltedInsideIfPredicate()
        {
            var enteredPredicate = false;
            var host = await LispHost.CreateAsync();
            host.RootFrame.EvaluatingExpression += (s, e) =>
            {
                if (e.Expression.ToString() == "(+ 1 2)")
                {
                    enteredPredicate = true;
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync(@"
(if (= (+ 1 2) 3)
    1
    2)
(setf x 123) ; make sure we don't get here");
            EnsureNotError(evalResult.LastResult);
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.True(enteredPredicate);
            Assert.Null(host.GetValue("X"));
        }

        [Fact]
        public async Task EvaluationCanBeHaltedInsideIfTrueBranch()
        {
            var enteredResult = false;
            var host = await LispHost.CreateAsync();
            host.RootFrame.EvaluatingExpression += (s, e) =>
            {
                if (e.Expression.ToString() == "(+ 1 2)")
                {
                    enteredResult = true;
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync(@"
(if t
    (+ 1 2)
    2)
(setf x 123) ; make sure we don't get here");
            EnsureNotError(evalResult.LastResult);
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.True(enteredResult);
            Assert.Null(host.GetValue("X"));
        }

        [Fact]
        public async Task EvaluationCanBeHaltedInsideIfFalseBranch()
        {
            var enteredResult = false;
            var host = await LispHost.CreateAsync();
            host.RootFrame.EvaluatingExpression += (s, e) =>
            {
                if (e.Expression.ToString() == "(+ 1 2)")
                {
                    enteredResult = true;
                    e.HaltExecution = true;
                }
            };
            var evalResult = await host.EvalAsync(@"
(if nil
    1
    (+ 1 2))
(setf x 123) ; make sure we don't get here");
            EnsureNotError(evalResult.LastResult);
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.True(enteredResult);
            Assert.Null(host.GetValue("X"));
        }
    }
}
