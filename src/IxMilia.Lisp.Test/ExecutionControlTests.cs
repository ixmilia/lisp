using Xunit;

namespace IxMilia.Lisp.Test
{
    public class ExecutionControlTests : TestBase
    {
        [Fact]
        public void HaltExecutionOnFunctionEnter()
        {
            var host = new LispHost();
            host.RootFrame.FunctionEntered += (s, e) =>
            {
                if (e.Frame.FunctionName == "INNER-FUNCTION")
                {
                    e.HaltExecution = true;
                }
            };
            var executionState = host.Eval(@"
(defun inner-function ()
    42)
(defun outer-function ()
    (inner-function))
(outer-function)
");
            Assert.False(executionState.IsExecutionComplete);
            Assert.Null(executionState.LastResult);
            host.Run(executionState);
            Assert.True(executionState.IsExecutionComplete);
            Assert.Equal(42, ((LispInteger)executionState.LastResult).Value);
        }

        [Fact]
        public void HaltExecutionOnFunctionReturn()
        {
            var host = new LispHost();
            bool sentinelHit = false;
            host.AddFunction("SENTINEL", (host, executionState, args) =>
            {
                sentinelHit = true;
                return new LispInteger(54);
            });
            LispObject capturedReturnValue = null;
            host.RootFrame.FunctionReturned += (s, e) =>
            {
                if (e.Frame.FunctionName == "INNER-FUNCTION")
                {
                    e.HaltExecution = true;
                    capturedReturnValue = e.ReturnValue;
                }
            };
            var executionState = host.Eval(@"
(defun inner-function ()
    42)
(defun outer-function ()
    (inner-function)
    (sentinel))
(outer-function)
");
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal(42, ((LispInteger)capturedReturnValue).Value);
            Assert.Equal(42, ((LispInteger)executionState.LastResult).Value);
            Assert.False(sentinelHit);
            host.Run(executionState);
            Assert.True(executionState.IsExecutionComplete);
            Assert.Equal(54, ((LispInteger)executionState.LastResult).Value);
            Assert.True(sentinelHit);
        }

        [Fact]
        public void HaltExecutionOnMacroExpansion()
        {
            var host = new LispHost();
            host.AddMacro("FOURTY-TWO", (host, executionState, args) =>
            {
                return new LispInteger(42);
            });
            host.RootFrame.MacroExpanded += (s, e) =>
            {
                if (e.Macro.Name == "FOURTY-TWO")
                {
                    e.HaltExecution = true;
                }
            };
            var executionState = host.Eval(@"
(fourty-two)
(+ 1 2)
");
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: 42", executionState.PeekOperation().ToString());

            host.Run(executionState);
            Assert.True(executionState.IsExecutionComplete);
            Assert.Equal(3, ((LispInteger)executionState.LastResult).Value);
        }

        [Fact]
        public void HaltExecutionOnExpressionEvaluation()
        {
            var host = new LispHost();
            bool hitBreakpoint = false;
            bool sentinelHit = false;
            host.AddFunction("SENTINEL", (host, executionState, args) =>
            {
                sentinelHit = true;
                return new LispInteger(54);
            });
            host.RootFrame.EvaluatingExpression += (s, e) =>
            {
                if (!hitBreakpoint &&
                    e.Expression is LispList list &&
                    list.Value is LispSymbol symbol &&
                    symbol.Value == "+")
                {
                    hitBreakpoint = true;
                    e.HaltExecution = true;
                }
            };
            var executionState = host.Eval(@"
(defun inner-function ()
    (+ 40 2))
(defun outer-function ()
    (inner-function)
    (sentinel))
(outer-function)
");
            Assert.True(hitBreakpoint);
            Assert.False(executionState.IsExecutionComplete);
            Assert.Null(executionState.LastResult);
            Assert.False(sentinelHit);
            host.Run(executionState);
            Assert.True(executionState.IsExecutionComplete);
            Assert.Equal(54, ((LispInteger)executionState.LastResult).Value);
            Assert.True(sentinelHit);
        }

        [Fact]
        public void HaltExecutionAfterSimpleExpressionEvaluation()
        {
            var host = new LispHost();
            host.RootFrame.EvaluatedExpression += (s, e) =>
            {
                if (e.Expression.ToString() == "2" &&
                    e.Result is LispInteger i &&
                    i.Value == 2)
                {
                    e.HaltExecution = true;
                }
            };
            var executionState = host.Eval(@"
(+ (* 2 3) (/ 12 4))
");
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal(2, ((LispInteger)executionState.LastResult).Value);
            host.Run(executionState);
            Assert.True(executionState.IsExecutionComplete);
            Assert.Equal(9, ((LispInteger)executionState.LastResult).Value);
        }

        [Fact]
        public void HaltExecutionAfterInvokeExpressionEvaluation()
        {
            var host = new LispHost();
            host.RootFrame.EvaluatedExpression += (s, e) =>
            {
                if (e.Expression.ToString() == "(* 2 3)" &&
                    e.Result is LispInteger i &&
                    i.Value == 6)
                {
                    e.HaltExecution = true;
                }
            };
            var executionState = host.Eval(@"
(+ (* 2 3) (/ 10 2))
");
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal(6, ((LispInteger)executionState.LastResult).Value);
            host.Run(executionState);
            Assert.True(executionState.IsExecutionComplete);
            Assert.Equal(11, ((LispInteger)executionState.LastResult).Value);
        }

        [Fact]
        public void HaltExecutionOnEvaluatingFunctionArgument()
        {
            var host = new LispHost();
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
            var executionState = host.Eval(@"
(+ (* 2 2) (* 3 3))
");
            Assert.True(hitBreakpoint);
            Assert.False(executionState.IsExecutionComplete);
            Assert.Null(executionState.LastResult);
            host.Run(executionState);
            Assert.True(executionState.IsExecutionComplete);
            Assert.Equal(13, ((LispInteger)executionState.LastResult).Value);
        }

        [Fact]
        public void ExecutionCannotBeHaltedWhenEvaluatingFromWithinANativeFunction()
        {
            var host = new LispHost();
            host.AddFunction("NATIVE-FUNCTION", (host, executionState, args) =>
            {
                var result = host.EvalAtStackFrame(executionState.StackFrame, LispList.FromEnumerable(new LispObject[] { new LispSymbol("*"), new LispInteger(2), new LispInteger(2) }));
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
            var executionState = host.Eval("(native-function)");
            Assert.True(hitBreakpoint);
            Assert.True(executionState.IsExecutionComplete);
            Assert.Equal(4, ((LispInteger)executionState.LastResult).Value);
        }

        [Fact]
        public void ExecutionCannotBeHaltedWhenEvaluatingFromWithinANativeMacro()
        {
            var host = new LispHost();
            host.AddMacro("NATIVE-FUNCTION", (host, executionState, args) =>
            {
                var result = host.EvalAtStackFrame(executionState.StackFrame, LispList.FromEnumerable(new LispObject[] { new LispSymbol("*"), new LispInteger(2), new LispInteger(2) }));
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
            var executionState = host.Eval("(native-function)");
            Assert.True(hitBreakpoint);
            Assert.True(executionState.IsExecutionComplete);
            Assert.Equal(4, ((LispInteger)executionState.LastResult).Value);
        }

        [Fact]
        public void ExecutionCanBeHaltedAfterSettingAValue()
        {
            var host = new LispHost();
            host.RootFrame.ValueSet += (s, e) =>
            {
                if (e.Name == "THE-ANSWER" &&
                    e.Value is LispInteger i &&
                    i.Value == 42)
                {
                    e.HaltExecution = true;
                }
            };
            var executionState = host.Eval(@"
(+ 1 2)
(setf the-answer (+ 40 2))
(+ the-answer 2)
");
            Assert.False(executionState.IsExecutionComplete);
            host.Run(executionState);
            Assert.True(executionState.IsExecutionComplete);
            Assert.Equal(44, ((LispInteger)executionState.LastResult).Value);
        }

        [Fact]
        public void ExecutionCanBeHaltedWhenAnErrorIsExplicitlyRaised()
        {
            var host = new LispHost();
            LispError capturedError = null;
            host.RootFrame.ErrorOccured += (s, e) =>
            {
                capturedError = e.Error;
            };
            var executionState = host.Eval(@"
(defun test-method ()
    (error ""test error""))
(test-method)
");
            // errors _always_ halt execution
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("test error", ((LispError)executionState.LastResult).Message);
            Assert.Equal("test error", capturedError.Message);
            Assert.True(ReferenceEquals(capturedError, executionState.LastResult));

            // all future processing stops
            host.Run(executionState);
            Assert.False(executionState.IsExecutionComplete);
        }

        [Fact]
        public void ExecutionCanBeHaltedWhenAnErrorIsNaturallyEncountered()
        {
            var host = new LispHost();
            LispError capturedError = null;
            host.RootFrame.ErrorOccured += (s, e) =>
            {
                capturedError = e.Error;
            };
            var executionState = host.Eval(@"
(defun test-method ()
    (not-a-method))
(test-method)
");
            // errors _always_ halt execution
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("Undefined macro/function 'NOT-A-METHOD', found '<null>'", ((LispError)executionState.LastResult).Message);
            Assert.Equal("Undefined macro/function 'NOT-A-METHOD', found '<null>'", capturedError.Message);
            Assert.True(ReferenceEquals(capturedError, executionState.LastResult));

            // all future processing stops
            host.Run(executionState);
            Assert.False(executionState.IsExecutionComplete);
        }

        [Theory]
        [InlineData(true)]
        [InlineData(false)]
        public void ExecutionCanStepOver(bool useTailCalls)
        {
            var host = new LispHost(useTailCalls: useTailCalls);
            var hasHalted = false;
            host.RootFrame.EvaluatingExpression += (s, e) =>
            {
                if (!hasHalted && e.Expression.ToString() == "(+ 2 (- 5 3))")
                {
                    e.HaltExecution = true;
                    hasHalted = true;
                }
            };
            var executionState = host.Eval(@"
(defun test-method ()
    (+ 1 1)
    (+ 2 (- 5 3)) ; initial halt here
    (+ 3 3))
(test-method)
(+ 4 4)
");
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: (+ 2 (- 5 3))", executionState.PeekOperation().ToString());

            host.StepOver(executionState);
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: (+ 3 3)", executionState.PeekOperation().ToString());

            host.StepOver(executionState); // end of function, this was really a step out
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: (+ 4 4)", executionState.PeekOperation().ToString());

            host.StepOver(executionState); // execution complete; this was the last operation
            Assert.True(executionState.IsExecutionComplete);
            Assert.Equal(8, ((LispInteger)executionState.LastResult).Value);
        }

        [Fact]
        public void ExecutionStepOverWhenNextOperationIsNotAnExpression()
        {
            var host = new LispHost();
            host.RootFrame.FunctionReturned += (s, e) =>
            {
                if (e.Frame.FunctionName == "TEST-METHOD")
                {
                    e.HaltExecution = true;
                }
            };
            var executionState = host.Eval(@"
(defun test-method ()
    (+ 1 2))
(test-method)
(+ 4 5)
");
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal(3, ((LispInteger)executionState.LastResult).Value);
            Assert.Null(executionState.PeekCurrentExpression());

            host.StepOver(executionState); // nothing to step over; it's really a step out
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: (+ 4 5)", executionState.PeekOperation().ToString());

            host.Run(executionState);
            Assert.True(executionState.IsExecutionComplete);
            Assert.Equal(9, ((LispInteger)executionState.LastResult).Value); ;
        }

        [Theory]
        [InlineData(true)]
        [InlineData(false)]
        public void ExecutionCanStepIn(bool useTailCalls)
        {
            var host = new LispHost(useTailCalls: useTailCalls);
            host.AddFunction("NATIVE-FUNCTION", (host, executionState, args) =>
            {
                return host.T;
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
            var executionState = host.Eval(@"
(defun test-method ()
    (native-function)
    (+ 1 1))
(test-method) ; initial halt here
");
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: (TEST-METHOD)", executionState.PeekOperation().ToString());

            host.StepIn(executionState);
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: (NATIVE-FUNCTION)", executionState.PeekOperation().ToString());

            host.StepIn(executionState); // can't step in to a native function; this is really a step over
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: (+ 1 1)", executionState.PeekOperation().ToString());
        }

        [Fact]
        public void ExecutionStepInWhenNextOperationIsNotAnExpression()
        {
            var host = new LispHost();
            host.RootFrame.FunctionReturned += (s, e) =>
            {
                if (e.Frame.FunctionName == "TEST-METHOD")
                {
                    e.HaltExecution = true;
                }
            };
            var executionState = host.Eval(@"
(defun test-method ()
    (+ 1 2))
(test-method)
(+ 4 5)
");
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal(3, ((LispInteger)executionState.LastResult).Value);
            Assert.Null(executionState.PeekCurrentExpression());

            host.StepIn(executionState); // nothing to step in to, so it's really step out _then_ step in
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: 4", executionState.PeekOperation().ToString());

            host.Run(executionState);
            Assert.True(executionState.IsExecutionComplete);
            Assert.Equal(9, ((LispInteger)executionState.LastResult).Value); ;
        }

        [Theory]
        [InlineData(true)]
        [InlineData(false)]
        public void ExecutionCanStepOut(bool useTailCalls)
        {
            var host = new LispHost(useTailCalls: useTailCalls);
            var hasHalted = false;
            host.RootFrame.EvaluatingExpression += (s, e) =>
            {
                if (!hasHalted && e.Expression.ToString() == "(+ 1 1)")
                {
                    e.HaltExecution = true;
                    hasHalted = true;
                }
            };
            var executionState = host.Eval(@"
(defun test-method ()
    (+ 1 1) ; initial halt here
    (+ 2 2))
(test-method)
(+ 3 3)
(+ 4 4)
");
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: (+ 1 1)", executionState.PeekOperation().ToString());

            host.StepOut(executionState);
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal("s: (+ 3 3)", executionState.PeekOperation().ToString());

            host.StepOut(executionState); // can't step out at the root level; this was really a run to end
            Assert.True(executionState.IsExecutionComplete);
            Assert.Equal(8, ((LispInteger)executionState.LastResult).Value);
        }

        [Fact]
        public void ExecutionStepOutWhenNextOperationIsNotAnExpression()
        {
            var host = new LispHost();
            host.RootFrame.FunctionReturned += (s, e) =>
            {
                if (e.Frame.FunctionName == "TEST-METHOD")
                {
                    e.HaltExecution = true;
                }
            };
            var executionState = host.Eval(@"
(defun test-method ()
    (+ 1 2))
(test-method)
(+ 4 5)
");
            Assert.False(executionState.IsExecutionComplete);
            Assert.Equal(3, ((LispInteger)executionState.LastResult).Value);
            Assert.Null(executionState.PeekCurrentExpression());

            host.StepOut(executionState); // stepping out here steps out of everything
            Assert.True(executionState.IsExecutionComplete);
            Assert.Equal(9, ((LispInteger)executionState.LastResult).Value); ;
        }
    }
}
