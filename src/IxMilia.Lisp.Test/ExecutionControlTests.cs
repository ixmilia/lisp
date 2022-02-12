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
                if (e.Frame.FunctionSymbol.Value == "COMMON-LISP-USER:INNER-FUNCTION")
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = host.Eval(@"
(defun inner-function ()
    42)
(defun outer-function ()
    (inner-function))
(outer-function)
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Null(evalResult.LastResult);
            host.EvalContinue(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(42, ((LispInteger)evalResult.LastResult).Value);
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
                if (e.Frame.FunctionSymbol.Value == "COMMON-LISP-USER:INNER-FUNCTION")
                {
                    e.HaltExecution = true;
                    capturedReturnValue = e.ReturnValue;
                }
            };
            var evalResult = host.Eval(@"
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
            host.EvalContinue(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(54, ((LispInteger)evalResult.LastResult).Value);
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
                if (e.Macro.NameSymbol.LocalName == "FOURTY-TWO")
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = host.Eval(@"
(fourty-two)
(+ 1 2)
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: 42", evalResult.ExecutionState.PeekOperation().ToString());

            host.EvalContinue(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(3, ((LispInteger)evalResult.LastResult).Value);
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
                    symbol.LocalName == "+")
                {
                    hitBreakpoint = true;
                    e.HaltExecution = true;
                }
            };
            var evalResult = host.Eval(@"
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
            host.EvalContinue(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(54, ((LispInteger)evalResult.LastResult).Value);
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
            var evalResult = host.Eval(@"
(+ (* 2 3) (/ 12 4))
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(2, ((LispInteger)evalResult.LastResult).Value);
            host.EvalContinue(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(9, ((LispInteger)evalResult.LastResult).Value);
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
            var evalResult = host.Eval(@"
(+ (* 2 3) (/ 10 2))
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(6, ((LispInteger)evalResult.LastResult).Value);
            host.EvalContinue(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(11, ((LispInteger)evalResult.LastResult).Value);
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
            var evalResult = host.Eval(@"
(+ (* 2 2) (* 3 3))
");
            Assert.True(hitBreakpoint);
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Null(evalResult.LastResult);
            host.EvalContinue(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(13, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public void ExecutionCannotBeHaltedWhenEvaluatingFromWithinANativeFunction()
        {
            var host = new LispHost();
            host.AddFunction("NATIVE-FUNCTION", (host, executionState, args) =>
            {
                var result = host.EvalAtStackFrame(executionState.StackFrame, LispList.FromEnumerable(new LispObject[] { LispSymbol.CreateFromString("*"), new LispInteger(2), new LispInteger(2) }));
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
            var evalResult = host.Eval("(native-function)");
            Assert.True(hitBreakpoint);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(4, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public void ExecutionCannotBeHaltedWhenEvaluatingFromWithinANativeMacro()
        {
            var host = new LispHost();
            host.AddMacro("NATIVE-FUNCTION", (host, executionState, args) =>
            {
                var result = host.EvalAtStackFrame(executionState.StackFrame, LispList.FromEnumerable(new LispObject[] { LispSymbol.CreateFromString("*"), new LispInteger(2), new LispInteger(2) }));
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
            var evalResult = host.Eval("(native-function)");
            Assert.True(hitBreakpoint);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(4, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public void ExecutionCanBeHaltedAfterSettingAValue()
        {
            var host = new LispHost();
            host.RootFrame.ValueSet += (s, e) =>
            {
                if (e.Symbol.Value == "COMMON-LISP-USER:THE-ANSWER" &&
                    e.Value is LispInteger i &&
                    i.Value == 42)
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = host.Eval(@"
(+ 1 2)
(setf the-answer (+ 40 2))
(+ the-answer 2)
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            host.EvalContinue(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(44, ((LispInteger)evalResult.LastResult).Value);
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
            var evalResult = host.Eval(@"
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
            host.EvalContinue(evalResult.ExecutionState);
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
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
            var evalResult = host.Eval(@"
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
            host.EvalContinue(evalResult.ExecutionState);
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
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
            var evalResult = host.Eval(@"
(defun test-method ()
    (+ 1 1)
    (+ 2 (- 5 3)) ; initial halt here
    (+ 3 3))
(test-method)
(+ 4 4)
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: (+ 2 (- 5 3))", evalResult.ExecutionState.PeekOperation().ToString());

            host.StepOver(evalResult.ExecutionState);
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: (+ 3 3)", evalResult.ExecutionState.PeekOperation().ToString());

            host.StepOver(evalResult.ExecutionState); // end of function, this was really a step out
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: (+ 4 4)", evalResult.ExecutionState.PeekOperation().ToString());

            host.StepOver(evalResult.ExecutionState); // execution complete; this was the last operation
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(8, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public void ExecutionStepOverWhenNextOperationIsNotAnExpression()
        {
            var host = new LispHost();
            host.RootFrame.FunctionReturned += (s, e) =>
            {
                if (e.Frame.FunctionSymbol.Value == "COMMON-LISP-USER:TEST-METHOD")
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = host.Eval(@"
(defun test-method ()
    (+ 1 2))
(test-method)
(+ 4 5)
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(3, ((LispInteger)evalResult.LastResult).Value);
            Assert.Null(evalResult.ExecutionState.PeekCurrentExpression());

            host.StepOver(evalResult.ExecutionState); // nothing to step over; it's really a step out
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: (+ 4 5)", evalResult.ExecutionState.PeekOperation().ToString());

            host.EvalContinue(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(9, ((LispInteger)evalResult.LastResult).Value); ;
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
            var evalResult = host.Eval(@"
(defun test-method ()
    (native-function)
    (+ 1 1))
(test-method) ; initial halt here
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: (TEST-METHOD)", evalResult.ExecutionState.PeekOperation().ToString());

            host.StepIn(evalResult.ExecutionState);
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: (NATIVE-FUNCTION)", evalResult.ExecutionState.PeekOperation().ToString());

            host.StepIn(evalResult.ExecutionState); // can't step in to a native function; this is really a step over
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: (+ 1 1)", evalResult.ExecutionState.PeekOperation().ToString());
        }

        [Fact]
        public void ExecutionStepInWhenNextOperationIsNotAnExpression()
        {
            var host = new LispHost();
            host.RootFrame.FunctionReturned += (s, e) =>
            {
                if (e.Frame.FunctionSymbol.Value == "COMMON-LISP-USER:TEST-METHOD")
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = host.Eval(@"
(defun test-method ()
    (+ 1 2))
(test-method)
(+ 4 5)
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(3, ((LispInteger)evalResult.LastResult).Value);
            Assert.Null(evalResult.ExecutionState.PeekCurrentExpression());

            host.StepIn(evalResult.ExecutionState); // nothing to step in to, so it's really step out _then_ step in
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: 4", evalResult.ExecutionState.PeekOperation().ToString());

            host.EvalContinue(evalResult.ExecutionState);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(9, ((LispInteger)evalResult.LastResult).Value); ;
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
            var evalResult = host.Eval(@"
(defun test-method ()
    (+ 1 1) ; initial halt here
    (+ 2 2))
(test-method)
(+ 3 3)
(+ 4 4)
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: (+ 1 1)", evalResult.ExecutionState.PeekOperation().ToString());

            host.StepOut(evalResult.ExecutionState);
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal("s: (+ 3 3)", evalResult.ExecutionState.PeekOperation().ToString());

            host.StepOut(evalResult.ExecutionState); // can't step out at the root level; this was really a run to end
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(8, ((LispInteger)evalResult.LastResult).Value);
        }

        [Fact]
        public void ExecutionStepOutWhenNextOperationIsNotAnExpression()
        {
            var host = new LispHost();
            host.RootFrame.FunctionReturned += (s, e) =>
            {
                if (e.Frame.FunctionSymbol.Value == "COMMON-LISP-USER:TEST-METHOD")
                {
                    e.HaltExecution = true;
                }
            };
            var evalResult = host.Eval(@"
(defun test-method ()
    (+ 1 2))
(test-method)
(+ 4 5)
");
            Assert.False(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(3, ((LispInteger)evalResult.LastResult).Value);
            Assert.Null(evalResult.ExecutionState.PeekCurrentExpression());

            host.StepOut(evalResult.ExecutionState); // stepping out here steps out of everything
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.Equal(9, ((LispInteger)evalResult.LastResult).Value); ;
        }
    }
}
