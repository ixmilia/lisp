﻿using Xunit;

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
                if (e.Frame.FunctionName == "inner-function")
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
            var finalExecutionState = host.Eval(executionState);
            Assert.True(finalExecutionState.IsExecutionComplete);
            Assert.Equal(42, ((LispInteger)finalExecutionState.LastResult).Value);
        }

        [Fact]
        public void HaltExecutionOnFunctionReturn()
        {
            var host = new LispHost();
            bool sentinelHit = false;
            host.AddFunction("sentinel", (frame, args) =>
            {
                sentinelHit = true;
                return new LispInteger(54);
            });
            LispObject capturedReturnValue = null;
            host.RootFrame.FunctionReturned += (s, e) =>
            {
                if (e.Frame.FunctionName == "inner-function")
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
            var finalExecutionState = host.Eval(executionState);
            Assert.True(finalExecutionState.IsExecutionComplete);
            Assert.Equal(54, ((LispInteger)finalExecutionState.LastResult).Value);
            Assert.True(sentinelHit);
        }

        [Fact]
        public void HaltExecutionOnExpressionEvaluation()
        {
            var host = new LispHost();
            bool hitBreakpoint = false;
            bool sentinelHit = false;
            host.AddFunction("sentinel", (frame, args) =>
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
            var finalExecutionState = host.Eval(executionState);
            Assert.True(finalExecutionState.IsExecutionComplete);
            Assert.Equal(54, ((LispInteger)finalExecutionState.LastResult).Value);
            Assert.True(sentinelHit);
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
            var finalExecutionState = host.Eval(executionState);
            Assert.True(finalExecutionState.IsExecutionComplete);
            Assert.Equal(13, ((LispInteger)finalExecutionState.LastResult).Value);
        }

        [Fact]
        public void ExecutionCannotBeHaltedWhenEvaluatingFromWithinANativeFunction()
        {
            var host = new LispHost();
            host.AddFunction("native-function", (frame, args) =>
            {
                var result = frame.Eval(LispList.FromEnumerable(new LispObject[] { new LispSymbol("*"), new LispInteger(2), new LispInteger(2) }));
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
            host.AddMacro("native-function", (frame, args) =>
            {
                var result = frame.Eval(LispList.FromEnumerable(new LispObject[] { new LispSymbol("*"), new LispInteger(2), new LispInteger(2) }));
                return new LispObject[] { result };
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
                if (e.Name == "the-answer" &&
                    e.Value is LispInteger i &&
                    i.Value == 42)
                {
                    e.HaltExecution = true;
                }
            };
            var executionState = host.Eval(@"
(defun test-method ()
    (let ((the-answer (+ 40 2)))
        (+ the-answer 2)))
(test-method)
");
            Assert.False(executionState.IsExecutionComplete);
            var finalExecutionState = host.Eval(executionState);
            Assert.True(finalExecutionState.IsExecutionComplete);
            Assert.Equal(44, ((LispInteger)finalExecutionState.LastResult).Value);
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
            var finalExecutionState = host.Eval(executionState);
            Assert.False(finalExecutionState.IsExecutionComplete);
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
            Assert.Equal("Undefined macro/function 'not-a-method', found '<null>'", ((LispError)executionState.LastResult).Message);
            Assert.Equal("Undefined macro/function 'not-a-method', found '<null>'", capturedError.Message);
            Assert.True(ReferenceEquals(capturedError, executionState.LastResult));

            // all future processing stops
            var finalExecutionState = host.Eval(executionState);
            Assert.False(finalExecutionState.IsExecutionComplete);
        }
    }
}
