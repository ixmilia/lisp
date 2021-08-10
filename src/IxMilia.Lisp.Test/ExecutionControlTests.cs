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
    }
}
