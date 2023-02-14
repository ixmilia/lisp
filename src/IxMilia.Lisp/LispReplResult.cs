namespace IxMilia.Lisp
{
    public class LispReplResult
    {
        public LispObject LastResult { get; }
        public LispExecutionState ExecutionState { get; }
        public int ExpressionDepth { get; }

        public LispReplResult(LispObject lastResult, LispExecutionState executionState, int expressionDepth)
        {
            LastResult = lastResult;
            ExecutionState = executionState;
            ExpressionDepth = expressionDepth;
        }
    }
}
