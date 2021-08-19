namespace IxMilia.Lisp
{
    public class LispReplResult
    {
        public LispExecutionState ExecutionState { get; }
        public int ExpressionDepth { get; }

        public LispReplResult(LispExecutionState executionState, int expressionDepth)
        {
            ExecutionState = executionState;
            ExpressionDepth = expressionDepth;
        }
    }
}
