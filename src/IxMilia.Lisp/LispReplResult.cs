namespace IxMilia.Lisp
{
    public class LispReplResult
    {
        public LispExecutionState ExecutionState { get; }
        public int ExpressionDepth { get; }

        public LispObject LastResult => ExecutionState?.LastReportedError ?? ExecutionState?.LastResult;

        public LispReplResult(LispExecutionState executionState, int expressionDepth)
        {
            ExecutionState = executionState;
            ExpressionDepth = expressionDepth;
        }
    }
}
