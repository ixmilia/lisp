namespace IxMilia.Lisp
{
    public class LispEvalResult
    {
        public LispExecutionState ExecutionState { get; }
        public int ExpressionDepth { get; internal set; }
        public LispError ReadError { get; internal set; }
        public string IncompleteInput { get; internal set; }

        public LispObject LastResult => ExecutionState?.LastReportedError ?? ExecutionState?.LastResult;

        internal LispEvalResult(LispExecutionState executionState)
        {
            ExecutionState = executionState;
        }
    }
}
