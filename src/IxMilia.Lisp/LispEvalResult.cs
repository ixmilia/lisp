namespace IxMilia.Lisp
{
    public class LispEvalResult
    {
        public LispEvaluationState State { get; }
        public LispObject Value { get; }

        internal LispEvalResult(LispEvaluationState state, LispObject value)
        {
            State = state;
            Value = value;
        }

        internal LispEvalResult(LispEvaluationState state, LispExecutionState executionState)
            : this(state, executionState.LastReportedError ?? executionState.LastResult)
        {
        }
    }
}
