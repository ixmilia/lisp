namespace IxMilia.Lisp
{
    public class LispEvaluatedExpressionEventArgs : LispExecutionEventArgs
    {
        public LispObject Expression { get; }
        public LispObject Result { get; }
        public LispStackFrame StackFrame { get; }

        public LispEvaluatedExpressionEventArgs(LispObject expression, LispObject result, LispStackFrame stackFrame)
        {
            Expression = expression;
            Result = result;
            StackFrame = stackFrame;
        }
    }
}
