namespace IxMilia.Lisp
{
    public class LispEvaluatingExpressionEventArgs : LispExecutionEventArgs
    {
        public LispObject Expression { get; }
        public LispStackFrame StackFrame { get; }

        public LispEvaluatingExpressionEventArgs(LispObject expression, LispStackFrame stackFrame)
        {
            Expression = expression;
            StackFrame = stackFrame;
        }
    }
}
