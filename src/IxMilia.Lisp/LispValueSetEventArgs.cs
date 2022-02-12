namespace IxMilia.Lisp
{
    public class LispValueSetEventArgs : LispExecutionEventArgs
    {
        public LispResolvedSymbol Symbol { get; }
        public LispObject Value { get; }
        public LispStackFrame StackFrame { get; }

        public LispValueSetEventArgs(LispResolvedSymbol symbol, LispObject value, LispStackFrame stackFrame)
        {
            Symbol = symbol;
            Value = value;
            StackFrame = stackFrame;
        }
    }
}
