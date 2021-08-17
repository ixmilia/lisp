namespace IxMilia.Lisp
{
    public class LispFunctionReturnedEventArgs : LispExecutionEventArgs
    {
        public LispFunction Function { get; }
        public LispStackFrame Frame { get; }
        public LispObject ReturnValue { get; }

        public LispFunctionReturnedEventArgs(LispFunction function, LispStackFrame frame, LispObject returnValue)
        {
            Function = function;
            Frame = frame;
            ReturnValue = returnValue;
        }
    }
}
