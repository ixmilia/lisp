namespace IxMilia.Lisp
{
    public class LispFunctionEnteredEventArgs : LispExecutionEventArgs
    {
        public LispStackFrame Frame { get; }
        public LispObject[] FunctionArguments { get; }

        public LispFunctionEnteredEventArgs(LispStackFrame frame, LispObject[] functionArguments)
        {
            Frame = frame;
            FunctionArguments = functionArguments;
        }
    }
}
