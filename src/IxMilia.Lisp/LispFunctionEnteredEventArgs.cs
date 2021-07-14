using System;

namespace IxMilia.Lisp
{
    public class LispFunctionEnteredEventArgs : EventArgs
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
