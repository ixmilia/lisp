using System;

namespace IxMilia.Lisp
{
    public class LispFunctionReturnedEventArgs : EventArgs
    {
        public LispStackFrame Frame { get; }
        public LispObject ReturnValue { get; }

        public LispFunctionReturnedEventArgs(LispStackFrame frame, LispObject returnValue)
        {
            Frame = frame;
            ReturnValue = returnValue;
        }
    }
}
