using System;

namespace IxMilia.Lisp
{
    public class LispErrorOccuredEventArgs : EventArgs
    {
        public LispError Error { get; }
        public LispStackFrame StackFrame { get; }

        public LispErrorOccuredEventArgs(LispError error, LispStackFrame stackFrame)
        {
            Error = error;
            StackFrame = stackFrame;
        }
    }
}
