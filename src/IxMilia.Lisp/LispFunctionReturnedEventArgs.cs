using System;

namespace IxMilia.Lisp
{
    public class LispFunctionReturnedEventArgs : EventArgs
    {
        public LispMacroOrFunction InvocationObject { get; }
        public LispStackFrame Frame { get; }
        public LispObject ReturnValue { get; }

        public LispFunctionReturnedEventArgs(LispMacroOrFunction invocationObject, LispStackFrame frame, LispObject returnValue)
        {
            InvocationObject = invocationObject;
            Frame = frame;
            ReturnValue = returnValue;
        }
    }
}
