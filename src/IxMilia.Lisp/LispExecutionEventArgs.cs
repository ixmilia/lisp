using System;

namespace IxMilia.Lisp
{
    public abstract class LispExecutionEventArgs : EventArgs
    {
        public bool HaltExecution { get; set; }

        protected LispExecutionEventArgs()
        {
            HaltExecution = false;
        }
    }
}
