namespace IxMilia.Lisp
{
    public class LispValueSetEventArgs : LispExecutionEventArgs
    {
        public string Name { get; }
        public LispObject Value { get; }
        public LispStackFrame StackFrame { get; }

        public LispValueSetEventArgs(string name, LispObject value, LispStackFrame stackFrame)
        {
            Name = name;
            Value = value;
            StackFrame = stackFrame;
        }
    }
}
