namespace IxMilia.Lisp
{
    public class LispMacroExpandedEventArgs : LispExecutionEventArgs
    {
        public LispMacro Macro { get; }
        public LispStackFrame Frame { get; }
        public LispObject[] ExpandedBody { get; }

        public LispMacroExpandedEventArgs(LispMacro macro, LispStackFrame frame, LispObject[] expandedBody)
        {
            Macro = macro;
            Frame = frame;
            ExpandedBody = expandedBody;
        }
    }
}
