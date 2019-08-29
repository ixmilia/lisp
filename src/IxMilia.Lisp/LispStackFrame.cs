namespace IxMilia.Lisp
{
    public class LispStackFrame
    {
        public LispStackFrame Parent { get; }
        public int Line { get; }
        public int Column { get; }

        public LispStackFrame(LispStackFrame parent, int line, int column)
        {
            Parent = parent;
            Line = line;
            Column = column;
        }

        public override string ToString()
        {
            return $"({Line}, {Column})\n{Parent}";
        }
    }
}
