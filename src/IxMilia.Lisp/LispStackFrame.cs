namespace IxMilia.Lisp
{
    public class LispStackFrame
    {
        public string FunctionName { get; }
        public LispStackFrame Parent { get; }
        public int Line { get; internal set; }
        public int Column { get; internal set; }

        public LispStackFrame(string functionName, LispStackFrame parent)
        {
            FunctionName = functionName;
            Parent = parent;
        }

        public override string ToString()
        {
            return $"  at {FunctionName}: ({Line}, {Column})\n{Parent}";
        }
    }
}
