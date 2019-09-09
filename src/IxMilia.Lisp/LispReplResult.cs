namespace IxMilia.Lisp
{
    public class LispReplResult
    {
        public LispObject LastValue { get; }
        public int ExpressionDepth { get; }

        public LispReplResult(LispObject lastValue, int expressionDepth)
        {
            LastValue = lastValue;
            ExpressionDepth = expressionDepth;
        }
    }
}
