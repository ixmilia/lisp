namespace IxMilia.Lisp
{
    internal class LispObjectReaderResult
    {
        public LispObject LastResult { get; }
        public string IncompleteInput { get; }
        public int ExpressionDepth { get; }

        public LispObjectReaderResult(LispObject lastResult, string incompleteInput, int expressionDepth)
        {
            LastResult = lastResult;
            IncompleteInput = incompleteInput;
            ExpressionDepth = expressionDepth;
        }
    }
}
