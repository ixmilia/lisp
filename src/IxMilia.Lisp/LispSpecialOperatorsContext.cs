namespace IxMilia.Lisp
{
    public class LispSpecialOperatorsContext
    {
        [LispSpecialOperator("PROGN")]
        public void ProgN(LispExecutionState executionState, LispObject[] args)
        {
            for (int i = args.Length - 1; i >= 0; i--)
            {
                executionState.InsertOperation(new LispEvaluatorObjectExpression(args[i]));
                if (i != 0)
                {
                    executionState.InsertOperation(new LispEvaluatorPopArgument());
                }
            }
        }

        [LispSpecialOperator("QUOTE")]
        public void Quote(LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate argument count
            executionState.PushArgument(args[0]);
        }
    }
}
