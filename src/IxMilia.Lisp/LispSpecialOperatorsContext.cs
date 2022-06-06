using System.Threading;
using System.Threading.Tasks;

namespace IxMilia.Lisp
{
    public class LispSpecialOperatorsContext
    {
        [LispSpecialOperator("PROGN")]
        public Task ProgN(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            for (int i = args.Length - 1; i >= 0; i--)
            {
                executionState.InsertOperation(new LispEvaluatorObjectExpression(args[i]));
                if (i != 0)
                {
                    executionState.InsertOperation(new LispEvaluatorPopArgument());
                }
            }

            return Task.CompletedTask;
        }

        [LispSpecialOperator("QUOTE")]
        public Task Quote(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: validate argument count
            executionState.PushArgument(args[0]);

            return Task.CompletedTask;
        }
    }
}
