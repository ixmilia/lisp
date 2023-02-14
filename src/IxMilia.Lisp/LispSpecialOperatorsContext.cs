using System.Linq;
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

        [LispSpecialOperator("SETF")]
        [LispSpecialOperator("SETQ")]
        public Task SetValue(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length % 2 != 0)
            {
                executionState.ReportError(new LispError("Expected even number of arguments"), null);
                return Task.CompletedTask;
            }

            for (int i = args.Length - 1; i >= 0; i -= 2)
            {
                var destination = args[i - 1];
                var unevaluatedValue = args[i];

                executionState.InsertOperation(new LispEvaluatorSetValue());
                executionState.InsertOperation(new LispEvaluatorObjectExpression(unevaluatedValue));
                if (destination is LispSymbol symbol)
                {
                    var resolvedSymbol = symbol.Resolve(host.CurrentPackage);
                    executionState.InsertOperation(new LispEvaluatorPushToArgumentStack(resolvedSymbol));
                }
                else
                {
                    executionState.InsertOperation(new LispEvaluatorObjectExpression(destination));
                }
            }

            return Task.CompletedTask;
        }

        [LispSpecialOperator("FUNCALL")]
        public Task FunCall(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length > 0)
            {
                executionState.InsertOperation(new LispEvaluatorFunCall(args.Length - 1));
                for (var i = args.Length - 1; i >= 0; i--)
                {
                    executionState.InsertOperation(new LispEvaluatorObjectExpression(args[i]));
                }
            }

            return Task.CompletedTask;
        }

        [LispSpecialOperator("EVAL")]
        public Task Eval(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 1)
            {
                executionState.InsertOperation(new LispEvaluatorSetStackFrame(executionState.StackFrame));
                executionState.InsertOperation(new LispEvaluatorSetStackFrame(executionState.StackFrame.Root));
                executionState.InsertOperation(new LispEvaluatorPopArgument(pushAsOperation: true, operationInsertDepth: 1));
                executionState.InsertOperation(new LispEvaluatorObjectExpression(args[0]));
            }
            else
            {
                executionState.ReportError(new LispError("Expected exactly one expression"));
            }

            return Task.CompletedTask;
        }
    }
}
