using System;
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

        //[LispSpecialOperator("EVAL")]
        //public Task Eval(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        //{
        //    if (args.Length != 1)
        //    {
        //        executionState.ReportError(new LispError("Expected expression"), null);
        //        return Task.CompletedTask;
        //    }

        //    executionState.InsertOperation(new LispEvaluatorObjectExpression(args[0]));
        //    return Task.CompletedTask;
        //}

        [LispSpecialOperator("IF")]
        public Task If(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length != 3)
            {
                executionState.ReportError(new LispError("Expected predicate, t-value, nil-value"), null);
                return Task.CompletedTask;
            }

            var predicate = args[0];
            var tExpression = args[1];
            var nilExpression = args[2];
            executionState.InsertOperation(new LispEvaluatorIfPredicate(tExpression, nilExpression));
            executionState.InsertOperation(new LispEvaluatorObjectExpression(predicate));
            return Task.CompletedTask;
        }
    }
}
