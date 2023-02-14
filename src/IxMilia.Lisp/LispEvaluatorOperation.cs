using System;
using System.Collections.Generic;
using System.Linq;

namespace IxMilia.Lisp
{
    internal interface ILispEvaluatorOperation
    {
    }

    internal class LispEvaluatorFunCall : ILispEvaluatorOperation
    {
        public int ArgumentCount { get; }

        public LispEvaluatorFunCall(int argumentCount)
        {
            ArgumentCount = argumentCount;
        }

        public override string ToString() => $"fc: {ArgumentCount}";
    }

    internal class LispEvaluatorInvocation : ILispEvaluatorOperation
    {
        public LispInvocableObject InvocationObject { get; }
        public int ArgumentCount { get; }
        public LispStackFrame StackFrame { get; }

        public LispEvaluatorInvocation(LispInvocableObject invocationObject, int argumentCount, LispStackFrame stackFrame = null)
        {
            InvocationObject = invocationObject;
            ArgumentCount = argumentCount;
            StackFrame = stackFrame;
        }

        public override string ToString()
        {
            return $"i: {InvocationObject.NameSymbol.LocalName}/{ArgumentCount}";
        }
    }

    internal class LispEvaluatorThrowCondition : ILispEvaluatorOperation
    {
        public LispObject Condition { get; }

        public LispEvaluatorThrowCondition(LispObject condition)
        {
            Condition = condition;
        }

        public override string ToString() => $"throw: {Condition}";
    }

    internal class LispEvaluatorHandlerCaseGuard : ILispEvaluatorOperation
    {
        public (LispResolvedSymbol typeSpec, LispResolvedSymbol argument, LispObject form)[] Handlers { get; }

        public LispEvaluatorHandlerCaseGuard((LispResolvedSymbol typeSpec, LispResolvedSymbol argument, LispObject form)[] handlers)
        {
            Handlers = handlers;
        }

        public override string ToString() => $"hc: {string.Join(" | ", Handlers.Select(h => h.typeSpec.ToString()))}";
    }

    internal class LispEvaluatorFunctionExit : ILispEvaluatorOperation
    {
        public LispFunction Function { get; }
        public LispObject InvocationExpression { get; }
        public bool PopFrame { get; }

        public LispEvaluatorFunctionExit(LispFunction function, LispObject invocationExpression, bool popFrame = true)
        {
            Function = function;
            InvocationExpression = invocationExpression;
            PopFrame = popFrame;
        }

        public LispEvaluatorFunctionExit WithoutFramePop()
        {
            return new LispEvaluatorFunctionExit(Function, InvocationExpression, popFrame: false);
        }

        public override string ToString()
        {
            return $"r: {Function.NameSymbol.LocalName} {(PopFrame ? "with" : "without")} pop";
        }
    }

    internal class LispEvaluatorPopForTailCall : ILispEvaluatorOperation
    {
        public LispInvocableObject Function { get; }
        public HashSet<string> InvocationArgumentNames { get; }

        public LispEvaluatorPopForTailCall(LispInvocableObject function, IEnumerable<string> invocationArgumentNames)
        {
            Function = function;
            InvocationArgumentNames = new HashSet<string>(invocationArgumentNames);
        }

        public override string ToString()
        {
            return "t: pop";
        }
    }

    internal class LispEvaluatorPopArgument : ILispEvaluatorOperation
    {
        public bool PushAsOperation { get; }

        public int OperationInsertDepth { get;  }

        public LispEvaluatorPopArgument(bool pushAsOperation = false, int operationInsertDepth = 0)
        {
            PushAsOperation = pushAsOperation;
            OperationInsertDepth = operationInsertDepth;
        }

        public override string ToString() => "a: pop" + (PushAsOperation ? " with push" : "");
    }

    internal class LispEvaluatorReturnImmediate : ILispEvaluatorOperation
    {
        public override string ToString() => "reti";
    }

    internal class LispEvaluatorPushToArgumentStack : ILispEvaluatorOperation
    {
        public LispObject Expression { get; }

        public LispEvaluatorPushToArgumentStack(LispObject expression)
        {
            Expression = expression;
        }

        public override string ToString()
        {
            return $"a: {Expression}";
        }
    }

    internal class LispEvaluatorObjectExpression : ILispEvaluatorOperation
    {
        public LispObject Expression { get; }

        public LispEvaluatorObjectExpression(LispObject expression)
        {
            if (expression == null)
            {
                throw new ArgumentNullException(nameof(expression));
            }

            Expression = expression;
        }

        public override string ToString()
        {
            return $"s: {Expression}";
        }
    }

    internal class LispEvaluatorSetStackFrame : ILispEvaluatorOperation
    {
        public LispStackFrame StackFrame { get; }

        public LispEvaluatorSetStackFrame(LispStackFrame stackFrame)
        {
            StackFrame = stackFrame;
        }

        public override string ToString() => $"sf: {StackFrame}";
    }

    internal class LispEvaluatorSetValue : ILispEvaluatorOperation
    {
        public LispEvaluatorSetValue()
        {
        }

        public override string ToString()
        {
            return "set";
        }
    }

    internal class LispEvaluatorDribbleEnter : ILispEvaluatorOperation
    {
        public LispObject Expression { get; }

        public LispEvaluatorDribbleEnter(LispObject expression)
        {
            if (expression == null)
            {
                throw new ArgumentNullException(nameof(expression));
            }

            Expression = expression;
        }

        public override string ToString()
        {
            return $"d: {Expression}";
        }
    }

    internal class LispEvaluatorDribbleExit : ILispEvaluatorOperation
    {
        public override string ToString()
        {
            return "d: <exit>";
        }
    }
}
