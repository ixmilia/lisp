using System;
using System.Collections.Generic;

namespace IxMilia.Lisp
{
    internal interface ILispEvaluatorOperation
    {
    }

    internal class LispEvaluatorInvocation : ILispEvaluatorOperation
    {
        public LispInvocableObject InvocationObject { get; }
        public int ArgumentCount { get; }

        public LispEvaluatorInvocation(LispInvocableObject invocationObject, int argumentCount)
        {
            InvocationObject = invocationObject;
            ArgumentCount = argumentCount;
        }

        public override string ToString()
        {
            return $"i: {InvocationObject.NameSymbol.LocalName}/{ArgumentCount}";
        }
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
        public override string ToString()
        {
            return "a: pop";
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
