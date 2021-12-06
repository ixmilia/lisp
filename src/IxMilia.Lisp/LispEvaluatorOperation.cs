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
        public LispSourceLocation? SourceLocation { get; }
        public int ArgumentCount { get; }

        public LispEvaluatorInvocation(LispInvocableObject invocationObject, LispSourceLocation? sourceLocation, int argumentCount)
        {
            InvocationObject = invocationObject;
            SourceLocation = sourceLocation;
            ArgumentCount = argumentCount;
        }

        public override string ToString()
        {
            return $"i: {InvocationObject.Name}/{ArgumentCount}{(SourceLocation.HasValue ? $" at {SourceLocation}" : "")}";
        }
    }

    internal class LispEvaluatorFunctionExit : ILispEvaluatorOperation
    {
        public LispFunction Function { get; }
        public LispObject InvocationExpression { get; }
        public LispSourceLocation? SourceLocation { get; }
        public bool PopFrame { get; }

        public LispEvaluatorFunctionExit(LispFunction function, LispObject invocationExpression, LispSourceLocation? sourceLocation, bool popFrame = true)
        {
            Function = function;
            InvocationExpression = invocationExpression;
            SourceLocation = sourceLocation;
            PopFrame = popFrame;
        }

        public LispEvaluatorFunctionExit WithoutFramePop()
        {
            return new LispEvaluatorFunctionExit(Function, InvocationExpression, SourceLocation, popFrame: false);
        }

        public override string ToString()
        {
            return $"r: {Function.Name} {(PopFrame ? "with" : "without")} pop";
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
