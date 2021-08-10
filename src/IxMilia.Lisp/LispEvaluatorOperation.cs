using System;
using System.Collections.Generic;

namespace IxMilia.Lisp
{
    internal interface ILispEvaluatorOperation
    {
    }

    internal class LispEvaluatorInvocation : ILispEvaluatorOperation
    {
        public LispMacroOrFunction InvocationObject { get; }
        public LispSourceLocation? SourceLocation { get; }
        public int ArgumentCount { get; }

        public LispEvaluatorInvocation(LispMacroOrFunction invocationObject, LispSourceLocation? sourceLocation, int argumentCount)
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

    internal class LispEvaluatorInvocationExit : ILispEvaluatorOperation
    {
        public LispMacroOrFunction InvocationObject { get; }
        public LispSourceLocation? InvocationLocation { get; }
        public bool PopFrame { get; }

        public LispEvaluatorInvocationExit(LispMacroOrFunction invocationObject, LispSourceLocation? invocationLocation, bool popFrame = true)
        {
            InvocationObject = invocationObject;
            InvocationLocation = invocationLocation;
            PopFrame = popFrame;
        }

        public LispEvaluatorInvocationExit WithoutFramePop()
        {
            return new LispEvaluatorInvocationExit(InvocationObject, InvocationLocation, popFrame: false);
        }

        public override string ToString()
        {
            return $"r: {InvocationObject.Name} {(PopFrame ? "with" : "without")} pop";
        }
    }

    internal class LispEvaluatorPopForTailCall : ILispEvaluatorOperation
    {
        public LispMacroOrFunction InvocationObject { get; }
        public HashSet<string> InvocationArgumentNames { get; }

        public LispEvaluatorPopForTailCall(LispMacroOrFunction invocationObject, IEnumerable<string> invocationArgumentNames)
        {
            InvocationObject = invocationObject;
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
