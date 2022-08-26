using System;
using System.Collections.Generic;
using System.Linq;

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

    internal class LispEvaluatorReturnIntermediate : ILispEvaluatorOperation
    {
        public override string ToString() => "reti";
    }

    internal class LispEvaluatorPreInvoke : ILispEvaluatorOperation
    {
        public LispObject[] Arguments { get; }

        public LispEvaluatorPreInvoke(LispObject[] arguments)
        {
            Arguments = arguments;
        }

        public override string ToString() => $"pi/{Arguments.Length}: {string.Join(", ", Arguments.Select(a => a.ToString()))}";
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

    internal class LispEvaluatorIfPredicate : ILispEvaluatorOperation
    {
        public LispObject TExpression { get; }
        public LispObject NilExpression { get; }

        public LispEvaluatorIfPredicate(LispObject tExpression, LispObject nilExpression)
        {
            TExpression = tExpression;
            NilExpression = nilExpression;
        }

        public override string ToString() => $"if: [{TExpression}] [{NilExpression}]";
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
