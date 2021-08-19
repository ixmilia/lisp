using System;

namespace IxMilia.Lisp
{
    public class LispEvaluationHaltedEventArgs : EventArgs
    {
        public LispEvaluationState EvaluationState { get; }

        public LispEvaluationHaltedEventArgs(LispEvaluationState evaluationState)
        {
            EvaluationState = evaluationState;
        }
    }
}
