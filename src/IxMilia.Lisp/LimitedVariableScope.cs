using System;

namespace IxMilia.Lisp
{
    internal class LimitedVariableScope : IDisposable
    {
        public LispResolvedSymbol Symbol { get; }
        public LispStackFrame StackFrame { get; }
        public LispObject OriginalValue { get; }

        public LimitedVariableScope(LispResolvedSymbol symbol, LispStackFrame stackFrame, LispObject newValue)
        {
            Symbol = symbol;
            StackFrame = stackFrame;
            OriginalValue = stackFrame.GetValue(symbol);
            stackFrame.SetValue(symbol, newValue);
        }

        public void Dispose()
        {
            StackFrame.SetValue(Symbol, OriginalValue);
        }
    }
}
