﻿using IxMilia.Lisp.DebugAdapter.Protocol;

namespace IxMilia.Lisp.DebugAdapter
{
    internal abstract class BreakReason
    {
    }

    internal class FunctionBreakReason : BreakReason
    {
        public Breakpoint Breakpoint { get; }

        public FunctionBreakReason(Breakpoint breakpoint)
        {
            Breakpoint = breakpoint;
        }
    }

    internal class ErrorBreakReason : BreakReason
    {
        public LispError Error { get;  }

        public ErrorBreakReason(LispError error)
        {
            Error = error;
        }
    } 
}
