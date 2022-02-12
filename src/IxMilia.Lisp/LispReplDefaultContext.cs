﻿using System.Collections.Generic;

namespace IxMilia.Lisp
{
    internal class LispReplDefaultContext
    {
        private LispRepl _repl;

        public LispReplDefaultContext(LispRepl repl)
        {
            _repl = repl;
        }

        [LispMacro("TRACE")]
        public LispObject Trace(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            var tracedList = _repl.Trace(args);
            return LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:QUOTE"), tracedList);
        }

        [LispMacro("UNTRACE")]
        public LispObject Untrace(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            var untracedList = _repl.Untrace(args);
            return LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:QUOTE"), untracedList);
        }
    }
}
