using System.Collections.Generic;

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
        public LispObject Trace(LispStackFrame frame, LispObject[] args)
        {
            var tracedList = _repl.Trace(args);
            return LispList.FromItems(new LispSymbol("QUOTE"), tracedList);
        }

        [LispMacro("UNTRACE")]
        public LispObject Untrace(LispStackFrame frame, LispObject[] args)
        {
            var untracedList = _repl.Untrace(args);
            return LispList.FromItems(new LispSymbol("QUOTE"), untracedList);
        }
    }
}
