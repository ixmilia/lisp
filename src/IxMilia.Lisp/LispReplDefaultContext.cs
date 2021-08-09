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

        [LispMacro("trace")]
        public IEnumerable<LispObject> Trace(LispStackFrame frame, LispObject[] args)
        {
            var tracedList = _repl.Trace(args);
            return new LispObject[] { new LispQuotedObject(tracedList) };
        }

        [LispMacro("untrace")]
        public IEnumerable<LispObject> Untrace(LispStackFrame frame, LispObject[] args)
        {
            var untracedList = _repl.Untrace(args);
            return new LispObject[] { new LispQuotedObject(untracedList) };
        }
    }
}
