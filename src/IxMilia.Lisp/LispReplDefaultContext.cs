using System.Threading;
using System.Threading.Tasks;

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
        public Task<LispObject> Trace(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            var tracedList = _repl.Trace(args);
            return Task.FromResult<LispObject>(LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:QUOTE"), tracedList));
        }

        [LispMacro("UNTRACE")]
        public Task<LispObject> Untrace(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            var untracedList = _repl.Untrace(args);
            return Task.FromResult<LispObject>(LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:QUOTE"), untracedList));
        }
    }
}
