using System.Collections.Generic;
using IxMilia.Lisp.Tokens;

namespace IxMilia.Lisp
{
    public class LispParseResult
    {
        public IEnumerable<LispObject> Nodes { get; }
        public int ParseDepth { get; }
        public IEnumerable<LispToken> RemainingTokens { get; }

        public LispParseResult(IEnumerable<LispObject> nodes, int parseDepth, IEnumerable<LispToken> remainingTokens)
        {
            Nodes = nodes;
            ParseDepth = parseDepth;
            RemainingTokens = remainingTokens;
        }
    }
}
