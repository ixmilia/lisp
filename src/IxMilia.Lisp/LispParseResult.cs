using System.Collections.Generic;

namespace IxMilia.Lisp
{
    public class LispParseResult
    {
        public IEnumerable<LispObject> Nodes { get; }
        public int ParseDepth { get; }

        public LispParseResult(IEnumerable<LispObject> nodes, int parseDepth)
        {
            Nodes = nodes;
            ParseDepth = parseDepth;
        }
    }
}
