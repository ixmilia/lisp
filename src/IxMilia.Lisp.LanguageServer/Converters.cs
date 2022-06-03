using System;
using IxMilia.Lisp.LanguageServer.Protocol;

namespace IxMilia.Lisp.LanguageServer
{
    public class Converters
    {
        public static Position PositionFromSourcePosition(LispSourcePosition position)
        {
            return new Position((uint)(position.Line - 1), (uint)(position.Column - 1));
        }

        public static Range RangeFromSoureLocation(LispSourceLocation location)
        {
            return new Range(PositionFromSourcePosition(location.Start), PositionFromSourcePosition(location.End));
        }

        public static LispSourcePosition SourcePositionFromPosition(Position position)
        {
            return new LispSourcePosition((int)position.Line + 1, (int)position.Character + 1);
        }
    }
}
