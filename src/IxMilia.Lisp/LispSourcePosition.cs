using System;

namespace IxMilia.Lisp
{
    public struct LispSourcePosition : IEquatable<LispSourcePosition>
    {
        public int Line { get; }
        public int Column { get; }

        public LispSourcePosition(int line, int column)
        {
            Line = line;
            Column = column;
        }

        public static bool operator==(LispSourcePosition a, LispSourcePosition b)
        {
            return a.Line == b.Line && a.Column == b.Column;
        }

        public static bool operator !=(LispSourcePosition a, LispSourcePosition b)
        {
            return !(a == b);
        }

        public static bool operator <(LispSourcePosition a, LispSourcePosition b)
        {
            return (a.Line < b.Line) || ((a.Line == b.Line) && (a.Column < b.Column));
        }

        public static bool operator >(LispSourcePosition a, LispSourcePosition b)
        {
            return (a.Line > b.Line) || ((a.Line == b.Line) && (a.Column > b.Column));
        }

        public static bool operator <=(LispSourcePosition a, LispSourcePosition b)
        {
            return (a < b) || (a == b);
        }

        public static bool operator >=(LispSourcePosition a, LispSourcePosition b)
        {
            return (a > b) || (a == b);
        }

        public override string ToString()
        {
            return $"({Line}, {Column})";
        }

        public override bool Equals(object obj)
        {
            if (obj is LispSourcePosition other)
            {
                return this == other;
            }

            return false;
        }

        public bool Equals(LispSourcePosition other)
        {
            return Line == other.Line &&
                   Column == other.Column;
        }

        public override int GetHashCode()
        {
            int hashCode = -1456208474;
            hashCode = hashCode * -1521134295 + Line.GetHashCode();
            hashCode = hashCode * -1521134295 + Column.GetHashCode();
            return hashCode;
        }
    }
}
