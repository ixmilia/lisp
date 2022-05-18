using System;

namespace IxMilia.Lisp
{
    public struct LispToken : IEquatable<LispToken>
    {
        public LispTokenType Type { get; }
        public LispSourcePosition Start { get; }
        public LispSourcePosition End { get; }

        public LispToken(LispTokenType type, LispSourcePosition start, LispSourcePosition end)
        {
            Type = type;
            Start = start;
            End = end;
        }

        public override string ToString()
        {
            return $"{Type}[{Start} - {End}]";
        }

        public override bool Equals(object obj)
        {
            return obj is LispToken token && Equals(token);
        }

        public bool Equals(LispToken other)
        {
            return this == other;
        }

        public override int GetHashCode()
        {
            int hashCode = 87782494;
            hashCode = hashCode * -1521134295 + Type.GetHashCode();
            hashCode = hashCode * -1521134295 + Start.GetHashCode();
            hashCode = hashCode * -1521134295 + End.GetHashCode();
            return hashCode;
        }

        public static bool operator ==(LispToken a, LispToken b)
        {
            return a.Type == b.Type
                && a.Start == b.Start
                && a.End == b.End;
        }

        public static bool operator !=(LispToken a, LispToken b)
        {
            return !(a == b);
        }
    }
}
