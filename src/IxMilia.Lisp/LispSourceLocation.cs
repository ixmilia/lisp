using System;
using System.Collections.Generic;

namespace IxMilia.Lisp
{
    public struct LispSourceLocation : IEquatable<LispSourceLocation>
    {
        public string FilePath { get; }
        public LispSourcePosition Start { get; }
        public LispSourcePosition End { get; }

        public LispSourceLocation(string filePath, LispSourcePosition start, LispSourcePosition end)
        {
            FilePath = filePath;
            Start = start;
            End = end;
        }

        public bool ContainsPosition(LispSourcePosition position)
        {
            return position >= Start && position <= End;
        }

        public static bool operator==(LispSourceLocation a, LispSourceLocation b)
        {
            return a.FilePath == b.FilePath && a.Start == b.Start && a.End == b.End;
        }

        public static bool operator !=(LispSourceLocation a, LispSourceLocation b)
        {
            return !(a == b);
        }

        public override string ToString()
        {
            return $"{FilePath}: [{Start}-{End})";
        }

        public override bool Equals(object obj)
        {
            if (obj is LispSourceLocation other)
            {
                return this == other;
            }

            return false;
        }

        public bool Equals(LispSourceLocation other)
        {
            return FilePath == other.FilePath &&
                   Start.Equals(other.Start) &&
                   End.Equals(other.End);
        }

        public override int GetHashCode()
        {
            int hashCode = -731373115;
            hashCode = hashCode * -1521134295 + EqualityComparer<string>.Default.GetHashCode(FilePath);
            hashCode = hashCode * -1521134295 + Start.GetHashCode();
            hashCode = hashCode * -1521134295 + End.GetHashCode();
            return hashCode;
        }
    }
}
