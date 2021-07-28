namespace IxMilia.Lisp
{
    internal interface ILispFormatToken
    {
        int Offset { get; }
        int Length { get; }
    }

    internal static class ILispFormatTokenExtensions
    {
        public static string GetString(this ILispFormatToken token, string formatString)
        {
            return formatString.Substring(token.Offset, token.Length);
        }

        public static string GetArgument(this LispEscapeSequenceFormatToken token, string formatString)
        {
            return formatString.Substring(token.ArgumentOffset, token.ArgumentLength);
        }
    }

    internal struct LispLiteralFormatToken : ILispFormatToken
    {
        public int Offset { get; }
        public int Length { get; }

        public LispLiteralFormatToken(int offset, int length)
        {
            Offset = offset;
            Length = length;
        }
    }

    internal struct LispEscapeSequenceFormatToken : ILispFormatToken
    {
        public int Offset { get; }
        public int Length { get; }
        public char EscapeCode { get; }
        public int ArgumentOffset { get;}
        public int ArgumentLength { get; }

        public LispEscapeSequenceFormatToken(int offset, int length, char escapeCode, int argumentOffset, int argumentLength)
        {
            Offset = offset;
            Length = length;
            EscapeCode = escapeCode;
            ArgumentOffset = argumentOffset;
            ArgumentLength = argumentLength;
        }
    }
}
