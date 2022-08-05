using System;
using System.Collections.Generic;
using System.Text;

namespace IxMilia.Lisp
{
    internal interface ILispFormatToken
    {
        int Offset { get; }
        int Length { get; }
    }

    internal interface ISimpleLispFormatToken : ILispFormatToken
    {
    }

    internal interface IEscapeSequenceFormatToken : ILispFormatToken
    {
        string GetText(StringBuilder builder);
    }

    internal interface ITakesObjectFormatToken : ILispFormatToken
    {
        string GetText(LispObject argument);
    }

    internal static class ILispFormatTokenExtensions
    {
        private struct CharComparer : IEqualityComparer<char>
        {
            public bool Equals(char x, char y)
            {
                return char.ToLowerInvariant(x) == char.ToLowerInvariant(y);
            }

            public int GetHashCode(char obj)
            {
                return char.ToLowerInvariant(obj).GetHashCode();
            }
        }

        internal static HashSet<char> EscapeCodes = new HashSet<char>(new CharComparer())
        {
            LispFormatTokenTilde.EscapeCode,
            LispFormatTokenNewLine.EscapeCode,
            LispFormatTokenLineStart.EscapeCode,
            LispFormatTokenAExpression.EscapeCode,
            LispFormatTokenSExpression.EscapeCode,
        };

        public static string GetTokenText(this ILispFormatToken token, string formatString)
        {
            return formatString.Substring(token.Offset, token.Length);
        }

        public static string GetText(this ISimpleLispFormatToken token, string formatString)
        {
            return token.GetTokenText(formatString);
        }

        public static string GetTokenArgumentText(this IEscapeSequenceFormatToken token, string formatString)
        {
            return formatString.Substring(token.Offset + 1, token.Length - 2);
        }

        public static bool TryBuildEscapeSequenceFormatToken(char escapeCode, string formatString, int offset, int length, out ILispFormatToken token, out string errorMessage)
        {
            token = default;
            switch (char.ToLowerInvariant(escapeCode))
            {
                case LispFormatTokenTilde.EscapeCode:
                    if (LispFormatTokenTilde.TryCreateTildeToken(formatString, offset, length, out var tildeToken, out errorMessage))
                    {
                        token = tildeToken;
                        return true;
                    }
                    break;
                case LispFormatTokenNewLine.EscapeCode:
                    if (LispFormatTokenNewLine.TryCreateNewLineToken(formatString, offset, length, out var newlineToken, out errorMessage))
                    {
                        token = newlineToken;
                        return true;
                    }
                    break;
                case LispFormatTokenLineStart.EscapeCode:
                    if (LispFormatTokenLineStart.TryCreateLineStartToken(formatString, offset, length, out var lineStartToken, out errorMessage))
                    {
                        token = lineStartToken;
                        return true;
                    }
                    break;
                case LispFormatTokenAExpression.EscapeCode:
                    if (LispFormatTokenAExpression.TryCreateAExpressionToken(formatString, offset, length, out var aToken, out errorMessage))
                    {
                        token = aToken;
                        return true;
                    }
                    break;
                case LispFormatTokenSExpression.EscapeCode:
                    if (LispFormatTokenSExpression.TryCreateSExpressionToken(formatString, offset, length, out var sToken, out errorMessage))
                    {
                        token = sToken;
                        return true;
                    }
                    break;
                default:
                    errorMessage = $"Unsuppored escape code '{escapeCode}'";
                    break;
            }

            return false;
        }
    }

    internal struct LispLiteralFormatToken : ISimpleLispFormatToken
    {
        public int Offset { get; }
        public int Length { get; }

        public LispLiteralFormatToken(int offset, int length)
        {
            Offset = offset;
            Length = length;
        }
    }

    internal struct LispFormatTokenTilde : IEscapeSequenceFormatToken
    {
        public const char EscapeCode = '~';

        public int Offset { get; }
        public int Length { get; }
        public int Count { get; }

        private LispFormatTokenTilde(int offset, int length, int count)
        {
            Offset = offset;
            Length = length;
            Count = count;
        }

        public string GetText(StringBuilder builder)
        {
            return new string('~', Count);
        }

        public static bool TryCreateTildeToken(string formatString, int offset, int length, out LispFormatTokenTilde token, out string errorMessage)
        {
            token = default;
            errorMessage = default;
            var argumentString = formatString.Substring(offset + 1, length - 2);
            var count = 1;
            if (argumentString.Length > 0)
            {
                if (!int.TryParse(argumentString, out count))
                {
                    errorMessage = $"Unable to parse argument string '{argumentString}'";
                    return false;
                }
            }

            token = new LispFormatTokenTilde(offset, length, count);
            return true;
        }
    }

    internal struct LispFormatTokenNewLine : IEscapeSequenceFormatToken
    {
        public const char EscapeCode = '%';

        public int Offset { get; }
        public int Length { get; }

        private LispFormatTokenNewLine(int offset, int length)
        {
            Offset = offset;
            Length = length;
        }

        public string GetText(StringBuilder builder)
        {
            return "\n";
        }

        public static bool TryCreateNewLineToken(string formatString, int offset, int length, out LispFormatTokenNewLine token, out string errorMessage)
        {
            token = default;
            errorMessage = default;
            if (length != 2)
            {
                errorMessage = "Expected no format arguments";
                return false;
            }

            token = new LispFormatTokenNewLine(offset, length);
            return true;
        }
    }

    internal struct LispFormatTokenLineStart : IEscapeSequenceFormatToken
    {
        public const char EscapeCode = '&';

        public int Offset { get; }
        public int Length { get; }

        private LispFormatTokenLineStart(int offset, int length)
        {
            Offset = offset;
            Length = length;
        }

        public string GetText(StringBuilder builder)
        {
            if (builder.Length == 0 || builder[builder.Length - 1] != '\n')
            {
                return "\n";
            }

            return "";
        }

        public static bool TryCreateLineStartToken(string formatString, int offset, int length, out LispFormatTokenLineStart token, out string errorMessage)
        {
            token = default;
            errorMessage = default;
            if (length != 2)
            {
                errorMessage = "Expected no format arguments";
                return false;
            }

            token = new LispFormatTokenLineStart(offset, length);
            return true;
        }
    }

    internal struct LispFormatTokenAExpression : ITakesObjectFormatToken
    {
        public const char EscapeCode = 'a';

        public int Offset { get; }
        public int Length { get; }
        public int Width { get; }

        private LispFormatTokenAExpression(int offset, int length, int width)
        {
            Offset = offset;
            Length = length;
            Width = width;
        }

        public string GetText(LispObject argument)
        {
            var sb = new StringBuilder();
            sb.Append(argument.ToString(useEscapeCharacters: false));
            var paddingWidth = Math.Max(0, Width - sb.Length);
            if (paddingWidth > 0)
            {
                sb.Append(new string(' ', paddingWidth));
            }

            return sb.ToString();
        }

        public static bool TryCreateAExpressionToken(string formatString, int offset, int length, out LispFormatTokenAExpression token, out string errorMessage)
        {
            token = default;
            errorMessage = default;
            var argumentString = formatString.Substring(offset + 1, length - 2);
            var width = 0;
            if (argumentString.Length > 0)
            {
                if (!int.TryParse(argumentString, out width))
                {
                    errorMessage = $"Unable to parse argument string '{argumentString}'";
                    return false;
                }
            }

            token = new LispFormatTokenAExpression(offset, length, width);
            return true;
        }
    }

    internal struct LispFormatTokenSExpression : ITakesObjectFormatToken
    {
        public const char EscapeCode = 's';

        public int Offset { get; }
        public int Length { get; }
        public int Width { get; }

        private LispFormatTokenSExpression(int offset, int length, int width)
        {
            Offset = offset;
            Length = length;
            Width = width;
        }

        public string GetText(LispObject argument)
        {
            var sb = new StringBuilder();
            sb.Append(argument.ToString(useEscapeCharacters: true));
            var paddingWidth = Math.Max(0, Width - sb.Length);
            if (paddingWidth > 0)
            {
                sb.Append(new string(' ', paddingWidth));
            }

            return sb.ToString();
        }

        public static bool TryCreateSExpressionToken(string formatString, int offset, int length, out LispFormatTokenSExpression token, out string errorMessage)
        {
            token = default;
            errorMessage = default;
            var argumentString = formatString.Substring(offset + 1, length - 2);
            var width = 0;
            if (argumentString.Length > 0)
            {
                if (!int.TryParse(argumentString, out width))
                {
                    errorMessage = $"Unable to parse argument string '{argumentString}'";
                    return false;
                }
            }

            token = new LispFormatTokenSExpression(offset, length, width);
            return true;
        }

        public static string FormatObject(LispObject obj)
        {
            var tokenSExpression = new LispFormatTokenSExpression(0, 0, 0);
            var result = tokenSExpression.GetText(obj);
            return result;
        }
    }
}
