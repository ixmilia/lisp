using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace IxMilia.Lisp
{
    public static class LispFormatter
    {
        internal static bool TryGetFormatTokens(string s, out IList<ILispFormatToken> tokens, out string errorMessage)
        {
            if (s is null)
            {
                throw new ArgumentNullException(nameof(s));
            }

            tokens = new List<ILispFormatToken>();
            errorMessage = default;
            var lastTokenStart = 0;
            var isEscaped = false;
            for (int i = 0; i < s.Length; i++)
            {
                var c = s[i];
                var tokenLength = i - lastTokenStart;
                if (isEscaped)
                {
                    if (ILispFormatTokenExtensions.EscapeCodes.Contains(c))
                    {
                        if (ILispFormatTokenExtensions.TryBuildEscapeSequenceFormatToken(c, s, lastTokenStart, tokenLength + 1, out var escapedToken, out errorMessage))
                        {
                            tokens.Add(escapedToken);
                            lastTokenStart = i + 1;
                            isEscaped = false;
                        }
                        else
                        {
                            return false;
                        }
                    }
                    else
                    {
                        // still building the format string
                    }
                }
                else
                {
                    if (c == '~')
                    {
                        isEscaped = true;
                        if (tokenLength > 0)
                        {
                            tokens.Add(new LispLiteralFormatToken(lastTokenStart, tokenLength));
                        }

                        lastTokenStart = i;
                    }
                    else
                    {
                        // still building the literal
                    }
                }
            }

            if (lastTokenStart < s.Length)
            {
                tokens.Add(new LispLiteralFormatToken(lastTokenStart, s.Length - lastTokenStart));
            }

            return true;
        }

        public static bool TryFormatString(string s, IEnumerable<LispObject> args, out string result)
        {
            var sb = new StringBuilder();
            var argList = args?.ToList() ?? new List<LispObject>();
            var argIndex = 0;
            if (!TryGetFormatTokens(s, out var tokens, out result))
            {
                return false;
            }

            foreach (var token in tokens)
            {
                switch (token)
                {
                    case ISimpleLispFormatToken simple:
                        sb.Append(simple.GetText(s));
                        break;
                    case IEscapeSequenceFormatToken escape:
                        sb.Append(escape.GetText(sb));
                        break;
                    case ITakesObjectFormatToken takes:
                        if (argIndex >= argList.Count)
                        {
                            result = "Not enough arguments";
                            return false;
                        }

                        var argument = argList[argIndex];
                        argIndex++;
                        sb.Append(takes.GetText(argument));
                        break;
                    default:
                        result = $"Unexpected format token type '{token.GetType().Name}'";
                        return false;
                }
            }

            result = sb.ToString();
            return true;
        }
    }
}
