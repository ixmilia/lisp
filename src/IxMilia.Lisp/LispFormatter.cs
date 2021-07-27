using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace IxMilia.Lisp
{
    public static class LispFormatter
    {
        public static IEnumerable<ILispFormatToken> GetFormatTokens(string s)
        {
            if (s is null)
            {
                throw new ArgumentNullException(nameof(s));
            }

            var lastTokenStart = 0;
            var lastArgumentStart = 0;
            var isEscaped = false;
            for (int i = 0; i < s.Length; i++)
            {
                var c = s[i];
                var tokenLength = i - lastTokenStart;
                if (isEscaped)
                {
                    var argumentLength = i - lastArgumentStart;
                    switch (c)
                    {
                        case '%':
                        case '&':
                        case 'A':
                        case 'S':
#if DEBUG
                            var tokenText = s.Substring(lastTokenStart, tokenLength + 1);
                            var argumentText = s.Substring(lastArgumentStart, argumentLength);
#endif
                            yield return new LispEscapeSequenceFormatToken(lastTokenStart, tokenLength + 1, c, lastArgumentStart, argumentLength);
                            lastTokenStart = i + 1;
                            isEscaped = false;
                            break;
                        default:
                            break;
                    }
                }
                else
                {
                    if (c == '~')
                    {
                        isEscaped = true;
                        lastArgumentStart = i + 1;
                        if (tokenLength > 0)
                        {
#if DEBUG
                            var tokenText = s.Substring(lastTokenStart, tokenLength);
#endif
                            yield return new LispRawFormatToken(lastTokenStart, tokenLength);
                        }

                        lastTokenStart = i;
                    }
                    else
                    {

                    }
                }
            }

            if (lastTokenStart < s.Length)
            {
#if DEBUG
                var tokenText = s.Substring(lastTokenStart);
#endif
                yield return new LispRawFormatToken(lastTokenStart, s.Length - lastTokenStart);
            }
        }

        public static bool TryFormatString(string s, IEnumerable<LispObject> args, out string result)
        {
            var sb = new StringBuilder();
            var argList = args?.ToList() ?? new List<LispObject>();
            var argIndex = 0;
            var tokens = GetFormatTokens(s);
            foreach (var token in tokens)
            {
                switch (token)
                {
                    case LispRawFormatToken _:
                        sb.Append(token.GetString(s));
                        break;
                    case LispEscapeSequenceFormatToken formatToken:
                        switch (formatToken.EscapeCode)
                        {
                            case '%':
                                sb.Append('\n');
                                break;
                            case '&':
                                var isAtStart = sb.Length == 0 || sb[sb.Length - 1] == '\n';
                                if (!isAtStart)
                                {
                                    sb.Append('\n');
                                }
                                break;
                            case 'A':
                            case 'S':
                                var useEscapeCharacters = formatToken.EscapeCode == 'S';
                                if (argIndex >= argList.Count)
                                {
                                    result = "Not enough arguments";
                                    return false;
                                }

                                var arg = argList[argIndex];
                                argIndex++;
                                sb.Append(arg.ToString(useEscapeCharacters));
                                break;
                            default:
                                result = $"Unsupported format string escape character '{formatToken.EscapeCode}'";
                                return false;
                        }
                        break;
                }
            }

            result = sb.ToString();
            return true;
        }
    }
}
