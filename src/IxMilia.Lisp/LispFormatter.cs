using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace IxMilia.Lisp
{
    public static class LispFormatter
    {
        public static bool TryFormatString(string s, IEnumerable<LispObject> args, out string result)
        {
            var sb = new StringBuilder();
            var argList = args?.ToList() ?? new List<LispObject>();
            var argIndex = 0;
            bool isAtStart = true;
            bool isEscaped = false;
            foreach (var c in s)
            {
                if (isEscaped)
                {
                    isEscaped = false;
                    switch (c)
                    {
                        case '%':
                            sb.Append('\n');
                            isAtStart = true;
                            break;
                        case '&':
                            if (!isAtStart)
                            {
                                sb.Append('\n');
                            }
                            isAtStart = true;
                            break;
                        case 'A':
                        case 'S':
                            var useEscapeCharacters = c == 'S';
                            if (argIndex >= argList.Count)
                            {
                                result = "Not enough arguments";
                                return false;
                            }

                            var arg = argList[argIndex];
                            argIndex++;
                            sb.Append(arg.ToString(useEscapeCharacters));
                            isAtStart = false;
                            break;
                        default:
                            result = $"Unsupported format string escape character '{c}'";
                            return false;
                    }
                }
                else
                {
                    if (c == '~')
                    {
                        isEscaped = true;
                    }
                    else
                    {
                        sb.Append(c);
                        isAtStart = false;
                    }
                }
            }

            result = sb.ToString();
            return true;
        }
    }
}
