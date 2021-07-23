using System.Text;

namespace IxMilia.Lisp
{
    public static class LispFormatter
    {
        public static bool TryFormatString(string s, out string result)
        {
            var sb = new StringBuilder();
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
