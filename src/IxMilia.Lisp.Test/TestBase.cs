using System;
using System.Text;

namespace IxMilia.Lisp.Test
{
    public abstract class TestBase
    {
        protected static string NormalizeNewlines(string value)
        {
            return value.Replace("\r", "");
        }

        protected static void GetCodeAndPosition(string code, out string resultCode, out int line, out int column)
        {
            var markerIndex = code.IndexOf("$$");
            if (markerIndex < 0)
            {
                throw new Exception("Position marker '$$' not found");
            }

            var sb = new StringBuilder();
            sb.Append(code.Substring(0, markerIndex));
            sb.Append(code.Substring(markerIndex + 2));
            resultCode = sb.ToString();
            line = 1;
            column = 1;
            for (int i = 0; i < markerIndex; i++)
            {
                var c = code[i];
                switch (c)
                {
                    case '\n':
                        line++;
                        column = 1;
                        break;
                    default:
                        column++;
                        break;
                }
            }
        }
    }
}
