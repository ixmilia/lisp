using System;
using System.Text;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public abstract class TestBase
    {
        protected static LispObject Eval(string code)
        {
            var host = new LispHost();
            var evalResult = host.Eval(code);
            var result = evalResult.LastResult;
            Assert.NotNull(result);
            EnsureNotError(result);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            return result;
        }

        protected static void EnsureNotError(LispObject obj)
        {
            if (obj is LispError error)
            {
                Assert.True(false, $"{error.Message} at {error.SourceLocation}");
            }
        }

        protected static string NormalizeNewlines(string value)
        {
            return value.Replace("\r", "");
        }

        protected static void GetCodeAndPosition(string code, out string resultCode, out LispSourcePosition position)
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
            var line = 1;
            var column = 1;
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

            position = new LispSourcePosition(line, column);
        }
    }
}
