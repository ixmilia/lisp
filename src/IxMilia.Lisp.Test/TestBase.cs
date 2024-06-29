using System;
using System.IO;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public abstract class TestBase
    {
        /// <summary>
        /// Create a new <see cref="LispHost"/> for testing.  Calling <see cref="LispHost.CreateAsync"/> can take several seconds,
        /// so this is used to make running unit tests not horribly slow.  The "fix" is that one canonical <see cref="LispHost"/>
        /// is created and internal package values are cloned into the new test instance.
        /// </summary>
        public static async Task<LispHost> CreateHostAsync(
            TextReader input = null,
            TextWriter output = null,
            bool useTailCalls = false,
            bool useInitScript = true,
            bool useJustMyCode = true,
            Func<LispResolvedSymbol, LispObject> getUntrackedValue = null,
            Func<LispResolvedSymbol, LispObject, bool> trySetUntrackedValue = null,
            CancellationToken cancellationToken = default)
        {
            var configuration = new LispHostConfiguration(
                useInitScript: useInitScript,
                useTailCalls: useTailCalls,
                useJustMyCode: useJustMyCode,
                readerType: LispReaderType.Compiled,
                input: input,
                output: output,
                getUntrackedValue: getUntrackedValue,
                trySetUntrackedValue: trySetUntrackedValue);
            var host = await LispHost.CreateAsync(
                configuration: configuration,
                cancellationToken: cancellationToken);
            return host;
        }

        protected static async Task<LispObject> EvalAsync(string code)
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            var evalResult = await host.EvalAsync("test.lisp", code, executionState);
            var result = evalResult.Value;
            Assert.NotNull(result);
            EnsureNotError(result);
            Assert.True(executionState.IsExecutionComplete);
            return result;
        }

        protected static void EnsureNotError(LispObject obj)
        {
            if (obj is LispError error)
            {
                Assert.Fail($"{error.Message} at {error.SourceLocation}");
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
