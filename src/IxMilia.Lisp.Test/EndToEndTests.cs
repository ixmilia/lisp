using System.IO;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class EndToEndTests
    {
        private static void EvalFile(string fileName)
        {
            var contents = File.ReadAllText(fileName);
            var host = new LispHost();
            host.AddMacro("assert", (h, args) =>
            {
                if (args.Length != 2)
                {
                    return new LispError("Expected 2 arguments");
                }

                var condition = h.Eval(args[0]);
                if (condition.Equals(host.T))
                {
                    return condition;
                }
                else
                {
                    var message = h.Eval(args[1]);
                    return new LispError($"Assert failed: {message}\nWith value: {condition}");
                }
            });
            var result = host.Eval(contents);
            Assert.True(result.Equals(host.T), result.ToString());
        }

        [Fact]
        public void Runtime()
        {
            EvalFile("runtime-tests.lisp");
        }
    }
}
