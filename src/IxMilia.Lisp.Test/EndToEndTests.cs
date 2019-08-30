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
            host.AddFunction("assert", (h, args) =>
            {
                if (args.Length != 2)
                {
                    return new LispError("Expected 2 arguments");
                }

                var condition = h.Eval(args[0]);
                switch (condition)
                {
                    case LispT t:
                        return t;
                    default:
                        var message = h.Eval(args[1]);
                        return new LispError($"Assert failed: {message}\nWith value: {condition}");
                }
            });
            var result = host.Eval(contents);
            switch (result)
            {
                case LispT _:
                    break;
                default:
                    Assert.True(false, result.ToString());
                    break;
            }
        }

        [Fact]
        public void Runtime()
        {
            EvalFile("runtime-tests.lisp");
        }
    }
}
