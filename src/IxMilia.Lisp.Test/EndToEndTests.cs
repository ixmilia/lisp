using System.Collections.Generic;
using System.IO;
using System.Reflection;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class EndToEndTests
    {
        private string GetScriptContents(string name)
        {
            var type = GetType();
            var lastDotIndex = type.FullName.LastIndexOf('.');
            var namespacePrefix = type.FullName.Substring(0, lastDotIndex);
            var assembly = type.GetTypeInfo().Assembly;
            using (var initStream = assembly.GetManifestResourceStream($"{namespacePrefix}.{name}"))
            using (var reader = new StreamReader(initStream))
            {
                var content = reader.ReadToEnd();
                return content;
            }
        }

        private void EvalFile(string fileName)
        {
            var contents = GetScriptContents(fileName);
            var host = new LispHost();
            host.AddFunction("join", (h, args) => new LispString(string.Join(" ", (IEnumerable<LispObject>)args)));
            host.AddFunction("fail", (h, args) => new LispError(args[0].ToString()));
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
