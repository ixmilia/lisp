using System.IO;
using System.Linq;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class StreamTests : TestBase
    {
        [Fact]
        public void ReadObjects()
        {
            var input = new StringReader("(abc \n 2)");
            var output = TextWriter.Null;
            var stream = new LispTerminalStream(input, output);
            var objects = stream.ReadCompleteObjects().ToList();
            var list = ((LispList)objects.Single()).ToList();
            Assert.Equal(2, list.Count);
            Assert.Equal("abc", ((LispSymbol)list[0]).Value);
            Assert.Equal(2, ((LispInteger)list[1]).Value);
        }

        [Fact]
        public void ReadObjectsAfterAComment()
        {
            var input = new StringReader("; a line comment\n\"a string on a separate line\"");
            var stream = new LispTerminalStream(input, TextWriter.Null);
            var objects = stream.ReadCompleteObjects().ToList();
            var str = (LispString)objects.Single();
            Assert.Equal("a string on a separate line", str.Value);
        }

        [Fact]
        public void WithOpenFile_Reading()
        {
            var output = new StringWriter();
            var host = new LispHost(output: output);
            var result = host.Eval(@"
(with-open-file (file-stream ""test-file.dat"")
    (format t ""read: ~S~%"" (read file-stream))
    (format t ""evaluated: ~S~%"" (eval (read file-stream)))
)
");
            Assert.IsNotType<LispError>(result);
            Assert.Equal("read: \"just a string\"\r\nevaluated: 5\r\n", NormalizeNewlines(output.ToString()));
            Assert.Null(host.GetValue("file-stream"));
        }
    }
}
