using System.IO;
using System.Linq;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class StreamTests : TestBase
    {
        [Fact]
        public void ReadStreamObjectsThenDefaultEofMarker()
        {
            var input = new StringReader("(abc 2)\n14");
            var stream = new LispStream("test-stream", input, TextWriter.Null);

            var list = ((LispList)stream.ReadObject()).ToList();
            Assert.Equal(2, list.Count);
            Assert.Equal("abc", ((LispSymbol)list[0]).Value);
            Assert.Equal(2, ((LispInteger)list[1]).Value);

            var number = (LispInteger)stream.ReadObject();
            Assert.Equal(14, number.Value);

            var eof = (LispError)stream.ReadObject();
            Assert.Equal("EOF", eof.Message);
        }

        [Fact]
        public void ReadStreamObjectsThenCustomEofMarker()
        {
            var input = new StringReader("14");
            var stream = new LispStream("test-stream", input, TextWriter.Null);

            var number = (LispInteger)stream.ReadObject();
            Assert.Equal(14, number.Value);

            var eof = (LispInteger)stream.ReadObject(new LispInteger(-54));
            Assert.Equal(-54, eof.Value);
        }

        [Fact]
        public void ReadFunctionDefaultEofMarker()
        {
            var input = new StringReader("14");
            var testStream = new LispStream("test-stream", input, TextWriter.Null);
            var host = new LispHost();
            host.SetValue("test-stream", testStream);
            var result = host.Eval("(list (read test-stream) (read test-stream))"); // EOF propagates to the top
            Assert.Equal("EOF", ((LispError)result).Message);
        }

        [Fact]
        public void ReadFunctionCustomEofMarker()
        {
            var input = new StringReader("14");
            var testStream = new LispStream("test-stream", input, TextWriter.Null);
            var host = new LispHost();
            host.SetValue("test-stream", testStream);
            var result = host.Eval("(list (read test-stream) (read test-stream nil -54))");
            var resultList = ((LispList)result).ToList();
            Assert.Equal(2, resultList.Count);
            Assert.Equal(14, ((LispInteger)resultList[0]).Value);
            Assert.Equal(-54, ((LispInteger)resultList[1]).Value);
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
            Assert.Equal("read: \"just a string\"\nevaluated: 5\n", NormalizeNewlines(output.ToString()));
            Assert.Null(host.GetValue("file-stream"));
        }

        [Fact]
        public void WithOpenFile_Writing()
        {
            var host = new LispHost();
            using (var outputFile = new TemporaryFile(createFile: false))
            {
                var result = host.Eval($@"
(with-open-file (file-stream ""{outputFile.FilePath.Replace("\\", "\\\\")}"" :direction :output)
    (format file-stream ""wrote: ~S~%"" ""just-a-string"")
    (format file-stream ""wrote: ~S~%"" '(+ 2 3))
)
");
                Assert.IsNotType<LispError>(result);
                var actual = NormalizeNewlines(File.ReadAllText(outputFile.FilePath));
                Assert.Equal("wrote: \"just-a-string\"\nwrote: (+ 2 3)\n", actual);
            }
        }
    }
}
