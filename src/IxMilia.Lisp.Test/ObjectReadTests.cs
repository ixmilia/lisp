using System.IO;
using System.Linq;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class ObjectReadTests : TestBase
    {
        private LispObject Read(string text)
        {
            var host = new LispHost();
            var input = new LispStream("", new StringReader(text), TextWriter.Null);
            host.ObjectReader.SetReaderStream(input);
            var result = host.ObjectReader.Read(false, new LispError("EOF"), false);
            return result.LastResult;
        }

        [Fact]
        public void EmptyLists()
        {
            Assert.Equal(LispNilList.Instance, Read("()"));
            Assert.Equal(LispNilList.Instance, Read(" ( ) "));
        }

        [Fact]
        public void NestedLists()
        {
            Assert.Equal(LispList.FromItems(new LispString("a")), Read(" (\"a\") "));
            Assert.Equal(LispList.FromItems(new LispString("a"), LispNilList.Instance, new LispString("b")), Read(" ( \"a\" () \"b\" ) "));
            Assert.Equal(LispList.FromItems(new LispInteger(4), LispNilList.Instance), Read("(4())"));
        }

        [Fact]
        public void ForwardReferncedLists()
        {
            var list = (LispForwardListReference)Read("#1=(1 2 #1#)");
            Assert.True(list.List.IsProperList);
            Assert.Equal(3, list.List.Length);
            Assert.Equal("#1=(1 2 #1#)", list.ToString());
        }

        [Fact]
        public void DottedList()
        {
            var list = (LispList)Read("(1 . 2)");
            Assert.Equal(1, list.Length);
            Assert.Equal(new[] { 1, 2 }, list.ToList().Cast<LispInteger>().Select(n => n.Value).ToArray());
            Assert.False(list.IsProperList);
        }

        [Fact]
        public void BadDottedList()
        {
            var error = (LispError)Read("(1 2 . 3 . 4)");
            Assert.Equal(1, error.SourceLocation?.Line);
            Assert.Equal(10, error.SourceLocation?.Column);
            Assert.Equal("Unexpected duplicate '.' in list at (1, 10); first '.' at (1, 6)", error.Message);
        }

        [Fact]
        public void UnmatchedLeftParen()
        {
            var error = (LispError)Read("(1 2 3");
            Assert.Equal("Unmatched '(' at (1, 1) (depth 1)", error.Message);
            Assert.Equal(1, error.SourceLocation?.Line);
            Assert.Equal(1, error.SourceLocation?.Column);
        }

        [Fact]
        public void UnmatchedRightParen()
        {
            var error = (LispError)Read(")");
            Assert.Equal("Unexpected character ')' at position (1, 1)", error.Message);
            Assert.Equal(1, error.SourceLocation?.Line);
            Assert.Equal(1, error.SourceLocation?.Column);
        }

        [Fact]
        public void Strings()
        {
            Assert.Equal("", ((LispString)Read("\"\"")).Value);
            Assert.Equal("a", ((LispString)Read(" \"a\" ")).Value);
            Assert.Equal("\\\"\\", ((LispString)Read(" \"\\\\\\\"\\\\\" ")).Value);
        }

        [Theory]
        [InlineData("0", 0)]
        [InlineData("3", 3)]
        [InlineData("+3", 3)]
        [InlineData("-31", -31)]
        public void Integers(string code, int value)
        {
            var number = (LispInteger)Read(code);
            Assert.Equal(value, number.Value);
        }

        [Theory]
        [InlineData("1/2", "1/2")]
        [InlineData("+1/2", "1/2")]
        [InlineData("-1/2", "-1/2")]
        [InlineData("1/-2", "-1/2")]
        [InlineData("2/4", "1/2")]
        public void Ratios(string code, string value)
        {
            var ratio = (LispRatio)Read(code);
            Assert.Equal(value, ratio.ToString());
        }

        [Theory]
        [InlineData("3.5", 3.5)]
        [InlineData("3.5e4", 3.5e4)]
        [InlineData("+3.5e4", 3.5e4)]
        [InlineData("-3.5e4", -3.5e4)]
        [InlineData("3.5e+4", 3.5e4)]
        [InlineData("3.5e-4", 3.5e-4)]
        public void Floats(string code, double value)
        {
            var number = (LispFloat)Read(code);
            Assert.Equal(value, number.Value);
        }

        [Theory]
        [InlineData(@"#\a", 'a')]
        [InlineData(@"#\/", '/')]
        [InlineData(@"#\'", '\'')]
        public void Characters(string code, char expected)
        {
            var c = (LispCharacter)Read(code);
            Assert.Equal(expected, c.Value);
        }

        [Fact]
        public void Keywords()
        {
            Assert.Equal(":ABC", ((LispKeyword)Read(" :abc ")).Keyword);
            Assert.Equal("&REST", ((LispLambdaListKeyword)Read(" &rest ")).Keyword);
        }

        [Fact]
        public void QuotedNamedFunctions()
        {
            Assert.Equal("READ", ((LispQuotedNamedFunctionReference)Read("#'read")).Name);
        }

        [Fact]
        public void Symbols()
        {
            Assert.Equal("+", ((LispSymbol)Read("+")).Value);
            Assert.Equal(">>", ((LispSymbol)Read(">>")).Value);
            Assert.Equal("SOME:SYMBOL", ((LispSymbol)Read("some:symbol")).Value);
        }

        [Fact]
        public void Quoted()
        {
            Assert.Equal(new LispSymbol("A"), Read("a"));
            Assert.Equal(LispList.FromItems(new LispSymbol("QUOTE"), new LispSymbol("A")), Read("'a"));
            Assert.Equal(LispList.FromItems(new LispSymbol("QUOTE"), LispList.FromItems(new LispSymbol("QUOTE"), new LispSymbol("A"))), Read("''a"));
            Assert.Equal(LispList.FromItems(new LispSymbol("QUOTE"), new LispList(new LispInteger(1))), Read("'(1)"));
        }

        [Fact]
        public void SourceLocations()
        {
            var list = (LispList)Read(" ( a b c ( 1 2 3 ) ) ");
            Assert.Equal(1, list.SourceLocation?.Line);
            Assert.Equal(2, list.SourceLocation?.Column);

            var listValues = list.ToList();
            Assert.Equal(1, listValues[0].SourceLocation?.Line);
            Assert.Equal(4, listValues[0].SourceLocation?.Column);
            Assert.Equal(1, listValues[1].SourceLocation?.Line);
            Assert.Equal(6, listValues[1].SourceLocation?.Column);
            Assert.Equal(1, listValues[2].SourceLocation?.Line);
            Assert.Equal(8, listValues[2].SourceLocation?.Column);

            var innerList = (LispList)listValues[3];
            Assert.Equal(1, innerList.SourceLocation?.Line);
            Assert.Equal(10, innerList.SourceLocation?.Column);

            var innerListValues = innerList.ToList();
            Assert.Equal(1, innerListValues[0].SourceLocation?.Line);
            Assert.Equal(12, innerListValues[0].SourceLocation?.Column);
            Assert.Equal(1, innerListValues[1].SourceLocation?.Line);
            Assert.Equal(14, innerListValues[1].SourceLocation?.Column);
            Assert.Equal(1, innerListValues[2].SourceLocation?.Line);
            Assert.Equal(16, innerListValues[2].SourceLocation?.Column);

            // after newline
            var symbol = Read("\na");
            Assert.Equal(2, symbol.SourceLocation?.Line);
            Assert.Equal(1, symbol.SourceLocation?.Column);
        }

        [Fact]
        public void ParsedObjectsHaveParentsSet()
        {
            var rootList = Read(@"
(defun test-function ()
    (+ 1 2)
    (* (- 3 4) (/ 5 6)))
");
            Assert.Null(rootList.Parent);

            // check top level children
            var children = rootList.GetChildren().ToList();
            Assert.Equal(5, children.Count);
            foreach (var child in children)
            {
                Assert.True(ReferenceEquals(child.Parent, rootList), $"child = [{child}]; child.Parent [{child.Parent}] != rootNode [{rootList}]");
            }

            // spot-check one level more
            var multiplyExpression = children.Last();
            var nextLevelChildren = multiplyExpression.GetChildren().ToList();
            Assert.Equal(3, nextLevelChildren.Count);
            foreach (var child in nextLevelChildren)
            {
                Assert.True(ReferenceEquals(child.Parent, multiplyExpression));
            }
        }

        [Fact]
        public void ReadStreamObjectsThenDefaultEofMarker()
        {
            var input = new StringReader("(abc 2)\n14");
            var stream = new LispStream("TEST-STREAM", input, TextWriter.Null);
            var host = new LispHost();
            host.ObjectReader.SetReaderStream(stream);

            var list = ((LispList)host.ObjectReader.Read(false, new LispError("EOF"), false).LastResult).ToList();
            Assert.Equal(2, list.Count);
            Assert.Equal("ABC", ((LispSymbol)list[0]).Value);
            Assert.Equal(2, ((LispInteger)list[1]).Value);

            var number = (LispInteger)host.ObjectReader.Read(false, new LispError("EOF"), false).LastResult;
            Assert.Equal(14, number.Value);

            var eof = (LispError)host.ObjectReader.Read(false, new LispError("EOF"), false).LastResult;
            Assert.Equal("EOF", eof.Message);
        }

        [Fact]
        public void ReadStreamObjectsThenCustomEofMarker()
        {
            var input = new StringReader("14");
            var stream = new LispStream("TEST-STREAM", input, TextWriter.Null);
            var host = new LispHost();
            host.ObjectReader.SetReaderStream(stream);

            var number = (LispInteger)host.ObjectReader.Read(false, new LispError("EOF"), false).LastResult;
            Assert.Equal(14, number.Value);

            var eof = (LispInteger)host.ObjectReader.Read(false, new LispInteger(-54), false).LastResult;
            Assert.Equal(-54, eof.Value);
        }

        [Fact]
        public void ReadFunctionDefaultEofMarker()
        {
            var input = new StringReader("14");
            var testStream = new LispStream("TEST-STREAM", input, TextWriter.Null);
            var host = new LispHost();

            host.SetValue("TEST-STREAM", testStream);
            var result = host.Eval("(list (read test-stream) (read test-stream))").LastResult; // EOF propagates to the top
            Assert.Equal("EOF", ((LispError)result).Message);
        }

        [Fact]
        public void ReadFunctionCustomEofMarker()
        {
            var input = new StringReader("14");
            var testStream = new LispStream("TEST-STREAM", input, TextWriter.Null);
            var host = new LispHost();
            host.SetValue("TEST-STREAM", testStream);
            var result = host.Eval("(list (read test-stream) (read test-stream nil -54))").LastResult;
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
            Assert.Null(host.GetValue("FILE-STREAM"));
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
                Assert.Null(host.GetValue("FILE-STREAM"));
            }
        }

        [Fact]
        public void SupportReaderMacros()
        {
            var host = new LispHost();
            var evalResult = host.Eval(@"
; %5 -> (* 100 5)
(defun percent-reader (stream char)
    (list (quote *) 100 (read stream t nil t)))
(set-macro-character #\% #'percent-reader)
%5
");
            Assert.Equal(new LispInteger(500), evalResult.LastResult);
        }

        [Fact]
        public void PeekCharPeekTypeIsNotGiven()
        {
            var input = new StringReader(" ab ");
            var testStream = new LispStream("TEST-STREAM", input, TextWriter.Null);
            var host = new LispHost();

            host.SetValue("TEST-STREAM", testStream);
            var result = host.Eval("(peek-char test-stream)").LastResult;
            Assert.Equal(new LispCharacter(' '), result);
        }

        [Fact]
        public void PeekCharPeekTypeIsNil()
        {
            var input = new StringReader(" ab ");
            var testStream = new LispStream("TEST-STREAM", input, TextWriter.Null);
            var host = new LispHost();

            host.SetValue("TEST-STREAM", testStream);
            var result = host.Eval("(peek-char nil test-stream)").LastResult;
            Assert.Equal(new LispCharacter(' '), result);
        }

        [Fact]
        public void PeekCharPeekTypeIsTThenSymbol()
        {
            var input = new StringReader(" ab ");
            var testStream = new LispStream("TEST-STREAM", input, TextWriter.Null);
            var host = new LispHost();

            host.SetValue("TEST-STREAM", testStream);
            var result = host.Eval("(peek-char t test-stream)").LastResult;
            Assert.Equal(new LispCharacter('a'), result);
        }

        [Fact]
        public void PeekCharPeekTypeIsTThenComment()
        {
            var input = new StringReader(" ;ab ");
            var testStream = new LispStream("TEST-STREAM", input, TextWriter.Null);
            var host = new LispHost();

            host.SetValue("TEST-STREAM", testStream);
            var result = host.Eval("(peek-char t test-stream)").LastResult;
            Assert.Equal(new LispCharacter(';'), result);
        }

        [Fact]
        public void PeekCharPeekTypeIsCharacter()
        {
            var input = new StringReader(" ab ");
            var testStream = new LispStream("TEST-STREAM", input, TextWriter.Null);
            var host = new LispHost();

            host.SetValue("TEST-STREAM", testStream);
            var result = host.Eval("(peek-char #\\b test-stream)").LastResult;
            Assert.Equal(new LispCharacter('b'), result);
        }
    }
}
