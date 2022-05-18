using System.IO;
using System.Linq;
using Xunit;
using static System.Net.Mime.MediaTypeNames;

namespace IxMilia.Lisp.Test
{
    public class ObjectReadTests : TestBase
    {
        private LispObject Read(string text)
        {
            var host = new LispHost();
            var input = new LispTextStream("", new StringReader(text), TextWriter.Null);
            host.ObjectReader.SetReaderStream(input);
            var result = host.ObjectReader.Read(false, new LispError("EOF"), false);
            return result.LastResult;
        }

        private LispToken[] ReadTokens(string code)
        {
            var host = new LispHost();
            var input = new LispTextStream("", new StringReader(code), TextWriter.Null);
            host.ObjectReader.SetReaderStream(input);
            var result = host.ObjectReader.Read(false, new LispError("EOF"), false);
            var tokens = result.LastResult.GetSemanticTokens(host).ToArray();
            return tokens;
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
            Assert.Equal("COMMON-LISP-USER:#1=(1 2 COMMON-LISP-USER:#1#)", list.ToString());
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
            Assert.Equal(1, error.SourceLocation?.Start.Line);
            Assert.Equal(10, error.SourceLocation?.Start.Column);
            Assert.Equal("Unexpected duplicate '.' in list at (1, 10); first '.' at (1, 6)", error.Message);
        }

        [Fact]
        public void UnmatchedLeftParen()
        {
            var error = (LispError)Read("(1 2 3");
            Assert.Equal("Unmatched '(' at (1, 1) (depth 1)", error.Message);
            Assert.Equal(1, error.SourceLocation?.Start.Line);
            Assert.Equal(1, error.SourceLocation?.Start.Column);
        }

        [Fact]
        public void UnmatchedRightParen()
        {
            var error = (LispError)Read(")");
            Assert.Equal("Unexpected character ')' at position (1, 1)", error.Message);
            Assert.Equal(1, error.SourceLocation?.Start.Line);
            Assert.Equal(1, error.SourceLocation?.Start.Column);
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

        [Fact]
        public void ComplexNumbers()
        {
            Assert.Equal(new LispComplexNumber(new LispInteger(1), new LispInteger(2)), Read("#c(1 2)"));
            Assert.Equal(new LispComplexNumber(new LispInteger(3), new LispInteger(4)), Read("#C(3 4)"));
            Assert.Equal(new LispInteger(5), Read("#c(5 0)"));
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
            Assert.Equal(":ABC", ((LispResolvedSymbol)Read(" :abc ")).Value);
            Assert.Equal("&REST", ((LispLambdaListKeyword)Read(" &rest ")).Keyword);
        }

        [Fact]
        public void QuotedNamedFunctions()
        {
            Assert.Equal("COMMON-LISP:READ", ((LispQuotedNamedFunctionReference)Read("#'read")).Name);
        }

        [Fact]
        public void Symbols()
        {
            Assert.Equal("+", ((LispSymbol)Read("+")).LocalName);
            Assert.Equal(">>", ((LispSymbol)Read(">>")).LocalName);
            Assert.Equal("SOME:SYMBOL", ((LispResolvedSymbol)Read("some:symbol")).Value);
        }

        [Fact]
        public void Quoted()
        {
            Assert.Equal(new LispUnresolvedSymbol("A"), Read("a"));
            Assert.Equal(LispList.FromItems(new LispUnresolvedSymbol("QUOTE"), new LispUnresolvedSymbol("A")), Read("'a"));
            Assert.Equal(LispList.FromItems(new LispUnresolvedSymbol("QUOTE"), LispList.FromItems(new LispUnresolvedSymbol("QUOTE"), new LispUnresolvedSymbol("A"))), Read("''a"));
            Assert.Equal(LispList.FromItems(new LispUnresolvedSymbol("QUOTE"), new LispList(new LispInteger(1))), Read("'(1)"));
        }

        [Fact]
        public void SourceLocations()
        {
            var list = (LispList)Read(" ( a b c ( 1 2 3 ) ) ");
            Assert.Equal(1, list.SourceLocation?.Start.Line);
            Assert.Equal(2, list.SourceLocation?.Start.Column);

            var listValues = list.ToList();
            Assert.Equal(new LispSourceLocation("", new LispSourcePosition(1, 4), new LispSourcePosition(1, 5)), listValues[0].SourceLocation); // a
            Assert.Equal(new LispSourceLocation("", new LispSourcePosition(1, 6), new LispSourcePosition(1, 7)), listValues[1].SourceLocation); // b
            Assert.Equal(new LispSourceLocation("", new LispSourcePosition(1, 8), new LispSourcePosition(1, 9)), listValues[2].SourceLocation); // c

            var innerList = (LispList)listValues[3];
            Assert.Equal(new LispSourceLocation("", new LispSourcePosition(1, 10), new LispSourcePosition(1, 19)), innerList.SourceLocation); // ( 1 2 3 )

            var innerListValues = innerList.ToList();
            Assert.Equal(new LispSourceLocation("", new LispSourcePosition(1, 12), new LispSourcePosition(1, 13)), innerListValues[0].SourceLocation); // 1
            Assert.Equal(new LispSourceLocation("", new LispSourcePosition(1, 14), new LispSourcePosition(1, 15)), innerListValues[1].SourceLocation); // 2
            Assert.Equal(new LispSourceLocation("", new LispSourcePosition(1, 16), new LispSourcePosition(1, 17)), innerListValues[2].SourceLocation); // 3

            // after newline
            var symbol = Read("\na");
            Assert.Equal(new LispSourceLocation("", new LispSourcePosition(2, 1), new LispSourcePosition(2, 2)), symbol.SourceLocation);
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
            var stream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = new LispHost();
            host.ObjectReader.SetReaderStream(stream);

            var list = ((LispList)host.ObjectReader.Read(false, new LispError("EOF"), false).LastResult).ToList();
            Assert.Equal(2, list.Count);
            Assert.Equal("ABC", ((LispSymbol)list[0]).LocalName);
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
            var stream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
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
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = new LispHost();

            host.SetValue("TEST-STREAM", testStream);
            var result = host.Eval("(list (read test-stream) (read test-stream))").LastResult; // EOF propagates to the top
            Assert.Equal("EOF", ((LispError)result).Message);
        }

        [Fact]
        public void ReadFunctionCustomEofMarker()
        {
            var input = new StringReader("14");
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = new LispHost();
            host.SetValue("TEST-STREAM", testStream);
            var result = host.Eval("(list (read test-stream) (read test-stream nil -54))").LastResult;
            var resultList = ((LispList)result).ToList();
            Assert.Equal(2, resultList.Count);
            Assert.Equal(14, ((LispInteger)resultList[0]).Value);
            Assert.Equal(-54, ((LispInteger)resultList[1]).Value);
        }

        [Fact]
        public void FunctionDocumentationHandlesMultilineIndentedText()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(defun test-function ()
    ""First line of doc string.
     Second line of doc string.""
    ())
".Replace("\r", ""));
            var function = host.GetValue<LispFunction>("TEST-FUNCTION");
            Assert.Equal("First line of doc string.\nSecond line of doc string.", function.Documentation);
        }

        [Fact]
        public void MacroDocumentationHandlesMultilineIndentedText()
        {
            var host = new LispHost();
            var result = host.Eval(@"
(defmacro test-macro ()
    ""First line of doc string.
     Second line of doc string.""
    ())
".Replace("\r", ""));
            var macro = host.GetValue<LispMacro>("TEST-MACRO");
            Assert.Equal("First line of doc string.\nSecond line of doc string.", macro.Documentation);
        }

        [Fact]
        public void DocumentationStringsHandleBlankLinesInIndentedText()
        {
            var docString = (LispString)Read(@"
    ""First line.

     Third line.""
");
            var normalized = LispDefaultContext.NormalizeDocumentationString(docString).Replace("\r", "");
            Assert.Equal("First line.\n\nThird line.", normalized);
        }

        [Fact]
        public void DocumentationStringsHandleExtraWhitespaceBlankLinesInIndentedText()
        {
            var docString = (LispString)Read(@"
    ; note, all of the whitespace between the lines
    ""First line.
            
     Third line.""
");
            var normalized = LispDefaultContext.NormalizeDocumentationString(docString).Replace("\r", "");
            Assert.Equal("First line.\n\nThird line.", normalized);
        }

        [Fact]
        public void DocumentationStringsCanBeIndentedOneLessToAccountForTheLeadingDoubleQuote()
        {
            var docString = (LispString)Read(@"
    ""First line.

    Third line.""
    ; note, `Third line.` is indented one less than the first line.
");
            var normalized = LispDefaultContext.NormalizeDocumentationString(docString).Replace("\r", "");
            Assert.Equal("First line.\n\nThird line.", normalized);
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
            Assert.IsNotType<LispError>(result.LastResult);
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
                Assert.IsNotType<LispError>(result.LastResult);
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
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = new LispHost();

            host.SetValue("TEST-STREAM", testStream);
            var result = host.Eval("(peek-char test-stream)").LastResult;
            Assert.Equal(new LispCharacter(' '), result);
        }

        [Fact]
        public void PeekCharPeekTypeIsNil()
        {
            var input = new StringReader(" ab ");
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = new LispHost();

            host.SetValue("TEST-STREAM", testStream);
            var result = host.Eval("(peek-char nil test-stream)").LastResult;
            Assert.Equal(new LispCharacter(' '), result);
        }

        [Fact]
        public void PeekCharPeekTypeIsTThenSymbol()
        {
            var input = new StringReader(" ab ");
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = new LispHost();

            host.SetValue("TEST-STREAM", testStream);
            var result = host.Eval("(peek-char t test-stream)").LastResult;
            Assert.Equal(new LispCharacter('a'), result);
        }

        [Fact]
        public void PeekCharPeekTypeIsTThenComment()
        {
            var input = new StringReader(" ;ab ");
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = new LispHost();

            host.SetValue("TEST-STREAM", testStream);
            var result = host.Eval("(peek-char t test-stream)").LastResult;
            Assert.Equal(new LispCharacter(';'), result);
        }

        [Fact]
        public void PeekCharPeekTypeIsCharacter()
        {
            var input = new StringReader(" ab ");
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = new LispHost();

            host.SetValue("TEST-STREAM", testStream);
            var result = host.Eval("(peek-char #\\b test-stream)").LastResult;
            Assert.Equal(new LispCharacter('b'), result);
        }

        [Fact]
        public void ObjectsFromReaderMacrosHaveProperSourceSpansSet1()
        {
            var text = "\"some string\"";
            var host = new LispHost();
            var input = new LispTextStream("", new StringReader(text), TextWriter.Null);
            host.ObjectReader.SetReaderStream(input);
            var result = host.ObjectReader.Read(false, new LispError("EOF"), false);
            Assert.Equal(new LispSourceLocation("", new LispSourcePosition(1, 1), new LispSourcePosition(1, 14)), result.LastResult.SourceLocation);
        }

        [Fact]
        public void ObjectsFromReaderMacrosHaveProperSourceSpansSet2()
        {
            var text = "#'asdf";
            var host = new LispHost();
            var input = new LispTextStream("", new StringReader(text), TextWriter.Null);
            host.ObjectReader.SetReaderStream(input);
            var result = host.ObjectReader.Read(false, new LispError("EOF"), false);
            Assert.Equal("#'COMMON-LISP-USER:ASDF", result.LastResult.ToString());
            Assert.Equal(new LispSourceLocation("", new LispSourcePosition(1, 1), new LispSourcePosition(1, 7)), result.LastResult.SourceLocation);
        }

        [Fact]
        public void ObjectsAfterReaderMacroHaveProperSourceSpansSet1()
        {
            var text = "\"some string\" 42";
            var host = new LispHost();
            var input = new LispTextStream("", new StringReader(text), TextWriter.Null);
            host.ObjectReader.SetReaderStream(input);
            var _ = host.ObjectReader.Read(false, new LispError("EOF"), false); // swallow the string
            var result = host.ObjectReader.Read(false, new LispError("EOF"), false);
            Assert.Equal(new LispSourceLocation("", new LispSourcePosition(1, 15), new LispSourcePosition(1, 17)), result.LastResult.SourceLocation);
        }

        [Fact]
        public void ObjectsAfterReaderMacroHaveProperSourceSpansSet2()
        {
            var text = "#'asdf 42";
            var host = new LispHost();
            var input = new LispTextStream("", new StringReader(text), TextWriter.Null);
            host.ObjectReader.SetReaderStream(input);
            var _ = host.ObjectReader.Read(false, new LispError("EOF"), false); // swallow the string
            var result = host.ObjectReader.Read(false, new LispError("EOF"), false);
            Assert.Equal(new LispSourceLocation("", new LispSourcePosition(1, 8), new LispSourcePosition(1, 10)), result.LastResult.SourceLocation);
        }

        [Fact]
        public void IncompleteInputIsReturnedWhenReaderMacrosAreActive()
        {
            var text = "\"this string is incomplete";
            var host = new LispHost();
            var input = new LispTextStream("", new StringReader(text), TextWriter.Null);
            host.ObjectReader.SetReaderStream(input);
            var result = host.ObjectReader.Read(false, new LispError("EOF"), false);
            Assert.Equal(text, result.IncompleteInput);
        }

        [Fact]
        public void Tokens()
        {
            var code = @"(defun my-function (a b) (+ 11 22) ""some string"")";
            //            macro function     parameter
            //                                     function
            //                                       number string
            var tokens = ReadTokens(code);
            var expected = new[]
            {
                new LispToken(LispTokenType.Macro, new LispSourcePosition(1, 2), new LispSourcePosition(1, 7)), // `defun`
                // NYI
                //new LispToken(LispTokenType.Function, new LispSourcePosition(1, 8), new LispSourcePosition(1, 19)), // `my-function`
                //new LispToken(LispTokenType.Parameter, new LispSourcePosition(1, 21), new LispSourcePosition(1, 22)), // `a`
                //new LispToken(LispTokenType.Parameter, new LispSourcePosition(1, 23), new LispSourcePosition(1, 24)), // `b`
                new LispToken(LispTokenType.Function, new LispSourcePosition(1, 27), new LispSourcePosition(1, 28)), // `+`
                new LispToken(LispTokenType.Number, new LispSourcePosition(1, 29), new LispSourcePosition(1, 31)), // `11`
                new LispToken(LispTokenType.Number, new LispSourcePosition(1, 32), new LispSourcePosition(1, 34)), // `22`
                new LispToken(LispTokenType.String, new LispSourcePosition(1, 36), new LispSourcePosition(1, 49)), // `"some string"`
            };
            Assert.Equal(expected, tokens);
        }
    }
}
