using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class ObjectReadTests : TestBase
    {
        private async Task<LispObject> ReadAsync(string text)
        {
            var host = await LispHost.CreateAsync();
            var input = new LispTextStream("", new StringReader(text), TextWriter.Null);
            host.ObjectReader.SetReaderStream(input);
            var result = await host.ObjectReader.ReadAsync(false, new LispError("EOF"), false);
            return result.LastResult;
        }

        private async Task<LispToken[]> ReadTokensAsync(string code)
        {
            var host = await LispHost.CreateAsync();
            var input = new LispTextStream("", new StringReader(code), TextWriter.Null);
            host.ObjectReader.SetReaderStream(input);
            var result = await host.ObjectReader.ReadAsync(false, new LispError("EOF"), false);
            var tokens = result.LastResult.GetSemanticTokens(host).ToArray();
            return tokens;
        }

        [Fact]
        public async Task EmptyLists()
        {
            Assert.Equal(LispNilList.Instance, await ReadAsync("()"));
            Assert.Equal(LispNilList.Instance, await ReadAsync(" ( ) "));
        }

        [Fact]
        public async Task NestedLists()
        {
            Assert.Equal(LispList.FromItems(new LispString("a")), await ReadAsync(" (\"a\") "));
            Assert.Equal(LispList.FromItems(new LispString("a"), LispNilList.Instance, new LispString("b")), await ReadAsync(" ( \"a\" () \"b\" ) "));
            Assert.Equal(LispList.FromItems(new LispInteger(4), LispNilList.Instance), await ReadAsync("(4())"));
        }

        [Fact]
        public async Task ForwardReferncedLists()
        {
            var list = (LispForwardListReference)await ReadAsync("#1=(1 2 #1#)");
            Assert.True(list.List.IsProperList);
            Assert.Equal(3, list.List.Length);
            Assert.Equal("COMMON-LISP-USER:#1=(1 2 COMMON-LISP-USER:#1#)", list.ToString());
        }

        [Fact]
        public async Task DottedList()
        {
            var list = (LispList)await ReadAsync("(1 . 2)");
            Assert.Equal(1, list.Length);
            Assert.Equal(new[] { 1, 2 }, list.ToList().Cast<LispInteger>().Select(n => n.Value).ToArray());
            Assert.False(list.IsProperList);
        }

        [Fact]
        public async Task BadDottedList()
        {
            var error = (LispError)await ReadAsync("(1 2 . 3 . 4)");
            Assert.Equal(1, error.SourceLocation?.Start.Line);
            Assert.Equal(10, error.SourceLocation?.Start.Column);
            Assert.Equal("Unexpected duplicate '.' in list at (1, 10); first '.' at (1, 6)", error.Message);
        }

        [Fact]
        public async Task UnmatchedLeftParen()
        {
            var error = (LispError)await ReadAsync("(1 2 3");
            Assert.Equal("Unmatched '(' at (1, 1) (depth 1)", error.Message);
            Assert.Equal(1, error.SourceLocation?.Start.Line);
            Assert.Equal(1, error.SourceLocation?.Start.Column);
        }

        [Fact]
        public async Task UnmatchedRightParen()
        {
            var error = (LispError)await ReadAsync(")");
            Assert.Equal("Unexpected character ')' at position (1, 1)", error.Message);
            Assert.Equal(1, error.SourceLocation?.Start.Line);
            Assert.Equal(1, error.SourceLocation?.Start.Column);
        }

        [Fact]
        public async Task Strings()
        {
            Assert.Equal("", ((LispString)await ReadAsync("\"\"")).Value);
            Assert.Equal("a", ((LispString)await ReadAsync(" \"a\" ")).Value);
            Assert.Equal("\\\"\\", ((LispString)await ReadAsync(" \"\\\\\\\"\\\\\" ")).Value);
        }

        [Theory]
        [InlineData("0", 0)]
        [InlineData("3", 3)]
        [InlineData("+3", 3)]
        [InlineData("-31", -31)]
        public async Task Integers(string code, int value)
        {
            var number = (LispInteger)await ReadAsync(code);
            Assert.Equal(value, number.Value);
        }

        [Theory]
        [InlineData("1/2", "1/2")]
        [InlineData("+1/2", "1/2")]
        [InlineData("-1/2", "-1/2")]
        [InlineData("1/-2", "-1/2")]
        [InlineData("2/4", "1/2")]
        public async Task Ratios(string code, string value)
        {
            var ratio = (LispRatio)await ReadAsync(code);
            Assert.Equal(value, ratio.ToString());
        }

        [Theory]
        [InlineData("3.5", 3.5)]
        [InlineData("3.5e4", 3.5e4)]
        [InlineData("+3.5e4", 3.5e4)]
        [InlineData("-3.5e4", -3.5e4)]
        [InlineData("3.5e+4", 3.5e4)]
        [InlineData("3.5e-4", 3.5e-4)]
        public async Task Floats(string code, double value)
        {
            var number = (LispFloat)await ReadAsync(code);
            Assert.Equal(value, number.Value);
        }

        [Fact]
        public async Task ComplexNumbers()
        {
            Assert.Equal(new LispComplexNumber(new LispInteger(1), new LispInteger(2)), await ReadAsync("#c(1 2)"));
            Assert.Equal(new LispComplexNumber(new LispInteger(3), new LispInteger(4)), await ReadAsync("#C(3 4)"));
            Assert.Equal(new LispInteger(5), await ReadAsync("#c(5 0)"));
        }

        [Theory]
        [InlineData(@"#\a", 'a')]
        [InlineData(@"#\/", '/')]
        [InlineData(@"#\'", '\'')]
        public async Task Characters(string code, char expected)
        {
            var c = (LispCharacter)await ReadAsync(code);
            Assert.Equal(expected, c.Value);
        }

        [Fact]
        public async Task Keywords()
        {
            Assert.Equal(":ABC", ((LispResolvedSymbol)await ReadAsync(" :abc ")).Value);
            Assert.Equal("&REST", ((LispLambdaListKeyword)await ReadAsync(" &rest ")).Keyword);
        }

        [Fact]
        public async Task QuotedNamedFunctions()
        {
            Assert.Equal("COMMON-LISP:READ", ((LispQuotedNamedFunctionReference)await ReadAsync("#'read")).Name);
        }

        [Fact]
        public async Task Symbols()
        {
            Assert.Equal("+", ((LispSymbol)await ReadAsync("+")).LocalName);
            Assert.Equal(">>", ((LispSymbol)await ReadAsync(">>")).LocalName);
            Assert.Equal("SOME:SYMBOL", ((LispResolvedSymbol)await ReadAsync("some:symbol")).Value);
        }

        [Fact]
        public async Task Quoted()
        {
            Assert.Equal(new LispUnresolvedSymbol("A"), await ReadAsync("a"));
            Assert.Equal(LispList.FromItems(new LispUnresolvedSymbol("QUOTE"), new LispUnresolvedSymbol("A")), await ReadAsync("'a"));
            Assert.Equal(LispList.FromItems(new LispUnresolvedSymbol("QUOTE"), LispList.FromItems(new LispUnresolvedSymbol("QUOTE"), new LispUnresolvedSymbol("A"))), await ReadAsync("''a"));
            Assert.Equal(LispList.FromItems(new LispUnresolvedSymbol("QUOTE"), new LispList(new LispInteger(1))), await ReadAsync("'(1)"));
        }

        [Fact]
        public async Task SourceLocations()
        {
            var list = (LispList)await ReadAsync(" ( a b c ( 1 2 3 ) ) ");
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
            var symbol = await ReadAsync("\na");
            Assert.Equal(new LispSourceLocation("", new LispSourcePosition(2, 1), new LispSourcePosition(2, 2)), symbol.SourceLocation);
        }

        [Fact]
        public async Task ParsedObjectsHaveParentsSet()
        {
            var rootList = await ReadAsync(@"
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
        public async Task ReadStreamObjectsThenDefaultEofMarker()
        {
            var input = new StringReader("(abc 2)\n14");
            var stream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = await LispHost.CreateAsync();
            host.ObjectReader.SetReaderStream(stream);

            var list = ((LispList)(await host.ObjectReader.ReadAsync(false, new LispError("EOF"), false)).LastResult).ToList();
            Assert.Equal(2, list.Count);
            Assert.Equal("ABC", ((LispSymbol)list[0]).LocalName);
            Assert.Equal(2, ((LispInteger)list[1]).Value);

            var number = (LispInteger)(await host.ObjectReader.ReadAsync(false, new LispError("EOF"), false)).LastResult;
            Assert.Equal(14, number.Value);

            var eof = (LispError)(await host.ObjectReader.ReadAsync(false, new LispError("EOF"), false)).LastResult;
            Assert.Equal("EOF", eof.Message);
        }

        [Fact]
        public async Task ReadStreamObjectsThenCustomEofMarker()
        {
            var input = new StringReader("14");
            var stream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = await LispHost.CreateAsync();
            host.ObjectReader.SetReaderStream(stream);

            var number = (LispInteger)(await host.ObjectReader.ReadAsync(false, new LispError("EOF"), false)).LastResult;
            Assert.Equal(14, number.Value);

            var eof = (LispInteger)(await host.ObjectReader.ReadAsync(false, new LispInteger(-54), false)).LastResult;
            Assert.Equal(-54, eof.Value);
        }

        [Fact]
        public async Task ReadFunctionDefaultEofMarker()
        {
            var input = new StringReader("14");
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = await LispHost.CreateAsync();

            host.SetValue("TEST-STREAM", testStream);
            var result = (await host.EvalAsync("(list (read test-stream) (read test-stream))")).LastResult; // EOF propagates to the top
            Assert.Equal("EOF", ((LispError)result).Message);
        }

        [Fact]
        public async Task ReadFunctionCustomEofMarker()
        {
            var input = new StringReader("14");
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = await LispHost.CreateAsync();
            host.SetValue("TEST-STREAM", testStream);
            var result = (await host.EvalAsync("(list (read test-stream) (read test-stream nil -54))")).LastResult;
            var resultList = ((LispList)result).ToList();
            Assert.Equal(2, resultList.Count);
            Assert.Equal(14, ((LispInteger)resultList[0]).Value);
            Assert.Equal(-54, ((LispInteger)resultList[1]).Value);
        }

        [Fact]
        public async Task FunctionDocumentationHandlesMultilineIndentedText()
        {
            var host = await LispHost.CreateAsync();
            var result = await host.EvalAsync(@"
(defun test-function ()
    ""First line of doc string.
     Second line of doc string.""
    ())
".Replace("\r", ""));
            var function = host.GetValue<LispFunction>("TEST-FUNCTION");
            Assert.Equal("First line of doc string.\nSecond line of doc string.", function.Documentation);
        }

        [Fact]
        public async Task MacroDocumentationHandlesMultilineIndentedText()
        {
            var host = await LispHost.CreateAsync();
            var result = await host.EvalAsync(@"
(defmacro test-macro ()
    ""First line of doc string.
     Second line of doc string.""
    ())
".Replace("\r", ""));
            var macro = host.GetValue<LispMacro>("TEST-MACRO");
            Assert.Equal("First line of doc string.\nSecond line of doc string.", macro.Documentation);
        }

        [Fact]
        public async Task DocumentationStringsHandleBlankLinesInIndentedText()
        {
            var docString = (LispString)await ReadAsync(@"
    ""First line.

     Third line.""
");
            var normalized = LispDefaultContext.NormalizeDocumentationString(docString).Replace("\r", "");
            Assert.Equal("First line.\n\nThird line.", normalized);
        }

        [Fact]
        public async Task DocumentationStringsHandleExtraWhitespaceBlankLinesInIndentedText()
        {
            var docString = (LispString)await ReadAsync(@"
    ; note, all of the whitespace between the lines
    ""First line.
            
     Third line.""
");
            var normalized = LispDefaultContext.NormalizeDocumentationString(docString).Replace("\r", "");
            Assert.Equal("First line.\n\nThird line.", normalized);
        }

        [Fact]
        public async Task DocumentationStringsCanBeIndentedOneLessToAccountForTheLeadingDoubleQuote()
        {
            var docString = (LispString)await ReadAsync(@"
    ""First line.

    Third line.""
    ; note, `Third line.` is indented one less than the first line.
");
            var normalized = LispDefaultContext.NormalizeDocumentationString(docString).Replace("\r", "");
            Assert.Equal("First line.\n\nThird line.", normalized);
        }

        [Fact]
        public async Task WithOpenFile_Reading()
        {
            var output = new StringWriter();
            var host = await LispHost.CreateAsync(output: output);
            var result = await host.EvalAsync(@"
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
        public async Task WithOpenFile_Writing()
        {
            var host = await LispHost.CreateAsync();
            using (var outputFile = new TemporaryFile(createFile: false))
            {
                var result = await host.EvalAsync($@"
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
        public async Task SupportReaderMacros()
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(@"
; %5 -> (* 100 5)
(defun percent-reader (stream char)
    (list (quote *) 100 (read stream t nil t)))
(set-macro-character #\% #'percent-reader)
%5
");
            Assert.Equal(new LispInteger(500), evalResult.LastResult);
        }

        [Fact]
        public async Task ReadTableCopying()
        {
            var host = await LispHost.CreateAsync();
            var enteredOriginalBangReader = false;
            var enteredNewBangReader = false;
            host.AddFunction("ORIGINAL-BANG-READER", (_host, _executionState, _args, _cancellationToken) =>
            {
                if (enteredOriginalBangReader)
                {
                    throw new Exception("Already entered the original bang reader.");
                }

                enteredOriginalBangReader = true;
                return Task.FromResult<LispObject>(new LispInteger(42));
            });
            host.AddFunction("NEW-BANG-READER", (_host, _executionState, _args, _cancellationToken) =>
            {
                if (enteredNewBangReader)
                {
                    throw new Exception("Already entered the new bang reader");
                }

                enteredNewBangReader = true;
                return Task.FromResult(_host.Nil);
            });
            var evalResult = await host.EvalAsync(@"
(set-macro-character (code-char 33) (function original-bang-reader)) ; #\!
(defun percent-reader (stream char)
    (let ((*readtable* (copy-readtable)))
        (set-macro-character (code-char 33) (function new-bang-reader))
        (read stream t nil t)))
(set-macro-character (code-char 37) (function percent-reader)) ; #\%

; this will trigger the new bang reader
%!

; this will ensure the old bang reader still functions
!
");
            Assert.Null(evalResult.ReadError);
            Assert.True(enteredOriginalBangReader);
            Assert.True(enteredNewBangReader);
            Assert.Equal(42, ((LispInteger)evalResult.LastResult).Value);
        }

        [Theory]
        [InlineData("`(1 ,(+ 2 3) (b))", "(CONS (QUOTE 1) (CONS (QUOTE 5) (CONS (CONS (QUOTE B) ()) ())))")]
        [InlineData("`,(+ 1 2)", "(QUOTE 3)")]
        [InlineData("`#(1 2 ,(+ 1 2) ,#(4))", "(QUOTE #(1 2 3 #(4)))")]
        [InlineData("`a", "(QUOTE A)")]
        public async Task BackQuoteReadAsync(string code, string expected)
        {
            var result = await ReadAsync(code);
            Assert.Equal(expected, result.ToString());
        }

        [Fact]
        public async Task PeekCharPeekTypeIsNotGiven()
        {
            var input = new StringReader(" ab ");
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = await LispHost.CreateAsync();

            host.SetValue("TEST-STREAM", testStream);
            var result = (await host.EvalAsync("(peek-char test-stream)")).LastResult;
            Assert.Equal(new LispCharacter(' '), result);
        }

        [Fact]
        public async Task PeekCharPeekTypeIsNil()
        {
            var input = new StringReader(" ab ");
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = await LispHost.CreateAsync();

            host.SetValue("TEST-STREAM", testStream);
            var result = (await host.EvalAsync("(peek-char nil test-stream)")).LastResult;
            Assert.Equal(new LispCharacter(' '), result);
        }

        [Fact]
        public async Task PeekCharPeekTypeIsTThenSymbol()
        {
            var input = new StringReader(" ab ");
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = await LispHost.CreateAsync();

            host.SetValue("TEST-STREAM", testStream);
            var result = (await host.EvalAsync("(peek-char t test-stream)")).LastResult;
            Assert.Equal(new LispCharacter('a'), result);
        }

        [Fact]
        public async Task PeekCharPeekTypeIsTThenComment()
        {
            var input = new StringReader(" ;ab ");
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = await LispHost.CreateAsync();

            host.SetValue("TEST-STREAM", testStream);
            var result = (await host.EvalAsync("(peek-char t test-stream)")).LastResult;
            Assert.Equal(new LispCharacter(';'), result);
        }

        [Fact]
        public async Task PeekCharPeekTypeIsCharacter()
        {
            var input = new StringReader(" ab ");
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = await LispHost.CreateAsync();

            host.SetValue("TEST-STREAM", testStream);
            var result = (await host.EvalAsync("(peek-char #\\b test-stream)")).LastResult;
            Assert.Equal(new LispCharacter('b'), result);
        }

        [Fact]
        public async Task ObjectsFromReaderMacrosHaveProperSourceSpansSet1()
        {
            var text = "\"some string\"";
            var host = await LispHost.CreateAsync();
            var input = new LispTextStream("", new StringReader(text), TextWriter.Null);
            host.ObjectReader.SetReaderStream(input);
            var result = await host.ObjectReader.ReadAsync(false, new LispError("EOF"), false);
            Assert.Equal(new LispSourceLocation("", new LispSourcePosition(1, 1), new LispSourcePosition(1, 14)), result.LastResult.SourceLocation);
        }

        [Fact]
        public async Task ObjectsFromReaderMacrosHaveProperSourceSpansSet2()
        {
            var text = "#'asdf";
            var host = await LispHost.CreateAsync();
            var input = new LispTextStream("", new StringReader(text), TextWriter.Null);
            host.ObjectReader.SetReaderStream(input);
            var result = await host.ObjectReader.ReadAsync(false, new LispError("EOF"), false);
            Assert.Equal("#'COMMON-LISP-USER:ASDF", result.LastResult.ToString());
            Assert.Equal(new LispSourceLocation("", new LispSourcePosition(1, 1), new LispSourcePosition(1, 7)), result.LastResult.SourceLocation);
        }

        [Fact]
        public async Task ObjectsAfterReaderMacroHaveProperSourceSpansSet1()
        {
            var text = "\"some string\" 42";
            var host = await LispHost.CreateAsync();
            var input = new LispTextStream("", new StringReader(text), TextWriter.Null);
            host.ObjectReader.SetReaderStream(input);
            var _ = await host.ObjectReader.ReadAsync(false, new LispError("EOF"), false); // swallow the string
            var result = await host.ObjectReader.ReadAsync(false, new LispError("EOF"), false);
            Assert.Equal(new LispSourceLocation("", new LispSourcePosition(1, 15), new LispSourcePosition(1, 17)), result.LastResult.SourceLocation);
        }

        [Fact]
        public async Task ObjectsAfterReaderMacroHaveProperSourceSpansSet2()
        {
            var text = "#'asdf 42";
            var host = await LispHost.CreateAsync();
            var input = new LispTextStream("", new StringReader(text), TextWriter.Null);
            host.ObjectReader.SetReaderStream(input);
            var _ = await host.ObjectReader.ReadAsync(false, new LispError("EOF"), false); // swallow the string
            var result = await host.ObjectReader.ReadAsync(false, new LispError("EOF"), false);
            Assert.Equal(new LispSourceLocation("", new LispSourcePosition(1, 8), new LispSourcePosition(1, 10)), result.LastResult.SourceLocation);
        }

        [Fact]
        public async Task IncompleteInputIsReturnedWhenReaderMacrosAreActive()
        {
            var text = "\"this string is incomplete";
            var host = await LispHost.CreateAsync();
            var input = new LispTextStream("", new StringReader(text), TextWriter.Null);
            host.ObjectReader.SetReaderStream(input);
            var result = await host.ObjectReader.ReadAsync(false, new LispError("EOF"), false);
            Assert.Equal(text, result.IncompleteInput);
        }

        [Fact]
        public async Task Tokens()
        {
            var code = @"(defun my-function (a b) (+ 11 22) ""some string"")";
            //            macro function     parameter
            //                                     function
            //                                       number string
            var tokens = await ReadTokensAsync(code);
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

        [Fact]
        public async Task FullyQualifiedSymbolTokens()
        {
            var code = "kernel:+/2";
            //          1234567890
            var tokens = await ReadTokensAsync(code);
            var expected = new[]
            {
                new LispToken(LispTokenType.Package, new LispSourcePosition(1, 1), new LispSourcePosition(1, 7)), // `kernel`
                new LispToken(LispTokenType.Function, new LispSourcePosition(1, 8), new LispSourcePosition(1, 11)), // `+/2`
            };
            Assert.Equal(expected, tokens);
        }
    }
}
