using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public abstract class ObjectReadTestsBase : TestBase
    {
        public abstract LispReaderType ReaderType { get; }

        private async Task<(LispHost, LispObject)> ReadHostAndResultAsync(string code, bool checkForErrors = true)
        {
            var host = await CreateHostAsync();
            host.SetReaderFunction(ReaderType);
            var executionState = host.CreateExecutionState();
            var reader = new StringReader(code);
            var stream = new LispTextStream("TEST-INPUT", reader, TextWriter.Null);
            var obj = LispList.FromItems(new LispUnresolvedSymbol("READ"), stream);
            var evalResult = await host.EvalAsync(obj, executionState);
            if (checkForErrors)
            {
                EnsureNotError(evalResult.Value);
            }

            return (host, evalResult.Value);
        }

        protected async Task<LispObject> ReadAsync(string code, bool checkForErrors = true)
        {
            var (_host, result) = await ReadHostAndResultAsync(code, checkForErrors);
            return result;
        }

        private async Task<LispToken[]> ReadTokensAsync(string code)
        {
            var (host, result) = await ReadHostAndResultAsync(code);
            var tokens = result.GetSemanticTokens(host).ToArray();
            return tokens;
        }

        [Theory]
        [InlineData("x")]
        [InlineData("  \n;  \nx  ;")]
        public async Task SwallowTrivia(string code)
        {
            var host = await CreateHostAsync(useInitScript: false);
            var reader = new StringReader(code);
            var stream = new LispTextStream("TEST-INPUT", reader, TextWriter.Null);
            host.SetValue(stream.Name, stream);
            var obj = LispList.FromItems(new LispUnresolvedSymbol("SWALLOW-TRIVIA"), new LispUnresolvedSymbol(stream.Name));
            var executionState = LispExecutionState.CreateExecutionState(host.RootFrame, allowHalting: false);
            var evalResult = await host.EvalAsync(obj, executionState);
            EnsureNotError(evalResult.Value);

            obj = LispList.FromItems(new LispUnresolvedSymbol("READ-CHAR"), new LispUnresolvedSymbol(stream.Name));
            evalResult = await host.EvalAsync(obj, executionState);
            EnsureNotError(evalResult.Value);
            var x = (LispCharacter)evalResult.Value;
            Assert.Equal('x', x.Value);
        }

        [Theory]
        [InlineData("()", "()")]
        [InlineData(" ( ) ", "()")]
        [InlineData("(a)", "(A)")]
        [InlineData(" ( 1 ( b () ) ) ", "(1 (B ()))")]
        [InlineData(" ( 1 . 2 ) ", "(1 . 2)")]
        [InlineData(" ( 1 2 3 . 4 ) ", "(1 2 3 . 4)")]
        [InlineData("( \"a\" () \"b\" )", "(\"a\" () \"b\")")]
        [InlineData("(4())", "(4 ())")]
        public async Task MiscLists(string code, string expected)
        {
            var (host, result) = await ReadHostAndResultAsync(code);
            var actual = result.ToDisplayString(host.CurrentPackage);
            Assert.Equal(expected, actual);
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
            var error = (LispError)await ReadAsync("(1 2 . 3 . 4)", checkForErrors: false);
            Assert.Equal("Illegal end of dotted list", error.Message);
            Assert.Equal(1, error.SourceLocation?.Start.Line);
            Assert.Equal(10, error.SourceLocation?.Start.Column);
        }

        [Fact]
        public async Task UnmatchedLeftParen()
        {
            var error = (LispError)await ReadAsync("(1 2 3", checkForErrors: false);
            Assert.Equal("Unmatched #\\(", error.Message);
            Assert.Equal(1, error.SourceLocation?.Start.Line);
            Assert.Equal(1, error.SourceLocation?.Start.Column);
        }

        [Fact]
        public async Task UnmatchedRightParen()
        {
            var error = (LispError)await ReadAsync(")", checkForErrors: false);
            Assert.Equal("An object cannot start with #\\)", error.Message);
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
        [InlineData("0", 0)] // various formats
        [InlineData("3", 3)]
        [InlineData("+3", 3)]
        [InlineData("-31", -31)]
        [InlineData(" 4 ", 4)] // surrounded by whitespace
        [InlineData(" -4 ", -4)]
        public async Task Integers(string code, int value)
        {
            var number = (LispInteger)await ReadAsync(code);
            Assert.Equal(value, number.Value);
        }

        [Theory]
        [InlineData("1/2", 1, 2)] // various formats
        [InlineData("+1/2", 1, 2)]
        [InlineData("-1/2", -1, 2)]
        [InlineData("1/-2", -1, 2)]
        [InlineData("2/4", 1, 2)]
        [InlineData(" 4/5 ", 4, 5)] // surrounded by whitespace
        [InlineData(" -4/5 ", -4, 5)]
        public async Task Ratios(string code, int num, int denom)
        {
            var ratio = (LispRatio)await ReadAsync(code);
            Assert.Equal(num, ratio.Numerator);
            Assert.Equal(denom, ratio.Denominator);
        }

        [Theory]
        [InlineData("3.5", 3.5)] // various formats
        [InlineData("3.5e4", 3.5e4)]
        [InlineData("+3.5e4", 3.5e4)]
        [InlineData("-3.5e4", -3.5e4)]
        [InlineData("3.5e+4", 3.5e4)]
        [InlineData("3.5e-4", 3.5e-4)]
        [InlineData(" 4.5 ", 4.5)] // surrounded by whitespace
        [InlineData(" -4.5 ", -4.5)]
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

        [Theory]
        [InlineData("+", "COMMON-LISP:+")]
        [InlineData("x", "COMMON-LISP-USER:X")]
        [InlineData(" x ", "COMMON-LISP-USER:X")]
        [InlineData("some:symbol", "SOME:SYMBOL")]
        [InlineData(" some:symbol ", "SOME:SYMBOL")]
        public async Task Symbols(string code, string expected)
        {
            var symbol = (LispSymbol)await ReadAsync(code);
            var actual = symbol.ToString();
            Assert.Equal(expected, actual);
        }

        [Fact]
        public async Task Quoted()
        {
            Assert.Equal(LispSymbol.CreateFromString("COMMON-LISP-USER:A"), await ReadAsync("a"));
            Assert.Equal(LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:QUOTE"), LispSymbol.CreateFromString("COMMON-LISP-USER:A")), await ReadAsync("'a"));
            Assert.Equal(LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:QUOTE"), LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:QUOTE"), LispSymbol.CreateFromString("COMMON-LISP-USER:A"))), await ReadAsync("''a"));
            Assert.Equal(LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:QUOTE"), new LispList(new LispInteger(1))), await ReadAsync("'(1)"));
        }

        [Fact]
        public async Task SourceLocations()
        {
            var list = (LispList)await ReadAsync(" ( a b c ( 1 2 3 ) ( ) ) ");
            Assert.Equal(new LispSourceLocation("TEST-INPUT", new LispSourcePosition(1, 2), new LispSourcePosition(1, 25)), list.SourceLocation); // ( a b c ( 1 2 3 ) ( ) )

            var listValues = list.ToList();
            Assert.Equal(new LispSourceLocation("TEST-INPUT", new LispSourcePosition(1, 4), new LispSourcePosition(1, 5)), listValues[0].SourceLocation); // a
            Assert.Equal(new LispSourceLocation("TEST-INPUT", new LispSourcePosition(1, 6), new LispSourcePosition(1, 7)), listValues[1].SourceLocation); // b
            Assert.Equal(new LispSourceLocation("TEST-INPUT", new LispSourcePosition(1, 8), new LispSourcePosition(1, 9)), listValues[2].SourceLocation); // c

            var innerList = (LispList)listValues[3];
            Assert.Equal(new LispSourceLocation("TEST-INPUT", new LispSourcePosition(1, 10), new LispSourcePosition(1, 19)), innerList.SourceLocation); // ( 1 2 3 )

            var innerListValues = innerList.ToList();
            Assert.Equal(new LispSourceLocation("TEST-INPUT", new LispSourcePosition(1, 12), new LispSourcePosition(1, 13)), innerListValues[0].SourceLocation); // 1
            Assert.Equal(new LispSourceLocation("TEST-INPUT", new LispSourcePosition(1, 14), new LispSourcePosition(1, 15)), innerListValues[1].SourceLocation); // 2
            Assert.Equal(new LispSourceLocation("TEST-INPUT", new LispSourcePosition(1, 16), new LispSourcePosition(1, 17)), innerListValues[2].SourceLocation); // 3

            var nilList = (LispNilList)listValues[4];
            Assert.Equal(new LispSourceLocation("TEST-INPUT", new LispSourcePosition(1, 20), new LispSourcePosition(1, 23)), nilList.SourceLocation); // ( )

            // after newline
            var symbol = await ReadAsync("\na");
            Assert.Equal(new LispSourceLocation("TEST-INPUT", new LispSourcePosition(2, 1), new LispSourcePosition(2, 2)), symbol.SourceLocation);
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
        public async Task FunctionDocumentationHandlesMultilineIndentedText()
        {
            var host = await CreateHostAsync();
            var result = await host.EvalAsync("test.lisp", @"
(defun test-function ()
    ""First line of doc string.
     Second line of doc string.""
    ())
".Replace("\r", ""), host.CreateExecutionState());
            var function = host.GetValue<LispFunction>("TEST-FUNCTION");
            Assert.Equal("First line of doc string.\nSecond line of doc string.", function.Documentation);
        }

        [Fact]
        public async Task MacroDocumentationHandlesMultilineIndentedText()
        {
            var host = await CreateHostAsync();
            var result = await host.EvalAsync("test.lisp", @"
(defmacro test-macro ()
    ""First line of doc string.
     Second line of doc string.""
    ())
".Replace("\r", ""), host.CreateExecutionState());
            var macro = host.GetValue<LispMacro>("TEST-MACRO");
            Assert.Equal("First line of doc string.\nSecond line of doc string.", macro.Documentation);
        }

        [Fact]
        public async Task FunctionCanHaveOnlyOneStringStatementThatServesAsBothDocumentationStringAndBody()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            var evalResult = await host.EvalAsync("test.lisp", @"
(defun test-function ()
    ""doc string and content"")
(test-function)
".Replace("\r", ""), executionState);
            Assert.Equal("doc string and content", ((LispString)evalResult.Value).Value);
            var function = host.GetValue<LispFunction>("TEST-FUNCTION");
            Assert.Equal("doc string and content", function.Documentation);
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
            var host = await CreateHostAsync(output: output);
            var executionState = host.CreateExecutionState();
            var evalResult = await host.EvalAsync("test.lisp", @"
(with-open-file (file-stream ""test-file.dat"")
    (format t ""read: ~S~%"" (read file-stream))
    (format t ""evaluated: ~S~%"" (eval (read file-stream)))
)
", executionState);
            Assert.IsNotType<LispError>(evalResult.Value);
            Assert.Equal("read: \"just a string\"\nevaluated: 5\n", NormalizeNewlines(output.ToString()));
            Assert.Null(host.GetValue("FILE-STREAM"));
        }

        [Fact]
        public async Task WithOpenFile_Writing()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            using (var outputFile = new TemporaryFile(createFile: false))
            {
                var evalResult = await host.EvalAsync("test.lisp", $@"
(with-open-file (file-stream ""{outputFile.FilePath.Replace("\\", "\\\\")}"" :direction :output)
    (format file-stream ""wrote: ~S~%"" ""just-a-string"")
    (format file-stream ""wrote: ~S~%"" '(+ 2 3))
)
", executionState);
                Assert.IsNotType<LispError>(evalResult.Value);
                var actual = NormalizeNewlines(File.ReadAllText(outputFile.FilePath));
                Assert.Equal("wrote: \"just-a-string\"\nwrote: (+ 2 3)\n", actual);
                Assert.Null(host.GetValue("FILE-STREAM"));
            }
        }

        [Fact]
        public async Task SupportReaderMacros()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            var evalResult = await host.EvalAsync("test.lisp", @"
; %5 -> (* 100 5)
(defun percent-reader (stream char)
    (list (quote *) 100 (read stream t nil t)))
(set-macro-character #\% #'percent-reader)
%5
", executionState);
            Assert.Equal(new LispInteger(500), evalResult.Value);
        }

        [Fact]
        public async Task ReadTableCopying()
        {
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
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
            var evalResult = await host.EvalAsync("test.lisp", @"
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
", executionState);
            Assert.True(enteredOriginalBangReader);
            Assert.True(enteredNewBangReader);
            Assert.Equal(42, ((LispInteger)evalResult.Value).Value);
        }

        [Theory]
        [InlineData("`(1 ,(+ 2 3) (b))", "(1 5 (B))")]
        [InlineData("`,(+ 1 2)", "3")]
        [InlineData("`#(1 2 ,(+ 1 2) ,#(4))", "#(1 2 3 #(4))", "(APPLY #'VECTOR (QUOTE (1 2 3 #(4))))")]
        [InlineData("`a", "A")]
        public async Task BackQuoteReadAndEval(string code, string expected, string alternateExpected = null)
        {
            var (host, readResult) = await ReadHostAndResultAsync(code);
            var evalResult = host.EvalAsync(readResult, host.CreateExecutionState());
            var actual = evalResult.Result.Value.ToDisplayString(host.CurrentPackage);
            Assert.True(expected == actual || alternateExpected == actual, $"Expected: {expected} or {alternateExpected}, Actual: {actual}");
        }

        [Fact]
        public async Task PeekCharPeekTypeIsNotGiven()
        {
            var input = new StringReader(" ab ");
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();

            host.SetValue("TEST-STREAM", testStream);
            var evalResult = await host.EvalAsync("test.lisp", "(peek-char test-stream)", executionState);
            Assert.Equal(new LispCharacter(' '), evalResult.Value);
        }

        [Fact]
        public async Task PeekCharPeekTypeIsNil()
        {
            var input = new StringReader(" ab ");
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();

            host.SetValue("TEST-STREAM", testStream);
            var evalResult = await host.EvalAsync("test.lisp", "(peek-char nil test-stream)", executionState);
            Assert.Equal(new LispCharacter(' '), evalResult.Value);
        }

        [Fact]
        public async Task PeekCharPeekTypeIsTThenSymbol()
        {
            var input = new StringReader(" ab ");
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();

            host.SetValue("TEST-STREAM", testStream);
            var evalResult = await host.EvalAsync("test.lisp", "(peek-char t test-stream)", executionState);
            Assert.Equal(new LispCharacter('a'), evalResult.Value);
        }

        [Fact]
        public async Task PeekCharPeekTypeIsTThenComment()
        {
            var input = new StringReader(" ;ab ");
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();

            host.SetValue("TEST-STREAM", testStream);
            var evalResult = await host.EvalAsync("test.lisp", "(peek-char t test-stream)", executionState);
            Assert.Equal(new LispCharacter(';'), evalResult.Value);
        }

        [Fact]
        public async Task PeekCharPeekTypeIsCharacter()
        {
            var input = new StringReader(" ab ");
            var testStream = new LispTextStream("TEST-STREAM", input, TextWriter.Null);
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();

            host.SetValue("TEST-STREAM", testStream);
            var evalResult = await host.EvalAsync("test.lisp", "(peek-char #\\b test-stream)", executionState);
            Assert.Equal(new LispCharacter('b'), evalResult.Value);
        }

        [Fact]
        public async Task ObjectsFromReaderMacrosHaveProperSourceSpansSet1()
        {
            var text = "\"some string\"";
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            var input = new LispTextStream("test-input", new StringReader(text), TextWriter.Null);
            var evalResult = await host.EvalAsync(LispList.FromItems(new LispUnresolvedSymbol("READ"), input), executionState);
            Assert.Equal(new LispSourceLocation("test-input", new LispSourcePosition(1, 1), new LispSourcePosition(1, 14)), evalResult.Value.SourceLocation);
        }

        [Fact]
        public async Task ObjectsFromReaderMacrosHaveProperSourceSpansSet2()
        {
            var text = "#'asdf";
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            var input = new LispTextStream("test-input", new StringReader(text), TextWriter.Null);
            var evalResult = await host.EvalAsync(LispList.FromItems(new LispUnresolvedSymbol("READ"), input), executionState);
            Assert.Equal("#'COMMON-LISP-USER:ASDF", evalResult.Value.ToString());
            Assert.Equal(new LispSourceLocation("test-input", new LispSourcePosition(1, 1), new LispSourcePosition(1, 7)), evalResult.Value.SourceLocation);
        }

        [Fact]
        public async Task ObjectsAfterReaderMacroHaveProperSourceSpansSet1()
        {
            var text = "\"some string\" 42";
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            var input = new LispTextStream("test-input", new StringReader(text), TextWriter.Null);
            var evalResult = await host.EvalAsync(LispList.FromItems(new LispUnresolvedSymbol("READ"), input), executionState); // swallow the string
            evalResult = await host.EvalAsync(LispList.FromItems(new LispUnresolvedSymbol("READ"), input), executionState);
            Assert.Equal(new LispSourceLocation("test-input", new LispSourcePosition(1, 15), new LispSourcePosition(1, 17)), evalResult.Value.SourceLocation);
        }

        [Fact]
        public async Task ObjectsAfterReaderMacroHaveProperSourceSpansSet2()
        {
            var text = "#'asdf 42";
            var host = await CreateHostAsync();
            var executionState = host.CreateExecutionState();
            var input = new LispTextStream("test-input", new StringReader(text), TextWriter.Null);
            var evalResult = await host.EvalAsync(LispList.FromItems(new LispUnresolvedSymbol("READ"), input), executionState); // swallow the function reference
            evalResult = await host.EvalAsync(LispList.FromItems(new LispUnresolvedSymbol("READ"), input), executionState);
            Assert.Equal(new LispSourceLocation("test-input", new LispSourcePosition(1, 8), new LispSourcePosition(1, 10)), evalResult.Value.SourceLocation);
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
        public async Task SymbolFromDeepPackageIsResolvedCorrectly()
        {
            var code = @"
; define X in package A
(defpackage :a)
(in-package :a)
(setf x 1)

; package B inherits from package A
(defpackage :b
    (:use :a))
(in-package :b)

; package C inherits from package B
(defpackage :c
    (:use :b))
(in-package :c)

; now get symbol X; it's reported package should be A
(quote x)
";

            var host = await CreateHostAsync(useInitScript: false);
            var executionState = LispExecutionState.CreateExecutionState(host.RootFrame, allowHalting: false);
            var evalResult = await host.EvalAsync("test.lisp", code, executionState);
            var symbol = (LispResolvedSymbol)evalResult.Value;
            Assert.Equal("A", symbol.PackageName);
        }

        [Fact]
        public async Task SymbolFromDeepPackageThatDoesNotExistIsResolvedCorrectly()
        {
            var code = @"
; define X in package A
(defpackage :a)
(in-package :a)
(setf x 1)

; package B inherits from package A
(defpackage :b
    (:use :a))
(in-package :b)

; package C inherits from package B
(defpackage :c
    (:use :b))
(in-package :c)

; now get symbol Y; it's reported package should be C
(quote y)
";

            var host = await CreateHostAsync(useInitScript: false);
            var executionState = LispExecutionState.CreateExecutionState(host.RootFrame, allowHalting: false);
            var evalResult = await host.EvalAsync("test.lisp", code, executionState);
            var symbol = (LispResolvedSymbol)evalResult.Value;
            Assert.Equal("C", symbol.PackageName);
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
