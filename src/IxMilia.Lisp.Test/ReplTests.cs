using System.IO;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class ReplTests : TestBase
    {
        [Fact]
        public async Task Simple()
        {
            var repl = await LispRepl.CreateAsync();
            var result = await repl.EvalAsync("(+ (+ 1 2) (+ 3 4");
            Assert.Null(result.LastResult);
            Assert.Equal(2, result.ExpressionDepth);

            result = await repl.EvalAsync(")");
            Assert.Null(result.LastResult);
            Assert.Equal(1, result.ExpressionDepth);

            result = await repl.EvalAsync(")");
            Assert.Equal(new LispInteger(10), result.LastResult);
            Assert.Equal(0, result.ExpressionDepth);
        }

        [Fact]
        public async Task CompleteAndIncompleteSubmission()
        {
            var repl = await LispRepl.CreateAsync();
            var result = await repl.EvalAsync("(+ 1 2)(+ 5 6");
            Assert.Equal(new LispInteger(3), result.LastResult);
            Assert.Equal(1, result.ExpressionDepth);

            result = await repl.EvalAsync(")");
            Assert.Equal(new LispInteger(11), result.LastResult);
            Assert.Equal(0, result.ExpressionDepth);
        }

        [Fact]
        public async Task ReplErrorsArePropagated()
        {
            var repl = await LispRepl.CreateAsync();
            var result = await repl.EvalAsync("(+ 1 abcd)(+ 2 3)");
            var error = (LispError)result.LastResult;
            Assert.Equal("Symbol 'ABCD' not found", error.Message);
            Assert.Equal(1, error.SourceLocation?.Start.Line);
            Assert.Equal(6, error.SourceLocation?.Start.Column);
        }

        [Fact]
        public async Task FunctionTracing()
        {
            var traceWriter = new StringWriter();
            var repl = await LispRepl.CreateAsync(traceWriter: traceWriter);
            await repl.EvalAsync(@"
(defun half (n) (* n 0.5))
(defun average (x y)
    (+ (half x) (half y)))
(trace half average)
");
            await repl.EvalAsync("(average 3 7)");
            var actual = NormalizeNewlines(traceWriter.ToString().Trim('\r', '\n'));
            var expected = NormalizeNewlines(@"
0: (AVERAGE 3 7)
 1: (HALF 3)
 1: returned 1.5
 1: (HALF 7)
 1: returned 3.5
0: returned 5
".Trim('\r', '\n'));
            Assert.Equal(expected, actual);
        }

        [Fact]
        public async Task DribbleLogging()
        {
            var output = new StringWriter();
            var repl = await LispRepl.CreateAsync(output: output);
            using (var tempFile = new TemporaryFile(createFile: false))
            {
                var result = await repl.EvalAsync($@"
(+ 1 1) ; this shouldn't be in the log
(dribble ""{tempFile.FilePath.Replace("\\", "\\\\")}"") ; start recording
(+ 3 3) ; this should be in the log
(dribble) ; stop recording
(+ 5 5) ; this shouldn't be in the log
");
                var consoleOutput = NormalizeNewlines(output.ToString()).Trim();
                var expectedConsoleOutput = NormalizeNewlines($@"
Now recording in file {tempFile.FilePath}
Finished recording in file {tempFile.FilePath}
").Trim();
                Assert.Equal(expectedConsoleOutput, consoleOutput);

                var logContents = NormalizeNewlines(File.ReadAllText(tempFile.FilePath).Trim());
                // trim non-deterministic time stamp
                logContents = Regex.Replace(logContents, ";Recording started at .*$", ";Recording started at <TIME-STAMP>", RegexOptions.Multiline);
                var expectedLogContents = NormalizeNewlines($@"
;Recording in {tempFile.FilePath}
;Recording started at <TIME-STAMP>

> (+ 3 3)
6

> (DRIBBLE)
".Trim());
                Assert.Equal(expectedLogContents, logContents);
            }
        }

        [Fact]
        public async Task GetObjectAtLocationGetsNarrowestObject()
        {
            var markedCode = @"
(+ 1 2)
(+ (* 12 3$$4) (/ 56 78))
(- 1 2)
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var repl = await LispRepl.CreateAsync();
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            Assert.IsType<LispInteger>(parseResult.Object);
            Assert.Equal(34, ((LispInteger)parseResult.Object).Value);
        }

        [Fact]
        public async Task GetObjectAtLocationGetsParentObjectIfNoNarrowChildExists()
        {
            var markedCode = @"
(+ 1 2)
(+ (* 12 34) $$ (/ 56 78))
(- 1 2)
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var repl = await LispRepl.CreateAsync();
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            Assert.IsType<LispList>(parseResult.Object);
            Assert.Equal("(+ (* 12 34) (/ 56 78))", parseResult.Object.ToString());
        }

        [Fact]
        public async Task GetObjectAtLocationReturnsNullIfNothingIsAvailable()
        {
            var markedCode = @"
(+ 1 2)
$$
(- 1 2)
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var repl = await LispRepl.CreateAsync();
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            Assert.Null(parseResult.Object);
        }

        [Fact]
        public async Task GetMarkdownDisplayFromFunctionSourceObject()
        {
            var markedCode = @"
(add-2-num$$bers 3 5)
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var repl = await LispRepl.CreateAsync();
            await repl.EvalAsync("(defun add-2-numbers (a b) \"Adds 2 numbers.\" (+ a b))");
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            var markdown = parseResult.GetMarkdownDisplay().Replace("\r", "");
            var expected = @"
``` lisp
; <in module COMMON-LISP-USER>
(DEFUN ADD-2-NUMBERS (A B) ...)
```

Adds 2 numbers.
".Trim().Replace("\r", "");
            Assert.Equal(expected, markdown);
        }

        [Fact]
        public async Task GetMarkdownDisplayFromNumberSourceObject()
        {
            var markedCode = @"
3.14$$159
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var repl = await LispRepl.CreateAsync();
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            var markdown = parseResult.GetMarkdownDisplay().Replace("\r", "");
            Assert.Equal("3.14159", markdown);
        }

        [Fact]
        public async Task GetMarkdownDisplayFromUndefinedObject()
        {
            var markedCode = @"
as$$df
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var repl = await LispRepl.CreateAsync();
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            var markdown = parseResult.GetMarkdownDisplay();
            Assert.Null(markdown);
        }

        [Fact]
        public async Task GetMarkdownDisplayFromUnevaluatedParsedFunction()
        {
            var markedCode = @"
(defun some-function (a b)
    ())
(some-$$function 1 2)
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var repl = await LispRepl.CreateAsync();
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            var markdown = parseResult.GetMarkdownDisplay().Replace("\r", "");
            Assert.Contains("DEFUN SOME-FUNCTION", markdown);
        }

        [Fact]
        public async Task GetMarkdownDisplayFromUnevaludatedParsedMacro()
        {
            var markedCode = @"
(defmacro some-macro (a b)
    ())
(some-$$macro 1 2)
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var repl = await LispRepl.CreateAsync();
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            var markdown = parseResult.GetMarkdownDisplay().Replace("\r", "");
            Assert.Contains("DEFMACRO SOME-MACRO", markdown);
        }

        [Fact]
        public async Task GetMarkdownDisplayFromUnevaludatedParsedSet()
        {
            var markedCode = @"
(setf some-value (+ 1 1))
some-$$value
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var repl = await LispRepl.CreateAsync();
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            var markdown = parseResult.GetMarkdownDisplay().Replace("\r", "");
            Assert.Contains("(+ 1 1)", markdown);
        }

        [Fact]
        public async Task GetMarkdownDisplayFromUnevaluatedShadowedValues()
        {
            var markedCode = @"
(defun some-value () ; this is shadowed
    ())
(setf some-value 2222) ; this is the returned value
some-$$value
(setf some-value 3333) ; this is ignored
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var repl = await LispRepl.CreateAsync();
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            var markdown = parseResult.GetMarkdownDisplay().Replace("\r", "");
            Assert.Contains("2222", markdown);
        }

        [Fact]
        public async Task GetMarkdownDisplayFromUnevaluatedParsedSetInUnevaluatedFunction()
        {
            var repl = await LispRepl.CreateAsync();
            repl.Host.SetValue("some-value", new LispInteger(111)); // top-level value that's shadowed
            var markedCode = @"
(setf some-value 222) ; this is also shadowed
(defun some-function ()
    (setf some-value 333) ; this is the one we want to see
    some-$$value)
(setf some-value 444) ; this is never reached
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            var markdown = parseResult.GetMarkdownDisplay().Replace("\r", "");
            Assert.Contains("333", markdown);
        }

        [Fact]
        public async Task BoundValuesCanBeDeterminedFromAlreadyExecutedCode()
        {
            var repl = await LispRepl.CreateAsync();
            var result = await repl.EvalAsync("(setf the-answer 42)");
            Assert.IsNotType<LispError>(result.LastResult);
            var markedCode = @"the-$$answer";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            Assert.Contains(
                parseResult.VisibleValues.Values,
                boundSymbol => boundSymbol.Symbol.Value == "COMMON-LISP-USER:THE-ANSWER" && boundSymbol.Value is LispInteger i && i.Value == 42);
        }

        [Fact]
        public async Task BoundValuesCanBeDeterminedAtTheRoot()
        {
            var repl = await LispRepl.CreateAsync();
            var markedCode = @"
(setf the-answer 42)
the-$$answer
(setf then-answer 84)
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            Assert.Contains(
                parseResult.VisibleValues.Values,
                boundSymbol => boundSymbol.Symbol.Value == "COMMON-LISP-USER:THE-ANSWER" && boundSymbol.Value is LispInteger i && i.Value == 42);
        }

        [Fact]
        public async Task BoundValuesCanBeDeterminedFromWithinAFunctionDefinition()
        {
            var repl = await LispRepl.CreateAsync();
            var markedCode = @"
(setf the-answer 11)
(defun some-function ()
    (setf the-answer 22)
    (setf the-answer 42)
    the-$$answer
    (setf the-answer 33))
(setf the-answer 44)
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            Assert.Contains(
                parseResult.VisibleValues.Values,
                boundSymbol => boundSymbol.Symbol.Value == "COMMON-LISP-USER:THE-ANSWER" && boundSymbol.Value is LispInteger i && i.Value == 42);
        }

        [Fact]
        public async Task BoundValuesCanBeDeterminedFromFunctionArguments()
        {
            var repl = await LispRepl.CreateAsync();
            var markedCode = @"
(defun incomplete-function (some-parameter some-other-parameter)
    $$())
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            Assert.Contains(
                parseResult.VisibleValues.Values,
                boundSymbol => boundSymbol.Value is LispSymbol s && s.LocalName == "SOME-PARAMETER");
        }

        [Fact]
        public async Task BoundValuesCanBeDeterminedFromMacroArguments()
        {
            var repl = await LispRepl.CreateAsync();
            var markedCode = @"
(defmacro incomplete-function (some-parameter some-other-parameter)
    $$())
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            Assert.Contains(
                parseResult.VisibleValues.Values,
                boundSymbol => boundSymbol.Value is LispSymbol s && s.LocalName == "SOME-PARAMETER");
        }

        [Fact]
        public async Task BoundValuesCanBeDeterminedFromWithinAnIncompleteList()
        {
            var repl = await LispRepl.CreateAsync();
            var markedCode = @"
(defun some-function ()
    ())
'(some list $$
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            Assert.Contains(
                parseResult.VisibleValues.Values,
                boundSymbol => boundSymbol.Value is LispFunction f && f.NameSymbol.LocalName == "SOME-FUNCTION");
        }

        [Fact]
        public async Task BoundValueOfFunctionParameterCanBeReturnedFromAnIncompleteDefinition()
        {
            var repl = await LispRepl.CreateAsync();
            var markedCode = @"
(defun incomplete-function (some-parameter some-other-parameter)
    $$
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            Assert.Contains(
                parseResult.VisibleValues.Values,
                boundSymbol => boundSymbol.Value is LispSymbol s && s.LocalName == "SOME-PARAMETER");
        }

        [Fact]
        public async Task VisibleValuesAreHandledAfterAColonCharacter()
        {
            var repl = await LispRepl.CreateAsync();
            var markedCode = @"
(setf some-value 0)
common-lisp-user:$$
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            Assert.Contains(
                parseResult.VisibleValues.Values,
                boundSymbol => boundSymbol.Symbol.LocalName == "SOME-VALUE");
        }

        [Fact]
        public async Task ParsedObjectCanBeAnIncompleteResolvedSymbol()
        {
            var repl = await LispRepl.CreateAsync();
            var markedCode = @"common-lisp-user:$$";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var parseResult = await repl.ParseUntilSourceLocationAsync(code, position);
            var resolvedSymbol = (LispResolvedSymbol)parseResult.Object;
            Assert.Equal("COMMON-LISP-USER", resolvedSymbol.PackageName);
            Assert.Equal("", resolvedSymbol.LocalName);
        }
    }
}
