using System;
using System.Linq;
using System.Threading.Tasks;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class SourceParsingTests : TestBase
    {
        private static async Task<(LispHost Host, LispParseResult Result)> ParseAndGetHostAndResult(string markedCode)
        {
            GetCodeAndPosition(markedCode, out var code, out var position);
            var configuration = new LispHostConfiguration(useTailCalls: true); // TODO: this doesn't work with tail calls
            var host = await LispHost.CreateAsync(configuration);
            var parseResult = await host.ParseUntilSourceLocationAsync("test-input", code, position);
            return (host, parseResult);
        }

        private static async Task<LispParseResult> Parse(string markedCode)
        {
            var pair = await ParseAndGetHostAndResult(markedCode);
            return pair.Result;
        }

        [Fact]
        public async Task GetObjectAtLocationGetsNarrowestObject()
        {
            var markedCode = @"
(+ 1 2)
(+ (* 12 3$$4) (/ 56 78))
(- 1 2)
";
            var parseResult = await Parse(markedCode);
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
            var pair = await ParseAndGetHostAndResult(markedCode);
            Assert.IsType<LispList>(pair.Result.Object);
            Assert.Equal("(+ (* 12 34) (/ 56 78))", pair.Result.Object.ToDisplayString(pair.Host.CurrentPackage));
        }

        [Fact]
        public async Task GetObjectAtLocationReturnsNullIfNothingIsAvailable()
        {
            var markedCode = @"
(+ 1 2)
$$
(- 1 2)
";
            var parseResult = await Parse(markedCode);
            Assert.Null(parseResult.Object);
        }

        [Fact]
        public async Task GetMarkdownDisplayFromFunctionSourceObject()
        {
            var markedCode = @"
(add-2-num$$bers 3 5)
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var host = await LispHost.CreateAsync();
            var executionState = host.CreateExecutionState();
            await host.EvalAsync("test-input", "(defun add-2-numbers (a b) \"Adds 2 numbers.\" (+ a b))", executionState);
            var parseResult = await host.ParseUntilSourceLocationAsync("test-input", code, position);
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
            var parseResult = await Parse(markedCode);
            var markdown = parseResult.GetMarkdownDisplay().Replace("\r", "");
            Assert.Equal("3.14159", markdown);
        }

        [Fact]
        public async Task GetMarkdownDisplayFromUndefinedObject()
        {
            var markedCode = @"
as$$df
";
            var parseResult = await Parse(markedCode);
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
            var parseResult = await Parse(markedCode);
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
            var parseResult = await Parse(markedCode);
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
            var parseResult = await Parse(markedCode);
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
            var parseResult = await Parse(markedCode);
            var markdown = parseResult.GetMarkdownDisplay().Replace("\r", "");
            Assert.Contains("2222", markdown);
        }

        [Fact]
        public async Task GetMarkdownDisplayFromUnevaluatedParsedSetInUnevaluatedFunction()
        {
            var host = await LispHost.CreateAsync();
            host.SetValue("some-value", new LispInteger(111)); // top-level value that's shadowed
            var markedCode = @"
(setf some-value 222) ; this is also shadowed
(defun some-function ()
    (setf some-value 333) ; this is the one we want to see
    some-$$value)
(setf some-value 444) ; this is never reached
";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var parseResult = await host.ParseUntilSourceLocationAsync("test-input", code, position);
            var markdown = parseResult.GetMarkdownDisplay().Replace("\r", "");
            Assert.Contains("333", markdown);
        }

        [Fact]
        public async Task BoundValuesCanBeDeterminedFromAlreadyExecutedCode()
        {
            var host = await LispHost.CreateAsync();
            var executionState = host.CreateExecutionState();
            var evalResult = await host.EvalAsync("test-input", "(setf the-answer 42)", executionState);
            Assert.IsNotType<LispError>(evalResult.Value);
            var markedCode = @"the-$$answer";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var parseResult = await host.ParseUntilSourceLocationAsync("test-input", code, position);
            Assert.Contains(
                parseResult.VisibleValues.Values,
                boundSymbol => boundSymbol.Symbol.Value == "COMMON-LISP-USER:THE-ANSWER" && boundSymbol.Value is LispInteger i && i.Value == 42);
        }

        [Fact]
        public async Task BoundValuesCanBeDeterminedAtTheRoot()
        {
            var markedCode = @"
(setf the-answer 42)
the-$$answer
(setf then-answer 84)
";
            var parseResult = await Parse(markedCode);
            Assert.Contains(
                parseResult.VisibleValues.Values,
                boundSymbol => boundSymbol.Symbol.Value == "COMMON-LISP-USER:THE-ANSWER" && boundSymbol.Value is LispInteger i && i.Value == 42);
        }

        [Fact]
        public async Task BoundValuesCanBeDeterminedFromWithinAFunctionDefinition()
        {
            var markedCode = @"
(setf the-answer 11)
(defun some-function ()
    (setf the-answer 22)
    (setf the-answer 42)
    the-$$answer
    (setf the-answer 33))
(setf the-answer 44)
";
            var parseResult = await Parse(markedCode);
            Assert.Contains(
                parseResult.VisibleValues.Values,
                boundSymbol => boundSymbol.Symbol.Value == "COMMON-LISP-USER:THE-ANSWER" && boundSymbol.Value is LispInteger i && i.Value == 42);
        }

        [Fact]
        public async Task BoundValuesCanBeDeterminedFromFunctionArguments()
        {
            var markedCode = @"
(defun incomplete-function (some-parameter some-other-parameter)
    $$())
";
            var parseResult = await Parse(markedCode);
            Assert.Contains(
                parseResult.VisibleValues.Values,
                boundSymbol => boundSymbol.Value is LispSymbol s && s.LocalName == "SOME-PARAMETER");
        }

        [Fact]
        public async Task BoundValuesCanBeDeterminedFromMacroArguments()
        {
            var markedCode = @"
(defmacro incomplete-function (some-parameter some-other-parameter)
    $$())
";
            var parseResult = await Parse(markedCode);
            Assert.Contains(
                parseResult.VisibleValues.Values,
                boundSymbol => boundSymbol.Value is LispSymbol s && s.LocalName == "SOME-PARAMETER");
        }

        [Fact]
        public async Task BoundValuesCanBeDeterminedFromWithinAnIncompleteList()
        {
            var markedCode = @"
(defun some-function ()
    ())
'(some list $$
";
            var parseResult = await Parse(markedCode);
            Assert.Contains(
                parseResult.VisibleValues.Values,
                boundSymbol => boundSymbol.Value is LispFunction f && f.NameSymbol.LocalName == "SOME-FUNCTION");
        }

        [Fact]
        public async Task BoundValueOfFunctionParameterCanBeReturnedFromAnIncompleteDefinition()
        {
            var markedCode = @"
(defun incomplete-function (some-parameter some-other-parameter)
    $$
";
            var parseResult = await Parse(markedCode);
            Assert.Contains(
                parseResult.VisibleValues.Values,
                boundSymbol => boundSymbol.Value is LispSymbol s && s.LocalName == "SOME-PARAMETER");
        }

        [Fact]
        public async Task VisibleValuesAreHandledAfterAColonCharacter()
        {
            var markedCode = @"
(setf some-value 0)
common-lisp-user:$$
";
            var parseResult = await Parse(markedCode);
            Assert.Contains(
                parseResult.VisibleValues.Values,
                boundSymbol => boundSymbol.Symbol.LocalName == "SOME-VALUE");
        }

        [Fact]
        public async Task ParsedObjectCanBeAnIncompleteResolvedSymbol()
        {
            var markedCode = @"common-lisp-user:$$";
            var parseResult = await Parse(markedCode);
            var resolvedSymbol = (LispResolvedSymbol)parseResult.Object;
            Assert.Equal("COMMON-LISP-USER", resolvedSymbol.PackageName);
            Assert.Equal("", resolvedSymbol.LocalName);
        }

        [Fact]
        public async Task NoCompletionItemsInATerminatedString()
        {
            var markedCode = "\"in a string$$\"";
            var (host, parseResult) = await ParseAndGetHostAndResult(markedCode);
            var completionItems = parseResult.GetReducedCompletionItems(host.CurrentPackage, (symbol, value) => symbol.ToDisplayString(host.CurrentPackage), name => name).ToArray();
            Assert.Empty(completionItems);
        }

        [Fact]
        public async Task CompletionItemsContainOtherPackagesButNotTheContents()
        {
            var markedCode = @"($$";
            var (host, parseResult) = await ParseAndGetHostAndResult(markedCode);
            var completionItems = parseResult.GetReducedCompletionItems(host.CurrentPackage, (symbol, value) => symbol.ToDisplayString(host.CurrentPackage), name => name).ToArray();
            Assert.Single(completionItems.Where(i => i == "KERNEL"));
            Assert.Empty(completionItems.Where(i => i == "KERNEL:+/2"));
        }

        [Fact]
        public async Task CompletionItemsAreCorrectlyScopedToTheUserSpecifiedPackage()
        {
            var markedCode = @"KERNEL:$$";
            var (host, parseResult) = await ParseAndGetHostAndResult(markedCode);
            var completionItems = parseResult.GetReducedCompletionItems(host.CurrentPackage, (symbol, value) => symbol.ToDisplayString(host.CurrentPackage), name => name).ToArray();
            Assert.True(completionItems.All(i => i.StartsWith("KERNEL:")));
        }

        [Fact]
        public async Task CompletionItemsAreReturnedForPartialMatches()
        {
            var markedCode = "def$$";
            var (host, parseResult) = await ParseAndGetHostAndResult(markedCode);
            var completionItems = parseResult.GetReducedCompletionItems(host.CurrentPackage, (symbol, value) => symbol.ToString(), name => name).ToArray();
            Assert.Single(completionItems.Where(i => i == "COMMON-LISP:DEFUN"));
        }
    }
}
