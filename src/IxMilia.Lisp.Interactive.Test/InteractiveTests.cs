using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using IxMilia.Lisp.Test;
using Microsoft.DotNet.Interactive;
using Microsoft.DotNet.Interactive.Commands;
using Microsoft.DotNet.Interactive.Events;
using Xunit;

namespace IxMilia.Lisp.Interactive.Test
{
    public class InteractiveTests : TestBase
    {
        [Fact]
        public async Task GetCompletions()
        {
            var markedCode = @"KERNEL:$$";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var kernel = new LispKernel();
            var commandResult = await kernel.SendAsync(new RequestCompletions(code, GetLinePosition(position)), CancellationToken.None);
            AssertNoErrors(commandResult.Events);
            var completionsProduced = commandResult.Events.OfType<CompletionsProduced>().Single();
            var _ = completionsProduced.Completions.Where(c => c.DisplayText == "KERNEL:+/2").Single();
        }

        [Fact]
        public async Task GetHoverText()
        {
            var markedCode = @"N$$IL";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var kernel = new LispKernel();
            var commandResult = await kernel.SendAsync(new RequestHoverText(code, GetLinePosition(position)), CancellationToken.None);
            AssertNoErrors(commandResult.Events);
            var hoverText = commandResult.Events.OfType<HoverTextProduced>().Single();
            Assert.Equal("()", hoverText.Content.Single().Value);
        }

        [Fact]
        public async Task RequestValueInfosInitiallyProducesEmptySet()
        {
            var kernel = new LispKernel();
            var commandResult = await kernel.SendAsync(new RequestValueInfos(kernel.Name));
            AssertNoErrors(commandResult.Events);
            var valueInfosProduced = commandResult.Events.OfType<ValueInfosProduced>().Single();
            Assert.Empty(valueInfosProduced.ValueInfos);
        }

        [Fact]
        public async Task RequestValueInfosReturnsOnlyWhatHasBeenSet()
        {
            var kernel = new LispKernel();
            var commandResult = await kernel.SendAsync(new SubmitCode("(setf x 1)(setf y 2)"));
            AssertNoErrors(commandResult.Events);

            commandResult = await kernel.SendAsync(new RequestValueInfos(kernel.Name));
            AssertNoErrors(commandResult.Events);
            var valueInfosProduced = commandResult.Events.OfType<ValueInfosProduced>().Single();
            var valueInfos = valueInfosProduced.ValueInfos.Select(v => v.Name).ToArray();
            Assert.Equal("X,Y", string.Join(",", valueInfos));
        }

        [Fact]
        public async Task RequestValueInfosDoesNotReportFunctionsOrMacros()
        {
            var kernel = new LispKernel();
            var commandResult = await kernel.SendAsync(new SubmitCode("(setf x 1)(setf y 2)(defun some-function () ())(defmacro some-macro () ())"));
            AssertNoErrors(commandResult.Events);

            commandResult = await kernel.SendAsync(new RequestValueInfos(kernel.Name));
            AssertNoErrors(commandResult.Events);
            var valueInfosProduced = commandResult.Events.OfType<ValueInfosProduced>().Single();
            var valueInfos = valueInfosProduced.ValueInfos.Select(v => v.Name).ToArray();
            Assert.Equal("X,Y", string.Join(",", valueInfos));
        }

        [Theory]
        [InlineData("text/plain", "X", "(1 2 3)")]
        [InlineData("text/plain", "COMMON-LISP-USER:X", "(1 2 3)")]
        [InlineData("application/json", "X", "[1,2,3]")]
        [InlineData("application/json", "COMMON-LISP-USER:X", "[1,2,3]")]
        public async Task RequestValueReturnsTheAppropriateValue(string mimeType, string valueName, string expectedResult)
        {
            var kernel = new LispKernel();
            var commandResult = await kernel.SendAsync(new SubmitCode("(setf x '(1 2 3))"));
            AssertNoErrors(commandResult.Events);

            commandResult = await kernel.SendAsync(new RequestValue(valueName, mimeType: mimeType));
            AssertNoErrors(commandResult.Events);
            var valueProduced = commandResult.Events.OfType<ValueProduced>().Single();
            Assert.Equal(valueName, valueProduced.Name);
            Assert.Equal(mimeType, valueProduced.FormattedValue.MimeType);
            Assert.Equal(expectedResult, valueProduced.FormattedValue.Value);
        }

        [Fact]
        public async Task SubmitCodeProducesAResult()
        {
            var code = "(+ 1 2)";
            var kernel = new LispKernel();
            var commandResult = await kernel.SendAsync(new SubmitCode(code));
            AssertNoErrors(commandResult.Events);
            var returnValue = commandResult.Events.OfType<ReturnValueProduced>().Single();
            Assert.Equal("3", returnValue.FormattedValues.Single(f => f.MimeType == "text/plain").Value);
        }

        [Fact]
        public async Task SubmitCodeRedirectsStandardOut()
        {
            var code = "(format t \"stdout\")";
            var kernel = new LispKernel();
            var commandResult = await kernel.SendAsync(new SubmitCode(code));
            AssertNoErrors(commandResult.Events);
            var stdOut = commandResult.Events.OfType<StandardOutputValueProduced>().Single();
            Assert.Equal("stdout", stdOut.FormattedValues.Single().Value);
        }

        [Fact]
        public async Task SubmitCodeDoesNotReturnAnythingForNil()
        {
            var code = "()";
            var kernel = new LispKernel();
            var commandResult = await kernel.SendAsync(new SubmitCode(code));
            AssertNoErrors(commandResult.Events);
            Assert.Empty(commandResult.Events.OfType<DisplayEvent>());
        }

        [Fact]
        public async Task SendValueSetsValueInKernel()
        {
            var kernel = new LispKernel();
            var command = new SendValue("x", null, new FormattedValue("application/json", "[1,2]"));
            var commandResult = await kernel.SendAsync(command);
            AssertNoErrors(commandResult.Events);

            commandResult = await kernel.SendAsync(new RequestValueInfos());
            AssertNoErrors(commandResult.Events);
            var valueInfos = commandResult.Events.OfType<ValueInfosProduced>().Single();
            var valueInfo = valueInfos.ValueInfos.Single();
            Assert.Equal("X", valueInfo.Name);
            Assert.Equal("text/plain+summary", valueInfo.FormattedValue.MimeType);
            Assert.Equal("(1 2)", valueInfo.FormattedValue.Value);
        }

        private static void AssertNoErrors(IEnumerable<KernelEvent> events)
        {
            foreach (var e in events)
            {
                if (e is CommandFailed failed)
                {
                    throw new Exception(failed.Message);
                }
            }
        }

        private static LinePosition GetLinePosition(LispSourcePosition position)
        {
            return new LinePosition(position.Line - 1, position.Column - 1);
        }
    }
}
