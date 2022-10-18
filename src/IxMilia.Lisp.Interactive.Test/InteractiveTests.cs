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
            var events = GetEventList(commandResult.KernelEvents);
            AssertNoErrors(events);
            var completionsProduced = events.OfType<CompletionsProduced>().Single();
            var _ = completionsProduced.Completions.Where(c => c.DisplayText == "KERNEL:+/2").Single();
        }

        [Fact]
        public async Task GetHoverText()
        {
            var markedCode = @"N$$IL";
            GetCodeAndPosition(markedCode, out var code, out var position);
            var kernel = new LispKernel();
            var commandResult = await kernel.SendAsync(new RequestHoverText(code, GetLinePosition(position)), CancellationToken.None);
            var events = GetEventList(commandResult.KernelEvents);
            AssertNoErrors(events);
            var hoverText = events.OfType<HoverTextProduced>().Single();
            Assert.Equal("()", hoverText.Content.Single().Value);
        }

        [Fact]
        public async Task RequestValueInfosInitiallyProducesEmptySet()
        {
            var kernel = new LispKernel();
            var commandResult = await kernel.SendAsync(new RequestValueInfos(kernel.Name));
            var events = GetEventList(commandResult.KernelEvents);
            AssertNoErrors(events);
            var valueInfosProduced = events.OfType<ValueInfosProduced>().Single();
            Assert.Empty(valueInfosProduced.ValueInfos);
        }

        [Fact]
        public async Task RequestValueInfosReturnsOnlyWhatHasBeenSet()
        {
            var kernel = new LispKernel();
            var commandResult = await kernel.SendAsync(new SubmitCode("(setf x 1)(setf y 2)"));
            var events = GetEventList(commandResult.KernelEvents);
            AssertNoErrors(events);

            commandResult = await kernel.SendAsync(new RequestValueInfos(kernel.Name));
            events = GetEventList(commandResult.KernelEvents);
            AssertNoErrors(events);
            var valueInfosProduced = events.OfType<ValueInfosProduced>().Single();
            var valueInfos = valueInfosProduced.ValueInfos.Select(v => v.Name).ToArray();
            Assert.Equal("X,Y", string.Join(",", valueInfos));
        }

        [Fact]
        public async Task RequestValueInfosDoesNotReportFunctionsOrMacros()
        {
            var kernel = new LispKernel();
            var commandResult = await kernel.SendAsync(new SubmitCode("(setf x 1)(setf y 2)(defun some-function () ())(defmacro some-macro () ())"));
            var events = GetEventList(commandResult.KernelEvents);
            AssertNoErrors(events);

            commandResult = await kernel.SendAsync(new RequestValueInfos(kernel.Name));
            events = GetEventList(commandResult.KernelEvents);
            AssertNoErrors(events);
            var valueInfosProduced = events.OfType<ValueInfosProduced>().Single();
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
            var events = GetEventList(commandResult.KernelEvents);
            AssertNoErrors(events);

            commandResult = await kernel.SendAsync(new RequestValue(valueName, mimeType: mimeType));
            events = GetEventList(commandResult.KernelEvents);
            AssertNoErrors(events);
            var valueProduced = events.OfType<ValueProduced>().Single();
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
            var events = GetEventList(commandResult.KernelEvents);
            AssertNoErrors(events);
            var returnValue = events.OfType<ReturnValueProduced>().Single();
            Assert.Equal("3", returnValue.FormattedValues.Single().Value);
        }

        [Fact]
        public async Task SubmitCodeRedirectsStandardOut()
        {
            var code = "(format t \"stdout\")";
            var kernel = new LispKernel();
            var commandResult = await kernel.SendAsync(new SubmitCode(code));
            var events = GetEventList(commandResult.KernelEvents);
            AssertNoErrors(events);
            var stdOut = events.OfType<StandardOutputValueProduced>().Single();
            Assert.Equal("stdout", stdOut.FormattedValues.Single().Value);
        }

        [Fact]
        public async Task SubmitCodeDoesNotReturnAnythingForNil()
        {
            var code = "()";
            var kernel = new LispKernel();
            var commandResult = await kernel.SendAsync(new SubmitCode(code));
            var events = GetEventList(commandResult.KernelEvents);
            AssertNoErrors(events);
            Assert.Empty(events.OfType<DisplayEvent>());
        }

        [Fact]
        public async Task VariableDeclarerSetsValueInKernel()
        {
            var kernel = new LispKernel();
            var command = new SendValue("X", null, new FormattedValue("application/json", "[1,2]"));
            var commandResult = await kernel.SendAsync(command);
            var events = GetEventList(commandResult.KernelEvents);
            AssertNoErrors(events);

            commandResult = await kernel.SendAsync(new SubmitCode("X"));
            events = GetEventList(commandResult.KernelEvents);
            AssertNoErrors(events);
            var ret = events.OfType<ReturnValueProduced>().Single();
            Assert.Equal("(1 2)", ret.FormattedValues.Single().Value);
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

        private static IList<KernelEvent> GetEventList(IObservable<KernelEvent> observable)
        {
            var result = new List<KernelEvent>();
            observable.Subscribe(e =>
            {
                result.Add(e);
            });
            return result;
        }
    }
}
