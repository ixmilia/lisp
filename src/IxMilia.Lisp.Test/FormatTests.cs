using System.Collections.Generic;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class FormatTests : TestBase
    {
        [Fact]
        public void NewlineDirectives()
        {
            Assert.True(LispFormatter.TryFormatString("a~%b", null, out var result), $"Error from formatter: {result}");
            Assert.Equal("a\nb", NormalizeNewlines(result));
        }

        [Fact]
        public void EnsureLineStartDirective()
        {
            Assert.True(LispFormatter.TryFormatString("a~&b~&~&~&c", null, out var result), $"Error from formatter: {result}");
            Assert.Equal("a\nb\nc", NormalizeNewlines(result));
        }

        [Fact]
        public void SimpleFormatArguments()
        {
            var args = new List<LispObject>()
            {
                new LispSymbol("a"),
                new LispList(new LispSymbol("b")),
                new LispInteger(5),
            };
            Assert.True(LispFormatter.TryFormatString("A: ~S, B: ~S, C: ~S", args, out var result), $"Error from formatter: {result}");
            Assert.Equal("A: a, B: (b), C: 5", NormalizeNewlines(result));
        }

        [Fact]
        public void FormatStringValues()
        {
            var args = new LispObject[] { new LispString("a") };
            string result;
            Assert.True(LispFormatter.TryFormatString("[ ~S ]", args, out result), $"Error from formatter: {result}");
            Assert.Equal("[ \"a\" ]", result);
            Assert.True(LispFormatter.TryFormatString("[ ~A ]", args, out result), $"Error from formatter: {result}");
            Assert.Equal("[ a ]", result);
        }
    }
}
