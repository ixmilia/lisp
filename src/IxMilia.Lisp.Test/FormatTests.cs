using Xunit;

namespace IxMilia.Lisp.Test
{
    public class FormatTests : TestBase
    {
        [Fact]
        public void NewlineDirectives()
        {
            Assert.True(LispFormatter.TryFormatString("a~%b", out var result), $"Error from formatter: {result}");
            Assert.Equal("a\r\nb", NormalizeNewlines(result));
        }

        [Fact]
        public void EnsureLineStartDirective()
        {
            Assert.True(LispFormatter.TryFormatString("a~&b~&~&~&c", out var result), $"Error from formatter: {result}");
            Assert.Equal("a\r\nb\r\nc", NormalizeNewlines(result));
        }
    }
}
