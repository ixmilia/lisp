using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class FormatTests : TestBase
    {
        [Fact]
        public void NoArgumentTokens()
        {
            var formatString = "a~%bc~&~Ade~Sf";
            var tokens = LispFormatter.GetFormatTokens(formatString).ToList();
            Assert.Equal(8, tokens.Count);

            Assert.Equal("a", tokens[0].GetString(formatString));
            Assert.IsType<LispRawFormatToken>(tokens[0]);

            Assert.Equal("~%", tokens[1].GetString(formatString));
            Assert.IsType<LispEscapeSequenceFormatToken>(tokens[1]);
            Assert.Equal('%', ((LispEscapeSequenceFormatToken)tokens[1]).EscapeCode);
            Assert.Equal("", ((LispEscapeSequenceFormatToken)tokens[1]).GetArgument(formatString));

            Assert.Equal("bc", tokens[2].GetString(formatString));
            Assert.IsType<LispRawFormatToken>(tokens[2]);

            Assert.Equal("~&", tokens[3].GetString(formatString));
            Assert.IsType<LispEscapeSequenceFormatToken>(tokens[3]);
            Assert.Equal('&', ((LispEscapeSequenceFormatToken)tokens[3]).EscapeCode);
            Assert.Equal("", ((LispEscapeSequenceFormatToken)tokens[3]).GetArgument(formatString));

            Assert.Equal("~A", tokens[4].GetString(formatString));
            Assert.IsType<LispEscapeSequenceFormatToken>(tokens[4]);
            Assert.Equal('A', ((LispEscapeSequenceFormatToken)tokens[4]).EscapeCode);
            Assert.Equal("", ((LispEscapeSequenceFormatToken)tokens[4]).GetArgument(formatString));

            Assert.Equal("de", tokens[5].GetString(formatString));
            Assert.IsType<LispRawFormatToken>(tokens[5]);

            Assert.Equal("~S", tokens[6].GetString(formatString));
            Assert.IsType<LispEscapeSequenceFormatToken>(tokens[6]);
            Assert.Equal('S', ((LispEscapeSequenceFormatToken)tokens[6]).EscapeCode);
            Assert.Equal("", ((LispEscapeSequenceFormatToken)tokens[6]).GetArgument(formatString));

            Assert.Equal("f", tokens[7].GetString(formatString));
            Assert.IsType<LispRawFormatToken>(tokens[7]);
        }

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

        [Fact]
        public void FormatWithWidth()
        {
            var args = new LispObject[] { new LispString("abc"), new LispInteger(4) };
            string result;
            Assert.True(LispFormatter.TryFormatString("~10S.~S", args, out result), $"Error from formatter: {result}");
            Assert.Equal("\"abc\"     .4", result);
            Assert.True(LispFormatter.TryFormatString("~2S.~S", args, out result), $"Error from formatter: {result}");
            Assert.Equal("\"abc\".4", result);
        }
    }
}
