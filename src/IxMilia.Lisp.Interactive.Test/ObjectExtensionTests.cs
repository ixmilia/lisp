using IxMilia.Lisp.Test;
using Xunit;

namespace IxMilia.Lisp.Interactive.Test
{
    public class ObjectExtensionTests : TestBase
    {
        [Fact]
        public void ListIsFormattedAsJsonArray()
        {
            var list = LispList.FromItems(
                new LispInteger(1),
                new LispFloat(2.5),
                new LispRatio(9, 2),
                new LispComplexNumber(new LispInteger(2), new LispRatio(1, 2)),
                new LispCharacter('a'),
                new LispString("abc"),
                LispSymbol.CreateFromString("MY-SYMBOL"),
                LispVector.CreateFixed(new LispObject[]
                {
                    new LispInteger(2),
                    new LispString("def")
                }),
                new LispError("some error"),
                new LispLambdaListKeyword("MY-KEYWORD")
            );
            var actual = list.ToJsonString();
            var expected = @"[1,2.5,4.5,[2,0.5],""a"",""abc"",""MY-SYMBOL"",[2,""def""],{""MESSAGE"":""some error""},""MY-KEYWORD""]";
            Assert.Equal(expected, actual);
        }
    }
}
