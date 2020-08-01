using Xunit;

namespace IxMilia.Lisp.Test
{
    public class ObjectTests
    {
        [Fact]
        public void ListToString()
        {
            Assert.Equal("(1 2)", new LispList(new LispFloat(1), new LispList(new LispFloat(2), LispNilList.Instance)).ToString());
            Assert.Equal("(1 . 2)", new LispList(new LispFloat(1), new LispFloat(2)).ToString());
        }

        [Fact]
        public void ImproperLists()
        {
            Assert.True(LispNilList.Instance.IsProperList);
            Assert.True(new LispList(new LispFloat(1), new LispList(new LispFloat(2), LispNilList.Instance)).IsProperList);
            Assert.False(new LispList(new LispFloat(1), new LispFloat(2)).IsProperList);
            Assert.True(LispList.FromItems().IsProperList);
            Assert.True(LispList.FromItems(new LispFloat(1), new LispFloat(2)).IsProperList);
        }

        [Fact]
        public void RatioReductions()
        {
            Assert.Equal(new LispInteger(0), new LispRatio(0, 1).Reduce());
            Assert.Equal(new LispInteger(2), new LispRatio(2, 1).Reduce());
            Assert.Equal(new LispInteger(2), new LispRatio(4, 2).Reduce());
            Assert.Equal(new LispInteger(1), new LispRatio(2, 2).Reduce());
            Assert.Equal(new LispRatio(5, 4), new LispRatio(10, 8).Reduce());
        }

        [Fact]
        public void RatioArithmetic()
        {
            Assert.Equal(new LispRatio(2, 3), new LispRatio(1, 3) + new LispRatio(1, 3));
            Assert.Equal(new LispRatio(1, 3), new LispRatio(2, 3) - new LispRatio(1, 3));
            Assert.Equal(new LispRatio(1, 9), new LispRatio(1, 3) * new LispRatio(1, 3));
            Assert.Equal(new LispRatio(1, 2), new LispRatio(1, 3) / new LispRatio(2, 3));
            Assert.Equal(new LispInteger(1), new LispRatio(1, 2) + new LispRatio(1, 2));
            Assert.Equal(new LispRatio(2, 3), new LispInteger(1) - new LispRatio(1, 3));
        }
    }
}
