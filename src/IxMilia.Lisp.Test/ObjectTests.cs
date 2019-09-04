using Xunit;

namespace IxMilia.Lisp.Test
{
    public class ObjectTests
    {
        [Fact]
        public void ListToString()
        {
            Assert.Equal("(1 2)", new LispList(new LispNumber(1), new LispList(new LispNumber(2), LispNilList.Instance)).ToString());
            Assert.Equal("(1 . 2)", new LispList(new LispNumber(1), new LispNumber(2)).ToString());
        }

        [Fact]
        public void ImproperLists()
        {
            Assert.True(LispNilList.Instance.IsProperList);
            Assert.True(new LispList(new LispNumber(1), new LispList(new LispNumber(2), LispNilList.Instance)).IsProperList);
            Assert.False(new LispList(new LispNumber(1), new LispNumber(2)).IsProperList);
            Assert.True(LispList.FromItems().IsProperList);
            Assert.True(LispList.FromItems(new LispNumber(1), new LispNumber(2)).IsProperList);
        }
    }
}
