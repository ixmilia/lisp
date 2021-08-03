using System;
using System.Linq;
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

        [Fact]
        public void IsNil()
        {
            Assert.True(LispNilList.Instance.IsNil());
            Assert.False(new LispQuotedObject(new LispSymbol("a")).IsNil());
            Assert.False(new LispQuotedObject(LispNilList.Instance).IsNil());
            Assert.False(new LispError("e").IsNil());
            Assert.False(new LispSymbol("a").IsNil());
            Assert.False(new LispKeyword("a").IsNil());
            Assert.False(new LispInteger(0).IsNil());
            Assert.False(new LispFloat(0.0).IsNil());
            Assert.False(new LispRatio(1, 2).IsNil());
            Assert.False(new LispString("a").IsNil());
            Assert.False(LispList.FromItems(new LispInteger(0)).IsNil());
            Assert.False(new LispCodeFunction("function", null, new LispArgumentCollection(Array.Empty<LispRegularFunctionArgument>(), Array.Empty<LispOptionalFunctionArgument>(), Array.Empty<LispKeywordFunctionArgument>(), null), Enumerable.Empty<LispObject>()).IsNil());
            Assert.False(new LispNativeFunction("function", null, null).IsNil());
            Assert.False(new LispQuotedNamedFunctionReference("function").IsNil());
            Assert.False(new LispQuotedLambdaFunctionReference(new LispCodeFunction("function", null, new LispArgumentCollection(Array.Empty<LispRegularFunctionArgument>(), Array.Empty<LispOptionalFunctionArgument>(), Array.Empty<LispKeywordFunctionArgument>(), null), Enumerable.Empty<LispObject>())).IsNil());
            Assert.False(new LispCodeMacro("function", Enumerable.Empty<string>(), Enumerable.Empty<LispObject>()).IsNil());
            Assert.False(new LispNativeMacro("function", null).IsNil());
        }

        [Fact]
        public void IsTLike()
        {
            Assert.False(LispNilList.Instance.IsTLike());
            Assert.True(new LispQuotedObject(new LispSymbol("a")).IsTLike());
            Assert.True(new LispQuotedObject(LispNilList.Instance).IsTLike());
            Assert.True(new LispError("e").IsTLike());
            Assert.True(new LispSymbol("a").IsTLike());
            Assert.True(new LispKeyword("a").IsTLike());
            Assert.True(new LispInteger(0).IsTLike());
            Assert.True(new LispFloat(0.0).IsTLike());
            Assert.True(new LispRatio(1, 2).IsTLike());
            Assert.True(new LispString("a").IsTLike());
            Assert.True(LispList.FromItems(new LispInteger(0)).IsTLike());
            Assert.True(new LispCodeFunction("function", null, new LispArgumentCollection(Array.Empty<LispRegularFunctionArgument>(), Array.Empty<LispOptionalFunctionArgument>(), Array.Empty<LispKeywordFunctionArgument>(), null), Enumerable.Empty<LispObject>()).IsTLike());
            Assert.True(new LispNativeFunction("function", null, null).IsTLike());
            Assert.True(new LispQuotedNamedFunctionReference("function").IsTLike());
            Assert.True(new LispQuotedLambdaFunctionReference(new LispCodeFunction("function", null, new LispArgumentCollection(Array.Empty<LispRegularFunctionArgument>(), Array.Empty<LispOptionalFunctionArgument>(), Array.Empty<LispKeywordFunctionArgument>(), null), Enumerable.Empty<LispObject>())).IsTLike());
            Assert.True(new LispCodeMacro("function", Enumerable.Empty<string>(), Enumerable.Empty<LispObject>()).IsTLike());
            Assert.True(new LispNativeMacro("function", null).IsTLike());
        }
    }
}
