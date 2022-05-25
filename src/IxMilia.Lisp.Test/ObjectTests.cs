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
            Assert.Equal(new LispRatio(-1, 2), new LispRatio(2, -4).Reduce());
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
            Assert.False(LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:QUOTE"), LispSymbol.CreateFromString("A")).IsNil());
            Assert.False(LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:QUOTE"), LispNilList.Instance).IsNil());
            Assert.False(new LispError("e").IsNil());
            Assert.False(LispSymbol.CreateFromString("a").IsNil());
            Assert.False(new LispInteger(0).IsNil());
            Assert.False(new LispFloat(0.0).IsNil());
            Assert.False(new LispRatio(1, 2).IsNil());
            Assert.False(new LispString("a").IsNil());
            Assert.False(LispList.FromItems(new LispInteger(0)).IsNil());
            Assert.False(new LispCodeFunction(new LispResolvedSymbol("some-module", "function", true), null, LispArgumentCollection.Empty, Enumerable.Empty<LispObject>()).IsNil());
            Assert.False(new LispNativeFunction(new LispResolvedSymbol("some-module", "function", true), null, null).IsNil());
            Assert.False(new LispQuotedNamedFunctionReference("function").IsNil());
            Assert.False(new LispQuotedLambdaFunctionReference(new LispCodeFunction(new LispResolvedSymbol("some-module", "function", true), null, new LispArgumentCollection(Array.Empty<LispRegularInvocationArgument>(), Array.Empty<LispOptionalInvocationArgument>(), Array.Empty<LispKeywordInvocationArgument>(), Array.Empty<LispAuxiliaryInvocationArgument>(), null), Enumerable.Empty<LispObject>())).IsNil());
            Assert.False(new LispCodeMacro(new LispResolvedSymbol("some-module", "function", true), null, LispArgumentCollection.Empty, Enumerable.Empty<LispObject>()).IsNil());
            Assert.False(new LispNativeMacro(new LispResolvedSymbol("some-module", "function", true), null, null).IsNil());
        }

        [Fact]
        public void IsTLike()
        {
            Assert.False(LispNilList.Instance.IsTLike());
            Assert.True(LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:QUOTE"), LispSymbol.CreateFromString("A")).IsTLike());
            Assert.True(LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:QUOTE"), LispNilList.Instance).IsTLike());
            Assert.True(new LispError("e").IsTLike());
            Assert.True(LispSymbol.CreateFromString("a").IsTLike());
            Assert.True(new LispInteger(0).IsTLike());
            Assert.True(new LispFloat(0.0).IsTLike());
            Assert.True(new LispRatio(1, 2).IsTLike());
            Assert.True(new LispString("a").IsTLike());
            Assert.True(LispList.FromItems(new LispInteger(0)).IsTLike());
            Assert.True(new LispCodeFunction(new LispResolvedSymbol("some-module", "function", true), null, LispArgumentCollection.Empty, Enumerable.Empty<LispObject>()).IsTLike());
            Assert.True(new LispNativeFunction(new LispResolvedSymbol("some-module", "function", true), null, null).IsTLike());
            Assert.True(new LispQuotedNamedFunctionReference("function").IsTLike());
            Assert.True(new LispQuotedLambdaFunctionReference(new LispCodeFunction(new LispResolvedSymbol("some-module", "function", true), null, new LispArgumentCollection(Array.Empty<LispRegularInvocationArgument>(), Array.Empty<LispOptionalInvocationArgument>(), Array.Empty<LispKeywordInvocationArgument>(), Array.Empty<LispAuxiliaryInvocationArgument>(), null), Enumerable.Empty<LispObject>())).IsTLike());
            Assert.True(new LispCodeMacro(new LispResolvedSymbol("some-module", "function", true), null, LispArgumentCollection.Empty, Enumerable.Empty<LispObject>()).IsTLike());
            Assert.True(new LispNativeMacro(new LispResolvedSymbol("some-module", "function", true), null, null).IsTLike());
        }

        [Fact]
        public void ComplexMath()
        {
            Assert.Equal(new LispComplexNumber(new LispInteger(4), new LispInteger(6)), LispNumber.Add(new LispComplexNumber(new LispInteger(1), new LispInteger(2)), new LispComplexNumber(new LispInteger(3), new LispInteger(4))));
            Assert.Equal(new LispComplexNumber(new LispInteger(-2), new LispInteger(-2)), LispNumber.Sub(new LispComplexNumber(new LispInteger(1), new LispInteger(2)), new LispComplexNumber(new LispInteger(3), new LispInteger(4))));
            Assert.Equal(new LispComplexNumber(new LispInteger(-5), new LispInteger(10)), LispNumber.Mul(new LispComplexNumber(new LispInteger(1), new LispInteger(2)), new LispComplexNumber(new LispInteger(3), new LispInteger(4))));
            Assert.Equal(new LispComplexNumber(new LispInteger(-1), new LispInteger(-1)), LispNumber.Div(new LispComplexNumber(new LispInteger(1), new LispInteger(-3)), new LispComplexNumber(new LispInteger(1), new LispInteger(2))));
        }

        [Fact]
        public void PackageAndSymbolNameSplitting()
        {
            Assert.Equal(Tuple.Create((string)null, "SOME-SYMBOL", true), LispSymbol.SplitPackageAndSymbolName("SOME-SYMBOL"));
            Assert.Equal(Tuple.Create("SOME-PACKAGE", "SOME-SYMBOL", true), LispSymbol.SplitPackageAndSymbolName("SOME-PACKAGE:SOME-SYMBOL"));
            Assert.Equal(Tuple.Create("SOME-PACKAGE", "SOME-SYMBOL", false), LispSymbol.SplitPackageAndSymbolName("SOME-PACKAGE::SOME-SYMBOL"));
            Assert.Equal(Tuple.Create("KEYWORD", "SOME-SYMBOL", true), LispSymbol.SplitPackageAndSymbolName(":SOME-SYMBOL"));
            Assert.Equal(Tuple.Create("KEYWORD", "SOME-SYMBOL", false), LispSymbol.SplitPackageAndSymbolName("::SOME-SYMBOL"));
        }

        [Theory]
        [InlineData("#(1 2 3)", false, 3, 3, "#(1 2 3)")]
        [InlineData("(vector 1 2 3)", false, 3, 3, "#(1 2 3)")]
        [InlineData("(make-array 3)", false, 3, 3, "#(() () ())")]
        [InlineData("(make-array '(3))", false, 3, 3, "#(() () ())")]
        [InlineData("(make-array 3 :initial-element 42)", false, 3, 3, "#(42 42 42)")]
        [InlineData("(make-array 3 :fill-pointer 1)", false, 1, 1, "#(())")]
        [InlineData("(make-array 3 :fill-pointer 1 :adjustable t)", true, 1, 3, "#(())")]
        public void VectorProperties(string code, bool isAdjustable, int count, int size, string display)
        {
            var host = new LispHost();
            var evalResult = host.Eval(code);
            Assert.Null(evalResult.ReadError);
            Assert.IsType<LispVector>(evalResult.LastResult);
            var vector = (LispVector)evalResult.LastResult;
            Assert.Equal(isAdjustable, vector.IsAdjustable);
            Assert.Equal(count, vector.Count);
            Assert.Equal(size, vector.Size);
            Assert.Equal(display, vector.ToString());
        }

        [Theory]
        [InlineData("(elt (vector 1 2 3) 1)", "2")] // get vector
        [InlineData("(elt '(1 2 3) 1)", "2")] // get list
        [InlineData("(setf v (vector 1 2 3))\n(setf (elt v 1) 42)\nv", "#(1 42 3)")] // set vector
        [InlineData("(setf l '(1 2 3))\n(setf (elt l 1) 42)\nl", "(1 42 3)")] // set list
        public void SequenceElement(string code, string expected)
        {
            var host = new LispHost();
            var evalResult = host.Eval(code);
            Assert.Null(evalResult.ReadError);
            Assert.Equal(expected, evalResult.LastResult.ToString());
        }
    }
}
