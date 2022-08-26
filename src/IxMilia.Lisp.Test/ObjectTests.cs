using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
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
            Assert.Equal(new LispInteger(0), new LispRatio(0, 1).Simplify());
            Assert.Equal(new LispInteger(2), new LispRatio(2, 1).Simplify());
            Assert.Equal(new LispInteger(2), new LispRatio(4, 2).Simplify());
            Assert.Equal(new LispInteger(1), new LispRatio(2, 2).Simplify());
            Assert.Equal(new LispRatio(5, 4), new LispRatio(10, 8).Simplify());
            Assert.Equal(new LispRatio(-1, 2), new LispRatio(2, -4).Simplify());
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
        public void SimplifyNumber()
        {
            Assert.Equal(new LispInteger(2), new LispRatio(2, 1).Simplify());
            Assert.Equal(new LispInteger(2), new LispRatio(4, 2).Simplify());
            Assert.Equal(new LispInteger(0), new LispRatio(0, 1).Simplify());
            Assert.Equal(new LispInteger(2), new LispComplexNumber(new LispRatio(6, 3), new LispRatio(0, 2)).Simplify());
        }

        [Fact]
        public void NumberExponents()
        {
            var sqrt2 = Math.Sqrt(2.0);
            Assert.Equal(new LispInteger(8), LispNumber.Exponent(new LispInteger(2), new LispInteger(3))); // int/int
            Assert.Equal(new LispRatio(1, 8), LispNumber.Exponent(new LispInteger(2), new LispInteger(-3)));
            Assert.Equal(new LispInteger(1), LispNumber.Exponent(new LispInteger(2), new LispInteger(0)));
            Assert.Equal(new LispFloat(8.0), LispNumber.Exponent(new LispInteger(2), new LispFloat(3.0))); // int/float
            Assert.Equal(new LispFloat(1.0), LispNumber.Exponent(new LispInteger(2), new LispFloat(0.0)));
            Assert.Equal(new LispInteger(2), LispNumber.Exponent(new LispInteger(4), new LispRatio(1, 2))); // int/ratio
            Assert.Equal(new LispFloat(sqrt2), LispNumber.Exponent(new LispInteger(2), new LispRatio(1, 2)));
            Assert.Equal(new LispInteger(8), LispNumber.Exponent(new LispInteger(2), new LispComplexNumber(new LispInteger(3), new LispInteger(0)))); // int/complex
            Assert.Equal(new LispComplexNumber(new LispFloat(6.153911210911777), new LispFloat(5.111690210509078)), LispNumber.Exponent(new LispInteger(2), new LispComplexNumber(new LispInteger(3), new LispInteger(1))));
            Assert.Equal(new LispFloat(8.0), LispNumber.Exponent(new LispFloat(2.0), new LispInteger(3))); // float/int
            Assert.Equal(new LispFloat(8.0), LispNumber.Exponent(new LispFloat(2.0), new LispFloat(3.0))); // float/float
            Assert.Equal(new LispFloat(sqrt2), LispNumber.Exponent(new LispFloat(2.0), new LispRatio(1, 2))); // float/ratio
            Assert.Equal(new LispFloat(8.0), LispNumber.Exponent(new LispFloat(2.0), new LispComplexNumber(new LispInteger(3), new LispInteger(0)))); // float/complex
            Assert.Equal(new LispComplexNumber(new LispFloat(6.153911210911777), new LispFloat(5.111690210509078)), LispNumber.Exponent(new LispFloat(2.0), new LispComplexNumber(new LispInteger(3), new LispInteger(1))));
            Assert.Equal(new LispRatio(1, 8), LispNumber.Exponent(new LispRatio(1, 2), new LispInteger(3))); // ratio/int
            Assert.Equal(new LispFloat(0.125), LispNumber.Exponent(new LispRatio(1, 2), new LispFloat(3.0))); // ratio/float
            Assert.Equal(new LispRatio(1, 4), LispNumber.Exponent(new LispRatio(1, 2), new LispRatio(4, 2))); // ratio/ratio
            Assert.Equal(new LispFloat(0.8408964152537145), LispNumber.Exponent(new LispRatio(1, 2), new LispRatio(1, 4)));
            Assert.Equal(new LispRatio(1, 4), LispNumber.Exponent(new LispRatio(1, 2), new LispComplexNumber(new LispInteger(2), new LispInteger(0)))); // ratio/complex
            Assert.Equal(new LispComplexNumber(new LispFloat(0.19230972534099303), new LispFloat(-0.1597403190784087)), LispNumber.Exponent(new LispRatio(1, 2), new LispComplexNumber(new LispInteger(2), new LispInteger(1))));
            Assert.Equal(new LispComplexNumber(new LispInteger(3), new LispInteger(4)), LispNumber.Exponent(new LispComplexNumber(new LispInteger(2), new LispInteger(1)), new LispInteger(2))); // complex/int
            Assert.Equal(new LispComplexNumber(new LispFloat(3.000000000000001), new LispFloat(4.0)), LispNumber.Exponent(new LispComplexNumber(new LispInteger(2), new LispInteger(1)), new LispFloat(2.0))); // complex/float
            Assert.Equal(new LispComplexNumber(new LispFloat(1.455346690225355), new LispFloat(0.34356074972251244)), LispNumber.Exponent(new LispComplexNumber(new LispInteger(2), new LispInteger(1)), new LispRatio(1, 2))); // complex/ratio
            Assert.Equal(new LispComplexNumber(new LispFloat(-0.42589434775182566), new LispFloat(0.7753703444218832)), LispNumber.Exponent(new LispComplexNumber(new LispInteger(2), new LispInteger(1)), new LispComplexNumber(new LispInteger(1), new LispInteger(2)))); // complex/complex
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
            //Assert.False(new LispCodeFunction(new LispResolvedSymbol("some-module", "function", true), null, LispArgumentCollection.Empty, Enumerable.Empty<LispObject>()).IsNil());
            Assert.False(new LispNativeFunction(new LispResolvedSymbol("some-module", "function", true), null, null, null).IsNil());
            //Assert.False(new LispQuotedNamedFunctionReference("function").IsNil());
            //Assert.False(new LispQuotedLambdaFunctionReference(new LispCodeFunction(new LispResolvedSymbol("some-module", "function", true), null, new LispArgumentCollection(Array.Empty<LispRegularInvocationArgument>(), Array.Empty<LispOptionalInvocationArgument>(), Array.Empty<LispKeywordInvocationArgument>(), Array.Empty<LispAuxiliaryInvocationArgument>(), null), Enumerable.Empty<LispObject>())).IsNil());
            Assert.False(new LispCodeMacro(new LispResolvedSymbol("some-module", "function", true), null, LispArgumentCollection.Empty, Enumerable.Empty<LispObject>()).IsNil());
            Assert.False(new LispNativeMacro(new LispResolvedSymbol("some-module", "function", true), null, null, null).IsNil());
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
            //Assert.True(new LispCodeFunction(new LispResolvedSymbol("some-module", "function", true), null, LispArgumentCollection.Empty, Enumerable.Empty<LispObject>()).IsTLike());
            Assert.True(new LispNativeFunction(new LispResolvedSymbol("some-module", "function", true), null, null, null).IsTLike());
            //Assert.True(new LispQuotedNamedFunctionReference("function").IsTLike());
            //Assert.True(new LispQuotedLambdaFunctionReference(new LispCodeFunction(new LispResolvedSymbol("some-module", "function", true), null, new LispArgumentCollection(Array.Empty<LispRegularInvocationArgument>(), Array.Empty<LispOptionalInvocationArgument>(), Array.Empty<LispKeywordInvocationArgument>(), Array.Empty<LispAuxiliaryInvocationArgument>(), null), Enumerable.Empty<LispObject>())).IsTLike());
            Assert.True(new LispCodeMacro(new LispResolvedSymbol("some-module", "function", true), null, LispArgumentCollection.Empty, Enumerable.Empty<LispObject>()).IsTLike());
            Assert.True(new LispNativeMacro(new LispResolvedSymbol("some-module", "function", true), null, null, null).IsTLike());
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
        public async Task VectorProperties(string code, bool isAdjustable, int count, int size, string display)
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(code);
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
        public async Task SequenceElement(string code, string expected)
        {
            var host = await LispHost.CreateAsync();
            var evalResult = await host.EvalAsync(code);
            Assert.Null(evalResult.ReadError);
            Assert.Equal(expected, evalResult.LastResult.ToString());
        }

        [Theory]
        [InlineData("(+ 1 2)", null)]
        [InlineData("(IF (= (+ 1 2) (- 5 2)) 1 2)", 2)]
        [InlineData("(QUOTE (1))", 5)]
        [InlineData("(QUOTE (2))", null)]
        [InlineData("(SOME-FUNCTION)", 6)]
        public async Task ObjectBreakpointLine(string expression, int? expectedBreakpointLine)
        {
            var filePath = "test-file.lisp";
            int? reportedBreakpointLine = null;
            var foundExpression = false;
            var host = await LispHost.CreateAsync(filePath: filePath);
            var code = @"
(defun some-function ()         ; line 1
    (if (= (+ 1 2) (- 5 2))     ; line 2
        1                       ; line 3
        2)                      ; line 4
    '(1) '(2) '(3))             ; line 5
(some-function)                 ; line 6
".Trim();
            var seenExpressions = new List<string>();
            host.RootFrame.EvaluatingExpression += (_s, e) =>
            {
                if (e.Expression.SourceLocation.HasValue &&
                    e.Expression.SourceLocation.Value.FilePath == filePath)
                {
                    var expressionString = e.Expression.ToString();
                    seenExpressions.Add(expressionString);
                    if (expressionString == expression)
                    {
                        foundExpression = true;
                        reportedBreakpointLine = e.Expression.GetBreakpointLine();
                    }
                }
            };
            var evalResult = await host.EvalAsync(code);
            Assert.True(evalResult.ExecutionState.IsExecutionComplete);
            Assert.True(foundExpression, $"Did not see expression [{expression}] in\n\t{string.Join("\n\t", seenExpressions)}");
            Assert.Equal(expectedBreakpointLine, reportedBreakpointLine);
        }
    }
}
