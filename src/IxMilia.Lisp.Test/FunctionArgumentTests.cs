using System;
using System.Collections.Generic;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class FunctionArgumentTests : TestBase
    {
        private static LispArgumentCollection GetArgumentCollection(params LispObject[] argumentTypeValues)
        {
            Assert.True(LispArgumentCollection.TryBuildArgumentCollection(argumentTypeValues, out var argumentCollection, out var error), $"Error when building arguments: [{error?.Message}]");
            return argumentCollection;
        }

        private static Tuple<LispFunctionArgument, LispObject>[] MatchArguments(IEnumerable<LispRegularFunctionArgument> regularArguments, IEnumerable<LispOptionalFunctionArgument> optionalArguments, IEnumerable<LispKeywordFunctionArgument> keywordArguments, IEnumerable<LispAuxiliaryFunctionArgument> auxiliaryArguments, LispRestFunctionArgument restArgument, LispObject[] argumentValues)
        {
            var argumentCollection = new LispArgumentCollection(regularArguments, optionalArguments, keywordArguments, auxiliaryArguments, restArgument);
            Assert.True(LispCodeFunction.TryMatchFunctionArguments(argumentCollection, argumentValues, out var matchedArguments, out var error), $"Error when matching arguments: [{error?.Message}]");
            return matchedArguments;
        }

        [Fact]
        public void BindEmptyFunctionArguments()
        {
            var argumentCollection = GetArgumentCollection();
            Assert.Empty(argumentCollection.RegularArguments);
            Assert.Empty(argumentCollection.OptionalArguments);
            Assert.Empty(argumentCollection.KeywordArguments);
            Assert.Empty(argumentCollection.AuxiliaryArguments);
            Assert.Null(argumentCollection.RestArgument);
        }

        [Fact]
        public void BindBuildRegularFunctionArguments()
        {
            // a b c
            var argumentCollection = GetArgumentCollection(
                new LispSymbol("a"),
                new LispSymbol("b"),
                new LispSymbol("c")
            );
            Assert.Equal(3, argumentCollection.RegularArguments.Count);
            Assert.Empty(argumentCollection.OptionalArguments);
            Assert.Empty(argumentCollection.KeywordArguments);
            Assert.Empty(argumentCollection.AuxiliaryArguments);
            Assert.Null(argumentCollection.RestArgument);

            Assert.Equal("a", argumentCollection.RegularArguments[0].Name);
            Assert.Equal("b", argumentCollection.RegularArguments[1].Name);
            Assert.Equal("c", argumentCollection.RegularArguments[2].Name);
        }

        [Fact]
        public void BindOptionalArguments()
        {
            // a &optional (should-be-fourteen 14) should-be-nil
            var argumentCollection = GetArgumentCollection(
                new LispSymbol("a"),
                new LispLambdaListKeyword("&optional"),
                LispList.FromItems(
                    new LispSymbol("should-be-fourteen"),
                    new LispInteger(14)),
                new LispSymbol("should-be-nil")
            );
            Assert.Equal(1, argumentCollection.RegularArguments.Count);
            Assert.Equal(2, argumentCollection.OptionalArguments.Count);
            Assert.Empty(argumentCollection.KeywordArguments);
            Assert.Empty(argumentCollection.AuxiliaryArguments);
            Assert.Null(argumentCollection.RestArgument);

            Assert.Equal("a", argumentCollection.RegularArguments[0].Name);
            Assert.Equal("should-be-fourteen", argumentCollection.OptionalArguments[0].Name);
            Assert.Equal(14, ((LispInteger)argumentCollection.OptionalArguments[0].DefaultValue).Value);
            Assert.Equal("should-be-nil", argumentCollection.OptionalArguments[1].Name);
            Assert.True(argumentCollection.OptionalArguments[1].DefaultValue.IsNil());
        }

        [Fact]
        public void BindKeywordArguments()
        {
            // a &key (should-be-fourteen 14) should-be-nil
            var argumentCollection = GetArgumentCollection(
                new LispSymbol("a"),
                new LispLambdaListKeyword("&key"),
                LispList.FromItems(
                    new LispSymbol("should-be-fourteen"),
                    new LispInteger(14)),
                new LispSymbol("should-be-nil")
            );
            Assert.Equal(1, argumentCollection.RegularArguments.Count);
            Assert.Empty(argumentCollection.OptionalArguments);
            Assert.Equal(2, argumentCollection.KeywordArguments.Count);
            Assert.Empty(argumentCollection.AuxiliaryArguments);
            Assert.Null(argumentCollection.RestArgument);

            Assert.Equal("a", argumentCollection.RegularArguments[0].Name);
            Assert.Equal(14, ((LispInteger)argumentCollection.KeywordArguments["should-be-fourteen"].DefaultValue).Value);
            Assert.True(argumentCollection.KeywordArguments["should-be-nil"].DefaultValue.IsNil());
        }

        [Fact]
        public void BindAuxiliaryArguments()
        {
            // a &aux (two 2) nniill
            var argumentCollection = GetArgumentCollection(
                new LispSymbol("a"),
                new LispLambdaListKeyword("&aux"),
                LispList.FromItems(
                    new LispSymbol("two"),
                    new LispInteger(2)),
                new LispSymbol("nniill")
            );
            Assert.Equal(1, argumentCollection.RegularArguments.Count);
            Assert.Empty(argumentCollection.OptionalArguments);
            Assert.Empty(argumentCollection.KeywordArguments);
            Assert.Equal(2, argumentCollection.AuxiliaryArguments.Count);
            Assert.Null(argumentCollection.RestArgument);

            Assert.Equal("a", argumentCollection.RegularArguments[0].Name);
            Assert.Equal("two", argumentCollection.AuxiliaryArguments[0].Name);
            Assert.Equal(2, ((LispInteger)argumentCollection.AuxiliaryArguments[0].InitialValue).Value);
            Assert.Equal("nniill", argumentCollection.AuxiliaryArguments[1].Name);
            Assert.True(argumentCollection.AuxiliaryArguments[1].InitialValue.IsNil());
        }

        [Fact]
        public void BindRestArgument()
        {
            // a &rest the-rest
            var argumentCollection = GetArgumentCollection(
                new LispSymbol("a"),
                new LispLambdaListKeyword("&rest"),
                new LispSymbol("the-rest")
            );
            Assert.Equal(1, argumentCollection.RegularArguments.Count);
            Assert.Empty(argumentCollection.OptionalArguments);
            Assert.Empty(argumentCollection.KeywordArguments);
            Assert.NotNull(argumentCollection.RestArgument);

            Assert.Equal("a", argumentCollection.RegularArguments[0].Name);
            Assert.Equal("the-rest", argumentCollection.RestArgument.Name);
        }

        [Fact]
        public void BindOptionalAndKeywordAndRestArguments()
        {
            // a &optional should-be-nil &key some-special-value &rest the-rest
            var argumentCollection = GetArgumentCollection(
                new LispSymbol("a"),
                new LispLambdaListKeyword("&optional"),
                new LispSymbol("should-be-nil"),
                new LispLambdaListKeyword("&key"),
                new LispSymbol("some-special-value"),
                new LispLambdaListKeyword("&rest"),
                new LispSymbol("the-rest")
            );
            Assert.Equal(1, argumentCollection.RegularArguments.Count);
            Assert.Equal(1, argumentCollection.OptionalArguments.Count);
            Assert.Equal(1, argumentCollection.KeywordArguments.Count);
            Assert.NotNull(argumentCollection.RestArgument);

            Assert.Equal("a", argumentCollection.RegularArguments[0].Name);
            Assert.Equal("should-be-nil", argumentCollection.OptionalArguments[0].Name);
            Assert.True(argumentCollection.KeywordArguments.ContainsKey("some-special-value"));
            Assert.Equal("the-rest", argumentCollection.RestArgument.Name);
        }

        [Fact]
        public void MatchEmptyFunctionArguments()
        {
            var matched = MatchArguments(Array.Empty<LispRegularFunctionArgument>(), Array.Empty<LispOptionalFunctionArgument>(), Array.Empty<LispKeywordFunctionArgument>(), Array.Empty<LispAuxiliaryFunctionArgument>(), null, Array.Empty<LispObject>());
            Assert.Empty(matched);
        }

        [Fact]
        public void MatchRegularFunctionArguments()
        {
            // a b c
            // 1 "two" 3.0
            var matched = MatchArguments(
                new[]
                {
                    new LispRegularFunctionArgument(new LispSymbol("a")),
                    new LispRegularFunctionArgument(new LispSymbol("b")),
                    new LispRegularFunctionArgument(new LispSymbol("c")),
                },
                Array.Empty<LispOptionalFunctionArgument>(),
                Array.Empty<LispKeywordFunctionArgument>(),
                Array.Empty<LispAuxiliaryFunctionArgument>(),
                null,
                new LispObject[]
                {
                    new LispInteger(1),
                    new LispString("two"),
                    new LispFloat(3.0),
                }
            );
            Assert.Equal(3, matched.Length);

            Assert.Equal("a", matched[0].Item1.Name);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("b", matched[1].Item1.Name);
            Assert.Equal("two", ((LispString)matched[1].Item2).Value);

            Assert.Equal("c", matched[2].Item1.Name);
            Assert.Equal(3.0, ((LispFloat)matched[2].Item2).Value);
        }

        [Fact]
        public void MatchOptionalFunctionArguments()
        {
            // a &optional (defaults-to-fourteen 14) defaults-to-nil
            // 1 2
            var matched = MatchArguments(
                new[]
                {
                    new LispRegularFunctionArgument(new LispSymbol("a")),
                },
                new[]
                {
                    new LispOptionalFunctionArgument(new LispSymbol("defaults-to-fourteen"), new LispInteger(14)),
                    new LispOptionalFunctionArgument(new LispSymbol("defaults-to-nil"), LispNilList.Instance),
                },
                Array.Empty<LispKeywordFunctionArgument>(),
                Array.Empty<LispAuxiliaryFunctionArgument>(),
                null,
                new LispObject[]
                {
                    new LispInteger(1),
                    new LispInteger(2),
                }
            );
            Assert.Equal(3, matched.Length);

            Assert.Equal("a", matched[0].Item1.Name);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("defaults-to-fourteen", matched[1].Item1.Name);
            Assert.Equal(2, ((LispInteger)matched[1].Item2).Value);

            Assert.Equal("defaults-to-nil", matched[2].Item1.Name);
            Assert.True(matched[2].Item2.IsNil());
        }

        [Fact]
        public void MatchKeywordFunctionArguments()
        {
            // a &key (defaults-to-fourteen 14) defaults-to-nil
            // 1 2
            var matched = MatchArguments(
                new[]
                {
                    new LispRegularFunctionArgument(new LispSymbol("a")),
                },
                Array.Empty<LispOptionalFunctionArgument>(),
                new[]
                {
                    new LispKeywordFunctionArgument(new LispSymbol("defaults-to-fourteen"), new LispInteger(14)),
                    new LispKeywordFunctionArgument(new LispSymbol("defaults-to-nil"), LispNilList.Instance),
                },
                Array.Empty<LispAuxiliaryFunctionArgument>(),
                null,
                new LispObject[]
                {
                    new LispKeyword(":defaults-to-fourteen"),
                    new LispInteger(1),
                    new LispInteger(2),
                }
            );
            Assert.Equal(3, matched.Length);

            Assert.Equal("defaults-to-fourteen", matched[0].Item1.Name);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("a", matched[1].Item1.Name);
            Assert.Equal(2, ((LispInteger)matched[1].Item2).Value);

            Assert.Equal("defaults-to-nil", matched[2].Item1.Name);
            Assert.True(matched[2].Item2.IsNil());
        }

        [Fact]
        public void MatchAuxiliaryFunctionArguments()
        {
            // a &aux (two 2) nniill
            // 1
            var matched = MatchArguments(
                new[]
                {
                    new LispRegularFunctionArgument(new LispSymbol("a")),
                },
                Array.Empty<LispOptionalFunctionArgument>(),
                Array.Empty<LispKeywordFunctionArgument>(),
                new[]
                {
                    new LispAuxiliaryFunctionArgument(new LispSymbol("two"), new LispInteger(2)),
                    new LispAuxiliaryFunctionArgument(new LispSymbol("nniill"), LispNilList.Instance),
                },
                null,
                new LispObject[]
                {
                    new LispInteger(1),
                }
            );
            Assert.Equal(3, matched.Length);

            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);
            Assert.Equal(2, ((LispInteger)matched[1].Item2).Value);
            Assert.True((matched[2].Item2).IsNil());
        }

        [Fact]
        public void MatchRestFunctionArgumentsFromNothing()
        {
            // a &rest the-rest
            // 1
            var matched = MatchArguments(
                new[]
                {
                    new LispRegularFunctionArgument(new LispSymbol("a")),
                },
                Array.Empty<LispOptionalFunctionArgument>(),
                Array.Empty<LispKeywordFunctionArgument>(),
                Array.Empty<LispAuxiliaryFunctionArgument>(),
                new LispRestFunctionArgument(new LispSymbol("the-rest")),
                new LispObject[]
                {
                    new LispInteger(1),
                }
            );
            Assert.Equal(2, matched.Length);

            Assert.Equal("a", matched[0].Item1.Name);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("the-rest", matched[1].Item1.Name);
            Assert.True(matched[1].Item2.IsNil());
        }

        [Fact]
        public void MatchRestFunctionArgumentsFromRemainingValues()
        {
            // a &rest the-rest
            // 1 2 3
            var matched = MatchArguments(
                new[]
                {
                    new LispRegularFunctionArgument(new LispSymbol("a")),
                },
                Array.Empty<LispOptionalFunctionArgument>(),
                Array.Empty<LispKeywordFunctionArgument>(),
                Array.Empty<LispAuxiliaryFunctionArgument>(),
                new LispRestFunctionArgument(new LispSymbol("the-rest")),
                new LispObject[]
                {
                    new LispInteger(1),
                    new LispInteger(2),
                    new LispInteger(3),
                }
            );
            Assert.Equal(2, matched.Length);

            Assert.Equal("a", matched[0].Item1.Name);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("the-rest", matched[1].Item1.Name);
            var rest = ((LispList)matched[1].Item2).ToList();
            Assert.Equal(2, rest.Count);
            Assert.Equal(2, ((LispInteger)rest[0]).Value);
            Assert.Equal(3, ((LispInteger)rest[1]).Value);
        }

        [Fact]
        public void MatchOptionalAndKeywordAndRestArgumentsFromNothing()
        {
            // a &optional nil-list &key some-special-value &rest the-rest
            // 1
            var matched = MatchArguments(
                new[]
                {
                    new LispRegularFunctionArgument(new LispSymbol("a")),
                },
                new[]
                {
                    new LispOptionalFunctionArgument(new LispSymbol("nil-list"), LispNilList.Instance),
                },
                new[]
                {
                    new LispKeywordFunctionArgument(new LispSymbol("some-special-value"), LispNilList.Instance),
                },
                Array.Empty<LispAuxiliaryFunctionArgument>(),
                new LispRestFunctionArgument(new LispSymbol("the-rest")),
                new LispObject[]
                {
                    new LispInteger(1),
                }
            );
            Assert.Equal(4, matched.Length);

            Assert.Equal("a", matched[0].Item1.Name);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("nil-list", matched[1].Item1.Name);
            Assert.True(matched[1].Item2.IsNil());

            Assert.Equal("some-special-value", matched[2].Item1.Name);
            Assert.True(matched[2].Item2.IsNil());

            Assert.Equal("the-rest", matched[3].Item1.Name);
            Assert.True(matched[3].Item2.IsNil());
        }

        [Fact]
        public void MatchKeywordAndOptionalAndRestArgumentsWithValues()
        {
            // a &optional dos &key tres &rest the-rest
            // 1 2 :tres 3 4
            var matched = MatchArguments(
                new[]
                {
                    new LispRegularFunctionArgument(new LispSymbol("a")),
                },
                new[]
                {
                    new LispOptionalFunctionArgument(new LispSymbol("dos"), LispNilList.Instance),
                },
                new[]
                {
                    new LispKeywordFunctionArgument(new LispSymbol("tres"), LispNilList.Instance),
                },
                Array.Empty<LispAuxiliaryFunctionArgument>(),
                new LispRestFunctionArgument(new LispSymbol("the-rest")),
                new LispObject[]
                {
                    new LispInteger(1),
                    new LispInteger(2),
                    new LispKeyword(":tres"),
                    new LispInteger(3),
                    new LispInteger(4),
                }
            );
            Assert.Equal(4, matched.Length);

            Assert.Equal("a", matched[0].Item1.Name);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("dos", matched[1].Item1.Name);
            Assert.Equal(2, ((LispInteger)matched[1].Item2).Value);

            Assert.Equal("tres", matched[2].Item1.Name);
            Assert.Equal(3, ((LispInteger)matched[2].Item2).Value);

            Assert.Equal("the-rest", matched[3].Item1.Name);
            var rest = ((LispList)matched[3].Item2).ToList();
            Assert.Equal(1, rest.Count);
            Assert.Equal(4, ((LispInteger)rest[0]).Value);
        }
    }
}
