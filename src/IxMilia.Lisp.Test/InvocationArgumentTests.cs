using System;
using System.Collections.Generic;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class InvocationArgumentTests : TestBase
    {
        private static LispArgumentCollection GetArgumentCollection(params LispObject[] argumentTypeValues)
        {
            Assert.True(LispArgumentCollection.TryBuildArgumentCollection(argumentTypeValues, out var argumentCollection, out var error), $"Error when building arguments: [{error?.Message}]");
            return argumentCollection;
        }

        private static Tuple<LispInvocationArgument, LispObject>[] MatchArguments(IEnumerable<LispRegularInvocationArgument> regularArguments, IEnumerable<LispOptionalInvocationArgument> optionalArguments, IEnumerable<LispKeywordInvocationArgument> keywordArguments, IEnumerable<LispAuxiliaryInvocationArgument> auxiliaryArguments, LispRestInvocationArgument restArgument, LispObject[] argumentValues)
        {
            var argumentCollection = new LispArgumentCollection(regularArguments, optionalArguments, keywordArguments, auxiliaryArguments, restArgument);
            Assert.True(argumentCollection.TryMatchInvocationArguments(argumentValues, out var matchedArguments, out var error), $"Error when matching arguments: [{error?.Message}]");
            return matchedArguments;
        }

        [Fact]
        public void BindEmptyInvocationArguments()
        {
            var argumentCollection = GetArgumentCollection();
            Assert.Empty(argumentCollection.RegularArguments);
            Assert.Empty(argumentCollection.OptionalArguments);
            Assert.Empty(argumentCollection.KeywordArguments);
            Assert.Empty(argumentCollection.AuxiliaryArguments);
            Assert.Null(argumentCollection.RestArgument);
        }

        [Fact]
        public void BindBuildRegularInvocationArguments()
        {
            // a b c
            var argumentCollection = GetArgumentCollection(
                LispSymbol.CreateFromString("A"),
                LispSymbol.CreateFromString("B"),
                LispSymbol.CreateFromString("C")
            );
            Assert.Equal(3, argumentCollection.RegularArguments.Count);
            Assert.Empty(argumentCollection.OptionalArguments);
            Assert.Empty(argumentCollection.KeywordArguments);
            Assert.Empty(argumentCollection.AuxiliaryArguments);
            Assert.Null(argumentCollection.RestArgument);

            Assert.Equal("A", argumentCollection.RegularArguments[0].Name);
            Assert.Equal("B", argumentCollection.RegularArguments[1].Name);
            Assert.Equal("C", argumentCollection.RegularArguments[2].Name);
        }

        [Fact]
        public void BindOptionalArguments()
        {
            // a &optional (should-be-fourteen 14) should-be-nil
            var argumentCollection = GetArgumentCollection(
                LispSymbol.CreateFromString("A"),
                new LispLambdaListKeyword("&OPTIONAL"),
                LispList.FromItems(
                    LispSymbol.CreateFromString("SHOULD-BE-FOURTEEN"),
                    new LispInteger(14)),
                LispSymbol.CreateFromString("SHOULD-BE-NIL")
            );
            Assert.Equal(1, argumentCollection.RegularArguments.Count);
            Assert.Equal(2, argumentCollection.OptionalArguments.Count);
            Assert.Empty(argumentCollection.KeywordArguments);
            Assert.Empty(argumentCollection.AuxiliaryArguments);
            Assert.Null(argumentCollection.RestArgument);

            Assert.Equal("A", argumentCollection.RegularArguments[0].Name);
            Assert.Equal("SHOULD-BE-FOURTEEN", argumentCollection.OptionalArguments[0].Name);
            Assert.Equal(14, ((LispInteger)argumentCollection.OptionalArguments[0].DefaultValue).Value);
            Assert.Equal("SHOULD-BE-NIL", argumentCollection.OptionalArguments[1].Name);
            Assert.True(argumentCollection.OptionalArguments[1].DefaultValue.IsNil());
        }

        [Fact]
        public void BindKeywordArguments()
        {
            // a &key (should-be-fourteen 14) should-be-nil
            var argumentCollection = GetArgumentCollection(
                LispSymbol.CreateFromString("A"),
                new LispLambdaListKeyword("&KEY"),
                LispList.FromItems(
                    LispSymbol.CreateFromString("SHOULD-BE-FOURTEEN"),
                    new LispInteger(14)),
                LispSymbol.CreateFromString("SHOULD-BE-NIL")
            );
            Assert.Equal(1, argumentCollection.RegularArguments.Count);
            Assert.Empty(argumentCollection.OptionalArguments);
            Assert.Equal(2, argumentCollection.KeywordArguments.Count);
            Assert.Empty(argumentCollection.AuxiliaryArguments);
            Assert.Null(argumentCollection.RestArgument);

            Assert.Equal("A", argumentCollection.RegularArguments[0].Name);
            Assert.Equal(14, ((LispInteger)argumentCollection.KeywordArguments["SHOULD-BE-FOURTEEN"].DefaultValue).Value);
            Assert.True(argumentCollection.KeywordArguments["SHOULD-BE-NIL"].DefaultValue.IsNil());
        }

        [Fact]
        public void BindAuxiliaryArguments()
        {
            // a &aux (two 2) nniill
            var argumentCollection = GetArgumentCollection(
                LispSymbol.CreateFromString("A"),
                new LispLambdaListKeyword("&AUX"),
                LispList.FromItems(
                    LispSymbol.CreateFromString("TWO"),
                    new LispInteger(2)),
                LispSymbol.CreateFromString("NNIILL")
            );
            Assert.Equal(1, argumentCollection.RegularArguments.Count);
            Assert.Empty(argumentCollection.OptionalArguments);
            Assert.Empty(argumentCollection.KeywordArguments);
            Assert.Equal(2, argumentCollection.AuxiliaryArguments.Count);
            Assert.Null(argumentCollection.RestArgument);

            Assert.Equal("A", argumentCollection.RegularArguments[0].Name);
            Assert.Equal("TWO", argumentCollection.AuxiliaryArguments[0].Name);
            Assert.Equal(2, ((LispInteger)argumentCollection.AuxiliaryArguments[0].InitialValue).Value);
            Assert.Equal("NNIILL", argumentCollection.AuxiliaryArguments[1].Name);
            Assert.True(argumentCollection.AuxiliaryArguments[1].InitialValue.IsNil());
        }

        [Fact]
        public void BindRestArgument()
        {
            // a &rest the-rest
            var argumentCollection = GetArgumentCollection(
                LispSymbol.CreateFromString("A"),
                new LispLambdaListKeyword("&REST"),
                LispSymbol.CreateFromString("THE-REST")
            );
            Assert.Equal(1, argumentCollection.RegularArguments.Count);
            Assert.Empty(argumentCollection.OptionalArguments);
            Assert.Empty(argumentCollection.KeywordArguments);
            Assert.NotNull(argumentCollection.RestArgument);

            Assert.Equal("A", argumentCollection.RegularArguments[0].Name);
            Assert.Equal("THE-REST", argumentCollection.RestArgument.Name);
        }

        [Fact]
        public void BindOptionalAndKeywordAndRestArguments()
        {
            // a &optional should-be-nil &key some-special-value &rest the-rest
            var argumentCollection = GetArgumentCollection(
                LispSymbol.CreateFromString("A"),
                new LispLambdaListKeyword("&OPTIONAL"),
                LispSymbol.CreateFromString("SHOULD-BE-NIL"),
                new LispLambdaListKeyword("&KEY"),
                LispSymbol.CreateFromString("SOME-SPECIAL-VALUE"),
                new LispLambdaListKeyword("&REST"),
                LispSymbol.CreateFromString("THE-REST")
            );
            Assert.Equal(1, argumentCollection.RegularArguments.Count);
            Assert.Equal(1, argumentCollection.OptionalArguments.Count);
            Assert.Equal(1, argumentCollection.KeywordArguments.Count);
            Assert.NotNull(argumentCollection.RestArgument);

            Assert.Equal("A", argumentCollection.RegularArguments[0].Name);
            Assert.Equal("SHOULD-BE-NIL", argumentCollection.OptionalArguments[0].Name);
            Assert.True(argumentCollection.KeywordArguments.ContainsKey("SOME-SPECIAL-VALUE"));
            Assert.Equal("THE-REST", argumentCollection.RestArgument.Name);
        }

        [Fact]
        public void MatchEmptyInvocationArguments()
        {
            var matched = MatchArguments(Array.Empty<LispRegularInvocationArgument>(), Array.Empty<LispOptionalInvocationArgument>(), Array.Empty<LispKeywordInvocationArgument>(), Array.Empty<LispAuxiliaryInvocationArgument>(), null, Array.Empty<LispObject>());
            Assert.Empty(matched);
        }

        [Fact]
        public void MatchRegularInvocationArguments()
        {
            // a b c
            // 1 "two" 3.0
            var matched = MatchArguments(
                new[]
                {
                    new LispRegularInvocationArgument(LispSymbol.CreateFromString("A")),
                    new LispRegularInvocationArgument(LispSymbol.CreateFromString("B")),
                    new LispRegularInvocationArgument(LispSymbol.CreateFromString("C")),
                },
                Array.Empty<LispOptionalInvocationArgument>(),
                Array.Empty<LispKeywordInvocationArgument>(),
                Array.Empty<LispAuxiliaryInvocationArgument>(),
                null,
                new LispObject[]
                {
                    new LispInteger(1),
                    new LispString("TWO"),
                    new LispFloat(3.0),
                }
            );
            Assert.Equal(3, matched.Length);

            Assert.Equal("A", matched[0].Item1.Name);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("B", matched[1].Item1.Name);
            Assert.Equal("TWO", ((LispString)matched[1].Item2).Value);

            Assert.Equal("C", matched[2].Item1.Name);
            Assert.Equal(3.0, ((LispFloat)matched[2].Item2).Value);
        }

        [Fact]
        public void MatchOptionalInvocationArguments()
        {
            // a &optional (defaults-to-fourteen 14) defaults-to-nil
            // 1 2
            var matched = MatchArguments(
                new[]
                {
                    new LispRegularInvocationArgument(LispSymbol.CreateFromString("A")),
                },
                new[]
                {
                    new LispOptionalInvocationArgument(LispSymbol.CreateFromString("DEFAULTS-TO-FOURTEEN"), new LispInteger(14)),
                    new LispOptionalInvocationArgument(LispSymbol.CreateFromString("DEFAULTS-TO-NIL"), LispNilList.Instance),
                },
                Array.Empty<LispKeywordInvocationArgument>(),
                Array.Empty<LispAuxiliaryInvocationArgument>(),
                null,
                new LispObject[]
                {
                    new LispInteger(1),
                    new LispInteger(2),
                }
            );
            Assert.Equal(3, matched.Length);

            Assert.Equal("A", matched[0].Item1.Name);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("DEFAULTS-TO-FOURTEEN", matched[1].Item1.Name);
            Assert.Equal(2, ((LispInteger)matched[1].Item2).Value);

            Assert.Equal("DEFAULTS-TO-NIL", matched[2].Item1.Name);
            Assert.True(matched[2].Item2.IsNil());
        }

        [Fact]
        public void MatchKeywordInvocationArguments()
        {
            // a &key (defaults-to-fourteen 14) defaults-to-nil
            // 1 2
            var matched = MatchArguments(
                new[]
                {
                    new LispRegularInvocationArgument(LispSymbol.CreateFromString("A")),
                },
                Array.Empty<LispOptionalInvocationArgument>(),
                new[]
                {
                    new LispKeywordInvocationArgument(LispSymbol.CreateFromString("DEFAULTS-TO-FOURTEEN"), new LispInteger(14)),
                    new LispKeywordInvocationArgument(LispSymbol.CreateFromString("DEFAULTS-TO-NIL"), LispNilList.Instance),
                },
                Array.Empty<LispAuxiliaryInvocationArgument>(),
                null,
                new LispObject[]
                {
                    LispSymbol.CreateFromString(":DEFAULTS-TO-FOURTEEN"),
                    new LispInteger(1),
                    new LispInteger(2),
                }
            );
            Assert.Equal(3, matched.Length);

            Assert.Equal("DEFAULTS-TO-FOURTEEN", matched[0].Item1.Name);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("A", matched[1].Item1.Name);
            Assert.Equal(2, ((LispInteger)matched[1].Item2).Value);

            Assert.Equal("DEFAULTS-TO-NIL", matched[2].Item1.Name);
            Assert.True(matched[2].Item2.IsNil());
        }

        [Fact]
        public void MatchAuxiliaryInvocationArguments()
        {
            // a &aux (two 2) nniill
            // 1
            var matched = MatchArguments(
                new[]
                {
                    new LispRegularInvocationArgument(LispSymbol.CreateFromString("A")),
                },
                Array.Empty<LispOptionalInvocationArgument>(),
                Array.Empty<LispKeywordInvocationArgument>(),
                new[]
                {
                    new LispAuxiliaryInvocationArgument(LispSymbol.CreateFromString("TWO"), new LispInteger(2)),
                    new LispAuxiliaryInvocationArgument(LispSymbol.CreateFromString("NNIILL"), LispNilList.Instance),
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
        public void MatchRestInvocationArgumentsFromNothing()
        {
            // a &rest the-rest
            // 1
            var matched = MatchArguments(
                new[]
                {
                    new LispRegularInvocationArgument(LispSymbol.CreateFromString("A")),
                },
                Array.Empty<LispOptionalInvocationArgument>(),
                Array.Empty<LispKeywordInvocationArgument>(),
                Array.Empty<LispAuxiliaryInvocationArgument>(),
                new LispRestInvocationArgument(LispSymbol.CreateFromString("THE-REST")),
                new LispObject[]
                {
                    new LispInteger(1),
                }
            );
            Assert.Equal(2, matched.Length);

            Assert.Equal("A", matched[0].Item1.Name);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("THE-REST", matched[1].Item1.Name);
            Assert.True(matched[1].Item2.IsNil());
        }

        [Fact]
        public void MatchRestInvocationArgumentsFromRemainingValues()
        {
            // a &rest the-rest
            // 1 2 3
            var matched = MatchArguments(
                new[]
                {
                    new LispRegularInvocationArgument(LispSymbol.CreateFromString("A")),
                },
                Array.Empty<LispOptionalInvocationArgument>(),
                Array.Empty<LispKeywordInvocationArgument>(),
                Array.Empty<LispAuxiliaryInvocationArgument>(),
                new LispRestInvocationArgument(LispSymbol.CreateFromString("THE-REST")),
                new LispObject[]
                {
                    new LispInteger(1),
                    new LispInteger(2),
                    new LispInteger(3),
                }
            );
            Assert.Equal(2, matched.Length);

            Assert.Equal("A", matched[0].Item1.Name);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("THE-REST", matched[1].Item1.Name);
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
                    new LispRegularInvocationArgument(LispSymbol.CreateFromString("A")),
                },
                new[]
                {
                    new LispOptionalInvocationArgument(LispSymbol.CreateFromString("NIL-LIST"), LispNilList.Instance),
                },
                new[]
                {
                    new LispKeywordInvocationArgument(LispSymbol.CreateFromString("SOME-SPECIAL-VALUE"), LispNilList.Instance),
                },
                Array.Empty<LispAuxiliaryInvocationArgument>(),
                new LispRestInvocationArgument(LispSymbol.CreateFromString("THE-REST")),
                new LispObject[]
                {
                    new LispInteger(1),
                }
            );
            Assert.Equal(4, matched.Length);

            Assert.Equal("A", matched[0].Item1.Name);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("NIL-LIST", matched[1].Item1.Name);
            Assert.True(matched[1].Item2.IsNil());

            Assert.Equal("SOME-SPECIAL-VALUE", matched[2].Item1.Name);
            Assert.True(matched[2].Item2.IsNil());

            Assert.Equal("THE-REST", matched[3].Item1.Name);
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
                    new LispRegularInvocationArgument(LispSymbol.CreateFromString("A")),
                },
                new[]
                {
                    new LispOptionalInvocationArgument(LispSymbol.CreateFromString("DOS"), LispNilList.Instance),
                },
                new[]
                {
                    new LispKeywordInvocationArgument(LispSymbol.CreateFromString("TRES"), LispNilList.Instance),
                },
                Array.Empty<LispAuxiliaryInvocationArgument>(),
                new LispRestInvocationArgument(LispSymbol.CreateFromString("THE-REST")),
                new LispObject[]
                {
                    new LispInteger(1),
                    new LispInteger(2),
                    LispSymbol.CreateFromString(":TRES"),
                    new LispInteger(3),
                    new LispInteger(4),
                }
            );
            Assert.Equal(4, matched.Length);

            Assert.Equal("A", matched[0].Item1.Name);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("DOS", matched[1].Item1.Name);
            Assert.Equal(2, ((LispInteger)matched[1].Item2).Value);

            Assert.Equal("TRES", matched[2].Item1.Name);
            Assert.Equal(3, ((LispInteger)matched[2].Item2).Value);

            Assert.Equal("THE-REST", matched[3].Item1.Name);
            var rest = ((LispList)matched[3].Item2).ToList();
            Assert.Equal(1, rest.Count);
            Assert.Equal(4, ((LispInteger)rest[0]).Value);
        }

        [Fact]
        public void KeywordSymbolCanBindToRegularArguments()
        {
            var matched = MatchArguments(
                new[]
                {
                    new LispRegularInvocationArgument(LispSymbol.CreateFromString("A")),
                },
                Array.Empty<LispOptionalInvocationArgument>(),
                Array.Empty<LispKeywordInvocationArgument>(),
                Array.Empty<LispAuxiliaryInvocationArgument>(),
                null,
                new LispObject[]
                {
                    LispSymbol.CreateFromString(":SOME-KEYWORD")
                }
            );

            Assert.Single(matched);
            Assert.Equal("A", matched[0].Item1.Name);
            Assert.Equal(":SOME-KEYWORD", matched[0].Item2.ToString());
        }
    }
}
