using System;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class FunctionArgumentTests : TestBase
    {
        private static LispFunctionArgument[] GetArguments(params LispObject[] argumentTypeValues)
        {
            Assert.True(LispFunctionArgument.TryBuildFunctionArguments(argumentTypeValues, out var arguments, out var error), $"Error when building arguments: [{error?.Message}]");
            return arguments;
        }

        private static Tuple<string, LispObject>[] MatchArguments(LispFunctionArgument[] argumentDefinitions, LispObject[] argumentValues)
        {
            Assert.True(LispCodeFunction.TryMatchFunctionArguments(argumentDefinitions, argumentValues, out var matchedArguments, out var error), $"Error when matching arguments: [{error?.Message}]");
            return matchedArguments;
        }

        [Fact]
        public void BindEmptyFunctionArguments()
        {
            var arguments = GetArguments();
            Assert.Empty(arguments);
        }

        [Fact]
        public void BindBuildRegularFunctionArguments()
        {
            // a b c
            var arguments = GetArguments(
                new LispSymbol("a"),
                new LispSymbol("b"),
                new LispSymbol("c")
            );
            Assert.Equal(3, arguments.Length);
            Assert.Equal("a", ((LispRegularFunctionArgument)arguments[0]).Name);
            Assert.Equal("b", ((LispRegularFunctionArgument)arguments[1]).Name);
            Assert.Equal("c", ((LispRegularFunctionArgument)arguments[2]).Name);
        }

        [Fact]
        public void BindOptionalArguments()
        {
            // a &optional (should-be-fourteen 14) should-be-nil
            var arguments = GetArguments(
                new LispSymbol("a"),
                new LispSymbol("&optional"),
                LispList.FromItems(
                    new LispSymbol("should-be-fourteen"),
                    new LispInteger(14)),
                new LispSymbol("should-be-nil")
            );
            Assert.Equal(3, arguments.Length);
            Assert.Equal("a", ((LispRegularFunctionArgument)arguments[0]).Name);
            Assert.Equal("should-be-fourteen", ((LispOptionalFunctionArgument)arguments[1]).Name);
            Assert.Equal(14, ((LispInteger)((LispOptionalFunctionArgument)arguments[1]).DefaultValue).Value);
            Assert.Equal("should-be-nil", ((LispOptionalFunctionArgument)arguments[2]).Name);
            Assert.True(((LispOptionalFunctionArgument)arguments[2]).DefaultValue.IsNil());
        }

        [Fact]
        public void BindRestArgument()
        {
            // a &rest the-rest
            var arguments = GetArguments(
                new LispSymbol("a"),
                new LispSymbol("&rest"),
                new LispSymbol("the-rest")
            );
            Assert.Equal(2, arguments.Length);
            Assert.Equal("a", ((LispRegularFunctionArgument)arguments[0]).Name);
            Assert.Equal("the-rest", ((LispRestFunctionArgument)arguments[1]).Name);
        }

        [Fact]
        public void BindOptionalAndRestArguments()
        {
            // a &optional should-be-nil &rest the-rest
            var arguments = GetArguments(
                new LispSymbol("a"),
                new LispSymbol("&optional"),
                new LispSymbol("should-be-nil"),
                new LispSymbol("&rest"),
                new LispSymbol("the-rest")
            );
            Assert.Equal("a", ((LispRegularFunctionArgument)arguments[0]).Name);
            Assert.Equal("should-be-nil", ((LispOptionalFunctionArgument)arguments[1]).Name);
            Assert.Equal("the-rest", ((LispRestFunctionArgument)arguments[2]).Name);
        }

        [Fact]
        public void MatchEmptyFunctionArguments()
        {
            var matched = MatchArguments(Array.Empty<LispFunctionArgument>(), Array.Empty<LispObject>());
            Assert.Empty(matched);
        }

        [Fact]
        public void MatchRegularFunctionArguments()
        {
            // a b c
            // 1 "two" 3.0
            var matched = MatchArguments(
                new LispFunctionArgument[]
                {
                    new LispRegularFunctionArgument("a"),
                    new LispRegularFunctionArgument("b"),
                    new LispRegularFunctionArgument("c"),
                },
                new LispObject[]
                {
                    new LispInteger(1),
                    new LispString("two"),
                    new LispFloat(3.0),
                }
            );
            Assert.Equal(3, matched.Length);

            Assert.Equal("a", matched[0].Item1);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("b", matched[1].Item1);
            Assert.Equal("two", ((LispString)matched[1].Item2).Value);

            Assert.Equal("c", matched[2].Item1);
            Assert.Equal(3.0, ((LispFloat)matched[2].Item2).Value);
        }

        [Fact]
        public void MatchOptionalFunctionArguments()
        {
            // a &optional (defaults-to-fourteen 14) defaults-to-nil
            // 1 2
            var matched = MatchArguments(
                new LispFunctionArgument[]
                {
                    new LispRegularFunctionArgument("a"),
                    new LispOptionalFunctionArgument("defaults-to-fourteen", new LispInteger(14)),
                    new LispOptionalFunctionArgument("defaults-to-nil", LispNilList.Instance),
                },
                new LispObject[]
                {
                    new LispInteger(1),
                    new LispInteger(2),
                }
            );
            Assert.Equal(3, matched.Length);

            Assert.Equal("a", matched[0].Item1);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("defaults-to-fourteen", matched[1].Item1);
            Assert.Equal(2, ((LispInteger)matched[1].Item2).Value);

            Assert.Equal("defaults-to-nil", matched[2].Item1);
            Assert.True(matched[2].Item2.IsNil());
        }

        [Fact]
        public void MatchRestFunctionArgumentsFromNothing()
        {
            // a &rest the-rest
            // 1
            var matched = MatchArguments(
                new LispFunctionArgument[]
                {
                    new LispRegularFunctionArgument("a"),
                    new LispRestFunctionArgument("the-rest"),
                },
                new LispObject[]
                {
                    new LispInteger(1),
                }
            );
            Assert.Equal(2, matched.Length);

            Assert.Equal("a", matched[0].Item1);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("the-rest", matched[1].Item1);
            Assert.True(matched[1].Item2.IsNil());
        }

        [Fact]
        public void MatchRestFunctionArgumentsFromRemainingValues()
        {
            // a &rest the-rest
            // 1 2 3
            var matched = MatchArguments(
                new LispFunctionArgument[]
                {
                    new LispRegularFunctionArgument("a"),
                    new LispRestFunctionArgument("the-rest"),
                },
                new LispObject[]
                {
                    new LispInteger(1),
                    new LispInteger(2),
                    new LispInteger(3),
                }
            );
            Assert.Equal(2, matched.Length);

            Assert.Equal("a", matched[0].Item1);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("the-rest", matched[1].Item1);
            var rest = ((LispList)matched[1].Item2).ToList();
            Assert.Equal(2, rest.Count);
            Assert.Equal(2, ((LispInteger)rest[0]).Value);
            Assert.Equal(3, ((LispInteger)rest[1]).Value);
        }

        [Fact]
        public void MatchOptionalAndRestArgumentsFromNothing()
        {
            // a &optional nil-list &rest the-rest
            // 1
            var matched = MatchArguments(
                new LispFunctionArgument[]
                {
                    new LispRegularFunctionArgument("a"),
                    new LispOptionalFunctionArgument("nil-list", LispNilList.Instance),
                    new LispRestFunctionArgument("the-rest"),
                },
                new LispObject[]
                {
                    new LispInteger(1),
                }
            );
            Assert.Equal(3, matched.Length);

            Assert.Equal("a", matched[0].Item1);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("nil-list", matched[1].Item1);
            Assert.True(matched[1].Item2.IsNil());

            Assert.Equal("the-rest", matched[2].Item1);
            Assert.True(matched[2].Item2.IsNil());
        }

        [Fact]
        public void MatchOptionalAndRestArgumentsWithValues()
        {
            // a &optional dos &rest the-rest
            // 1 2 3 4
            var matched = MatchArguments(
                new LispFunctionArgument[]
                {
                    new LispRegularFunctionArgument("a"),
                    new LispOptionalFunctionArgument("dos", LispNilList.Instance),
                    new LispRestFunctionArgument("the-rest"),
                },
                new LispObject[]
                {
                    new LispInteger(1),
                    new LispInteger(2),
                    new LispInteger(3),
                    new LispInteger(4),
                }
            );
            Assert.Equal(3, matched.Length);

            Assert.Equal("a", matched[0].Item1);
            Assert.Equal(1, ((LispInteger)matched[0].Item2).Value);

            Assert.Equal("dos", matched[1].Item1);
            Assert.Equal(2, ((LispInteger)matched[1].Item2).Value);

            Assert.Equal("the-rest", matched[2].Item1);
            var rest = ((LispList)matched[2].Item2).ToList();
            Assert.Equal(2, rest.Count);
            Assert.Equal(3, ((LispInteger)rest[0]).Value);
            Assert.Equal(4, ((LispInteger)rest[1]).Value);
        }
    }
}
