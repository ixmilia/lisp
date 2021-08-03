using System;
using System.Collections.Generic;
using System.Linq;

namespace IxMilia.Lisp
{
    public class LispArgumentCollection
    {
        public IReadOnlyList<LispRegularFunctionArgument> RegularArguments { get; }
        public IReadOnlyList<LispOptionalFunctionArgument> OptionalArguments { get; }
        public IReadOnlyDictionary<string, LispKeywordFunctionArgument> KeywordArguments { get; }
        public LispRestFunctionArgument RestArgument { get; }

        internal IEnumerable<string> ArgumentNames
        {
            get
            {
                var argumentNames =
                    RegularArguments.Select(r => r.Name)
                    .Concat(OptionalArguments.Select(o => o.Name))
                    .Concat(KeywordArguments.Select(k => k.Key));
                if (RestArgument is object)
                {
                    argumentNames = argumentNames.Concat(new[] { RestArgument.Name });
                }

                return argumentNames;
            }
        }

        public LispArgumentCollection(IEnumerable<LispRegularFunctionArgument> regularArguments, IEnumerable<LispOptionalFunctionArgument> optionalArguments, IEnumerable<LispKeywordFunctionArgument> keywordArguments, LispRestFunctionArgument rest)
        {
            RegularArguments = regularArguments.ToList();
            OptionalArguments = optionalArguments.ToList();
            KeywordArguments = keywordArguments.ToDictionary(k => k.Name, k => k);
            RestArgument = rest;
        }

        public override string ToString()
        {
            var regularArguments = string.Join(" ", RegularArguments.Select(a => a.ToString()));
            var optionalArguments = string.Join(" ", OptionalArguments.Select(a => a.ToString()));
            var keywordArguments = string.Join(" ", KeywordArguments.Select(a => a.ToString()));
            var rest = RestArgument?.ToString();
            return string.Join(" ", new[] { regularArguments, optionalArguments, keywordArguments, rest });
        }

        internal static bool TryBuildArgumentCollection(LispObject[] argumentList, out LispArgumentCollection argumentCollection, out LispError error)
        {
            argumentCollection = default;
            error = default;

            var regularArguments = new List<LispRegularFunctionArgument>();
            var optionalArguments = new List<LispOptionalFunctionArgument>();
            var keywordArguments = new List<LispKeywordFunctionArgument>();
            LispRestFunctionArgument rest = null;

            var seenArguments = new HashSet<string>();

            for (int i = 0; i < argumentList.Length; i++)
            {
                var argumentListValue = argumentList[i];
                if (argumentListValue is LispSymbol symbol)
                {
                    switch (symbol.Value)
                    {
                        case "&key":
                        case "&optional":
                            i++;

                            Action<string, LispObject> addArgument;
                            switch (symbol.Value)
                            {
                                case "&key":
                                    addArgument = (name, defaultValue) => keywordArguments.Add(new LispKeywordFunctionArgument(name, defaultValue));
                                    break;
                                case "&optional":
                                    addArgument = (name, defaultValue) => optionalArguments.Add(new LispOptionalFunctionArgument(name, defaultValue));
                                    break;
                                default:
                                    throw new InvalidOperationException($"Unexpected argument specifier [{symbol.Value}]");
                            }

                            for (int j = i; j < argumentList.Length; i++, j++)
                            {
                                switch (argumentList[j])
                                {
                                    case LispSymbol argWithNilDefault:
                                        if (argWithNilDefault.Value.StartsWith("&"))
                                        {
                                            // another special argument, backtrack and force-break out of this loop
                                            i--;
                                            j = argumentList.Length;
                                            break;
                                        }

                                        if (!seenArguments.Add(argWithNilDefault.Value))
                                        {
                                            error = new LispError($"Duplicate argument declaratio for {argWithNilDefault.Value}");
                                            return false;
                                        }

                                        addArgument(argWithNilDefault.Value, LispNilList.Instance);
                                        break;
                                    case LispList argWithCustomDefault:
                                        if (argWithCustomDefault.Length == 2 &&
                                            argWithCustomDefault.Value is LispSymbol defaultArgName &&
                                            argWithCustomDefault.Next is LispList defaultValue)
                                        {
                                            if (!seenArguments.Add(defaultArgName.Value))
                                            {
                                                error = new LispError($"Duplicate argument declaratio for {defaultArgName.Value}");
                                                return false;
                                            }
                                            addArgument(defaultArgName.Value, defaultValue.Value);
                                        }
                                        else
                                        {
                                            error = new LispError("Expected argument name with exactly one default value");
                                            return false;
                                        }
                                        break;
                                    default:
                                        error = new LispError("Expected argument name or argument name with default");
                                        return false;
                                }
                            }
                            i--; // back track for next argument
                            break;
                        case "&rest":
                            if (argumentList.Length == i + 2 &&
                                argumentList[i + 1] is LispSymbol restName)
                            {
                                if (!(rest is null))
                                {
                                    error = new LispError("Duplicate `&rest` argument definition");
                                    return false;
                                }

                                rest = new LispRestFunctionArgument(restName.Value);
                                i++;
                            }
                            else
                            {
                                error = new LispError("Expected exactly one remaining argument name");
                                return false;
                            }
                            break;
                        default:
                            // regular variable
                            regularArguments.Add(new LispRegularFunctionArgument(symbol.Value));
                            break;
                    }
                }
                else
                {
                    error = new LispError("Expected argument name");
                    error.Location = argumentListValue.Location;
                    error.Line = argumentListValue.Line;
                    error.Column = argumentListValue.Column;
                    return false;
                }
            }

            argumentCollection = new LispArgumentCollection(regularArguments, optionalArguments, keywordArguments, rest);
            return true;
        }
    }
}
