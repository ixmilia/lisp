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
        public IReadOnlyList<LispAuxiliaryFunctionArgument> AuxiliaryArguments { get; }
        public LispRestFunctionArgument RestArgument { get; }

        internal IEnumerable<string> ArgumentNames
        {
            get
            {
                var argumentNames =
                    RegularArguments.Select(r => r.Name)
                    .Concat(OptionalArguments.Select(o => o.Name))
                    .Concat(KeywordArguments.Select(k => k.Key))
                    .Concat(AuxiliaryArguments.Select(a => a.Name));
                if (RestArgument is object)
                {
                    argumentNames = argumentNames.Concat(new[] { RestArgument.Name });
                }

                return argumentNames;
            }
        }

        public LispArgumentCollection(IEnumerable<LispRegularFunctionArgument> regularArguments, IEnumerable<LispOptionalFunctionArgument> optionalArguments, IEnumerable<LispKeywordFunctionArgument> keywordArguments, IEnumerable<LispAuxiliaryFunctionArgument> auxiliaryArguments, LispRestFunctionArgument rest)
        {
            RegularArguments = regularArguments.ToList();
            OptionalArguments = optionalArguments.ToList();
            KeywordArguments = keywordArguments.ToDictionary(k => k.Name, k => k);
            AuxiliaryArguments = auxiliaryArguments.ToList();
            RestArgument = rest;
        }

        public override string ToString()
        {
            var regularArguments = string.Join(" ", RegularArguments.Select(a => a.ToString()));
            var optionalArguments = string.Join(" ", OptionalArguments.Select(a => a.ToString()));
            var keywordArguments = string.Join(" ", KeywordArguments.Select(a => a.ToString()));
            var auxiliaryArguments = string.Join(" ", AuxiliaryArguments.Select(a => a.ToString()));
            var rest = RestArgument?.ToString();
            return string.Join(" ", new[] { regularArguments, optionalArguments, keywordArguments, auxiliaryArguments, rest });
        }

        internal static bool TryBuildArgumentCollection(LispObject[] argumentList, out LispArgumentCollection argumentCollection, out LispError error)
        {
            argumentCollection = default;
            error = default;

            var regularArguments = new List<LispRegularFunctionArgument>();
            var optionalArguments = new List<LispOptionalFunctionArgument>();
            var keywordArguments = new List<LispKeywordFunctionArgument>();
            var auxiliaryArguments = new List<LispAuxiliaryFunctionArgument>();
            LispRestFunctionArgument rest = null;

            var seenArguments = new HashSet<string>();

            for (int i = 0; i < argumentList.Length; i++)
            {
                var argumentListValue = argumentList[i];
                if (argumentListValue is LispLambdaListKeyword lambdaListKeyword)
                {
                    switch (lambdaListKeyword.Keyword)
                    {
                        case "&aux":
                        case "&key":
                        case "&optional":
                            i++;

                            Action<string, LispObject> addArgument;
                            switch (lambdaListKeyword.Keyword)
                            {
                                case "&aux":
                                    addArgument = (name, initialValue) => auxiliaryArguments.Add(new LispAuxiliaryFunctionArgument(name, initialValue));
                                    break;
                                case "&key":
                                    addArgument = (name, defaultValue) => keywordArguments.Add(new LispKeywordFunctionArgument(name, defaultValue));
                                    break;
                                case "&optional":
                                    addArgument = (name, defaultValue) => optionalArguments.Add(new LispOptionalFunctionArgument(name, defaultValue));
                                    break;
                                default:
                                    throw new InvalidOperationException($"Unexpected argument specifier [{lambdaListKeyword.Keyword}]");
                            }

                            for (int j = i; j < argumentList.Length; i++, j++)
                            {
                                switch (argumentList[j])
                                {
                                    case LispLambdaListKeyword _:
                                        // another special argument, backtrack and force-break out of this loop
                                        i--;
                                        j = argumentList.Length;
                                        break;
                                    case LispSymbol argWithNilDefault:
                                        if (!seenArguments.Add(argWithNilDefault.Value))
                                        {
                                            error = new LispError($"Duplicate argument declaration for {argWithNilDefault.Value}");
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
                                                error = new LispError($"Duplicate argument declaration for {defaultArgName.Value}");
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
                            error = new LispError($"Unsupported lambda list keyword {lambdaListKeyword.Keyword}");
                            return false;
                    }
                }
                else if (argumentListValue is LispSymbol symbol)
                {
                    // regular variable
                    regularArguments.Add(new LispRegularFunctionArgument(symbol.Value));
                }
                else
                {
                    error = new LispError("Expected argument name");
                    error.SourceLocation = argumentListValue.SourceLocation;
                    return false;
                }
            }

            argumentCollection = new LispArgumentCollection(regularArguments, optionalArguments, keywordArguments, auxiliaryArguments, rest);
            return true;
        }
    }
}
