using System;
using System.Collections.Generic;
using System.Linq;

namespace IxMilia.Lisp
{
    public class LispArgumentCollection
    {
        public IReadOnlyList<LispRegularInvocationArgument> RegularArguments { get; }
        public IReadOnlyList<LispOptionalInvocationArgument> OptionalArguments { get; }
        public IReadOnlyDictionary<string, LispKeywordInvocationArgument> KeywordArguments { get; }
        public IReadOnlyList<LispAuxiliaryInvocationArgument> AuxiliaryArguments { get; }
        public LispRestInvocationArgument RestArgument { get; }

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

        public IEnumerable<LispObject> GetChildren()
        {
            foreach (var argument in RegularArguments)
            {
                yield return argument.Declaration;
            }

            foreach (var argument in OptionalArguments)
            {
                yield return argument.Declaration;
                yield return argument.DefaultValue;
            }

            foreach (var argument in KeywordArguments.Values)
            {
                yield return argument.Declaration;
                yield return argument.DefaultValue;
            }

            foreach (var argument in AuxiliaryArguments)
            {
                yield return argument.Declaration;
                yield return argument.InitialValue;
            }

            if (RestArgument != null)
            {
                yield return RestArgument.Declaration;
            }
        }

        public LispArgumentCollection(IEnumerable<LispRegularInvocationArgument> regularArguments, IEnumerable<LispOptionalInvocationArgument> optionalArguments, IEnumerable<LispKeywordInvocationArgument> keywordArguments, IEnumerable<LispAuxiliaryInvocationArgument> auxiliaryArguments, LispRestInvocationArgument rest)
        {
            RegularArguments = regularArguments.ToList();
            OptionalArguments = optionalArguments.ToList();
            KeywordArguments = keywordArguments.ToDictionary(k => k.Name, k => k);
            AuxiliaryArguments = auxiliaryArguments.ToList();
            RestArgument = rest;
        }

        public override string ToString() => ToString(a => a.ToString());

        public string ToDisplayString(LispPackage currentPackage) => ToString(a => a.ToDisplayString(currentPackage));

        private string ToString(Func<LispInvocationArgument, string> argumentToString)
        {
            var regularArguments = string.Join(" ", RegularArguments.Select(argumentToString)).Trim();
            var optionalArguments = string.Join(" ", OptionalArguments.Select(argumentToString)).Trim();
            var keywordArguments = string.Join(" ", KeywordArguments.Select(a => argumentToString(a.Value))).Trim();
            var auxiliaryArguments = string.Join(" ", AuxiliaryArguments.Select(argumentToString)).Trim();
            var rest = RestArgument is { } ? argumentToString(RestArgument) : null;
            return string.Join(" ", new[] { regularArguments, optionalArguments, keywordArguments, auxiliaryArguments, rest }).Trim();
        }

        internal bool TryMatchInvocationArguments(LispObject[] argumentValues, out Tuple<LispInvocationArgument, LispObject>[] matchedArguments, out LispError error)
        {
            matchedArguments = default;
            error = default;

            var regularArgumentIndex = 0;
            var optionalArgumentIndex = 0;
            var argumentValueIndex = 0;
            var matchedArgumentsList = new List<Tuple<LispInvocationArgument, LispObject>>();
            var boundArguments = new HashSet<string>();
            var assignedRest = false;
            for (; argumentValueIndex < argumentValues.Length; argumentValueIndex++)
            {
                var assignedKeywordArgument = false;
                var argumentValue = argumentValues[argumentValueIndex];
                if (argumentValue is LispResolvedSymbol symbol && symbol.IsKeyword)
                {
                    if (argumentValueIndex < argumentValues.Length - 1)
                    {
                        var keywordArgumentName = symbol.Value.Substring(1);
                        if (KeywordArguments.TryGetValue(keywordArgumentName, out var keywordArgument))
                        {
                            if (!boundArguments.Add(keywordArgumentName))
                            {
                                error = new LispError($"Duplicate value for keyword argument {keywordArgumentName}");
                                return false;
                            }

                            matchedArgumentsList.Add(Tuple.Create((LispInvocationArgument)keywordArgument, argumentValues[argumentValueIndex + 1]));
                            argumentValueIndex++;
                            assignedKeywordArgument = true;
                        }
                    }
                }

                if (!assignedKeywordArgument)
                {
                    if (regularArgumentIndex < RegularArguments.Count)
                    {
                        var regularArgument = RegularArguments[regularArgumentIndex];
                        if (!boundArguments.Add(regularArgument.Name))
                        {
                            error = new LispError($"Duplicate value for argument {regularArgument.Name}");
                            return false;
                        }

                        matchedArgumentsList.Add(Tuple.Create((LispInvocationArgument)regularArgument, argumentValue));
                        regularArgumentIndex++;
                    }
                    else if (optionalArgumentIndex < OptionalArguments.Count)
                    {
                        var optionalArgument = OptionalArguments[optionalArgumentIndex];
                        if (!boundArguments.Add(optionalArgument.Name))
                        {
                            error = new LispError($"Duplicate value for optional argument {optionalArgument.Name}");
                            return false;
                        }

                        matchedArgumentsList.Add(Tuple.Create((LispInvocationArgument)optionalArgument, argumentValue));
                        optionalArgumentIndex++;
                    }
                    else if (RestArgument is object)
                    {
                        if (!boundArguments.Add(RestArgument.Name))
                        {
                            error = new LispError($"Duplicate binding for `&rest` argument {RestArgument.Name}");
                            return false;
                        }

                        var restArgumentList = LispList.FromEnumerable(argumentValues.Skip(argumentValueIndex));
                        matchedArgumentsList.Add(Tuple.Create((LispInvocationArgument)RestArgument, (LispObject)restArgumentList));
                        argumentValueIndex = argumentValues.Length;
                        assignedRest = true;
                    }
                    else
                    {
                        var errorMessage = argumentValue is LispResolvedSymbol resolvedSymbol && resolvedSymbol.IsKeyword
                            ? $"Missing value for keyword argument {resolvedSymbol.Value}"
                            : "Too many arguments";
                        error = new LispError(errorMessage);
                        return false;
                    }
                }
            }

            if (regularArgumentIndex < RegularArguments.Count)
            {
                error = new LispError("Too few arguments");
                return false;
            }

            // use defaults on any remaining optional arguments
            foreach (var optionalArgument in OptionalArguments.Skip(optionalArgumentIndex))
            {
                if (!boundArguments.Add(optionalArgument.Name))
                {
                    error = new LispError($"Duplicate binding for optional argument {optionalArgument.Name}");
                    return false;
                }

                matchedArgumentsList.Add(Tuple.Create((LispInvocationArgument)optionalArgument, optionalArgument.DefaultValue));
            }

            // use defaults on any remaining keyword arguments
            foreach (var keywordArgument in KeywordArguments)
            {
                if (!boundArguments.Contains(keywordArgument.Key))
                {
                    matchedArgumentsList.Add(Tuple.Create((LispInvocationArgument)keywordArgument.Value, keywordArgument.Value.DefaultValue));
                    boundArguments.Add(keywordArgument.Key);
                }
            }

            if (argumentValueIndex < argumentValues.Length)
            {
                // any remaining values are `&rest`
                if (RestArgument is null)
                {
                    error = new LispError("Too many arguments");
                    return false;
                }
                else
                {
                    var restArgumentList = LispList.FromEnumerable(argumentValues.Skip(argumentValueIndex));
                    matchedArgumentsList.Add(Tuple.Create((LispInvocationArgument)RestArgument, (LispObject)restArgumentList));
                }
            }
            else if (!assignedRest)
            {
                if (RestArgument is object)
                {
                    // `&rest` is empty
                    matchedArgumentsList.Add(Tuple.Create((LispInvocationArgument)RestArgument, (LispObject)LispNilList.Instance));
                }
            }

            // `&aux` are always last
            foreach (var auxiliaryArgument in AuxiliaryArguments)
            {
                matchedArgumentsList.Add(Tuple.Create((LispInvocationArgument)auxiliaryArgument, auxiliaryArgument.InitialValue));
            }

            matchedArguments = matchedArgumentsList.ToArray();
            return true;
        }

        internal static LispArgumentCollection Empty => new LispArgumentCollection(Array.Empty<LispRegularInvocationArgument>(), Array.Empty<LispOptionalInvocationArgument>(), Array.Empty<LispKeywordInvocationArgument>(), Array.Empty<LispAuxiliaryInvocationArgument>(), null);

        internal static bool TryBuildArgumentCollection(LispObject[] argumentList, out LispArgumentCollection argumentCollection, out LispError error)
        {
            argumentCollection = default;
            error = default;

            var regularArguments = new List<LispRegularInvocationArgument>();
            var optionalArguments = new List<LispOptionalInvocationArgument>();
            var keywordArguments = new List<LispKeywordInvocationArgument>();
            var auxiliaryArguments = new List<LispAuxiliaryInvocationArgument>();
            LispRestInvocationArgument rest = null;

            var seenArguments = new HashSet<string>();

            for (int i = 0; i < argumentList.Length; i++)
            {
                var argumentListValue = argumentList[i];
                if (argumentListValue is LispLambdaListKeyword lambdaListKeyword)
                {
                    switch (lambdaListKeyword.Keyword)
                    {
                        case "&AUX":
                        case "&KEY":
                        case "&OPTIONAL":
                            i++;

                            Action<LispSymbol, LispObject> addArgument;
                            switch (lambdaListKeyword.Keyword)
                            {
                                case "&AUX":
                                    addArgument = (declaration, initialValue) => auxiliaryArguments.Add(new LispAuxiliaryInvocationArgument(declaration, initialValue));
                                    break;
                                case "&KEY":
                                    addArgument = (declaration, defaultValue) => keywordArguments.Add(new LispKeywordInvocationArgument(declaration, defaultValue));
                                    break;
                                case "&OPTIONAL":
                                    addArgument = (declaration, defaultValue) => optionalArguments.Add(new LispOptionalInvocationArgument(declaration, defaultValue));
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
                                        if (!seenArguments.Add(argWithNilDefault.LocalName))
                                        {
                                            error = new LispError($"Duplicate argument declaration for {argWithNilDefault.LocalName}");
                                            return false;
                                        }

                                        addArgument(argWithNilDefault, LispNilList.Instance);
                                        break;
                                    case LispList argWithCustomDefault:
                                        if (argWithCustomDefault.Length == 2 &&
                                            argWithCustomDefault.Value is LispSymbol defaultArgName &&
                                            argWithCustomDefault.Next is LispList defaultValue)
                                        {
                                            var defaultArgNameString = defaultArgName.ToString();
                                            if (!seenArguments.Add(defaultArgNameString))
                                            {
                                                error = new LispError($"Duplicate argument declaration for {defaultArgNameString}");
                                                return false;
                                            }

                                            addArgument(defaultArgName, defaultValue.Value);
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
                        case "&REST":
                            if (argumentList.Length == i + 2 &&
                                argumentList[i + 1] is LispSymbol restName)
                            {
                                if (!(rest is null))
                                {
                                    error = new LispError("Duplicate `&REST` argument definition");
                                    return false;
                                }

                                rest = new LispRestInvocationArgument(restName);
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
                    regularArguments.Add(new LispRegularInvocationArgument(symbol));
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
