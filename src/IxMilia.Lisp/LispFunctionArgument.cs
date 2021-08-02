using System.Collections.Generic;

namespace IxMilia.Lisp
{
    public abstract class LispFunctionArgument
    {
        public string Name { get; }

        protected LispFunctionArgument(string name)
        {
            Name = name;
        }

        internal static bool TryBuildFunctionArguments(LispObject[] argumentList, out LispFunctionArgument[] arguments, out LispError error)
        {
            arguments = default;
            error = default;

            var argumentListResult = new List<LispFunctionArgument>();
            for (int i = 0; i < argumentList.Length; i++)
            {
                var argumentListValue = argumentList[i];
                if (argumentListValue is LispSymbol symbol)
                {
                    switch (symbol.Value)
                    {
                        case "&optional":
                            i++;
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
                                        argumentListResult.Add(new LispOptionalFunctionArgument(argWithNilDefault.Value, LispNilList.Instance));
                                        break;
                                    case LispList argWithCustomDefault:
                                        if (argWithCustomDefault.Length == 2 &&
                                            argWithCustomDefault.Value is LispSymbol defaultArgName &&
                                            argWithCustomDefault.Next is LispList defaultValue)
                                        {
                                            argumentListResult.Add(new LispOptionalFunctionArgument(defaultArgName.Value, defaultValue.Value));
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
                                argumentListResult.Add(new LispRestFunctionArgument(restName.Value));
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
                            argumentListResult.Add(new LispRegularFunctionArgument(symbol.Value));
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

            arguments = argumentListResult.ToArray();
            return true;
        }
    }

    public class LispRegularFunctionArgument : LispFunctionArgument
    {
        internal LispRegularFunctionArgument(string name)
            : base(name)
        {
        }

        public override string ToString()
        {
            return Name;
        }
    }

    public class LispOptionalFunctionArgument : LispFunctionArgument
    {
        public LispObject DefaultValue { get; }

        internal LispOptionalFunctionArgument(string name, LispObject defaultValue)
            : base(name)
        {
            DefaultValue = defaultValue;
        }

        public override string ToString()
        {
            return $"&optional ({Name} {DefaultValue})";
        }
    }

    public class LispRestFunctionArgument : LispFunctionArgument
    {
        internal LispRestFunctionArgument(string name)
            : base(name)
        {
        }

        public override string ToString()
        {
            return $"&rest {Name}";
        }
    }
}
