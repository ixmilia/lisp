using System;
using System.Linq;

namespace IxMilia.Lisp
{
    public class LispDefaultContext
    {
        [LispMacro("defmacro")]
        public LispObject DefineMacro(LispHost host, LispObject[] args)
        {
            // TODO: validate arg types and count
            var macroNameSymbol = (LispSymbol)args[0];
            var macroName = macroNameSymbol.Value;
            var macroArgs = ((LispList)args[1]).Value.Cast<LispSymbol>().Select(s => s.Value);
            // TODO: allow docstring
            var macroBody = args.Skip(2);
            var macro = new LispMacro(macroName, macroArgs, macroBody)
            {
                Line = macroNameSymbol.Line,
                Column = macroNameSymbol.Column
            };
            host.SetValue(macroName, macro);
            return macro;
        }

        [LispMacro("defun")]
        public LispObject DefineFunction(LispHost host, LispObject[] args)
        {
            // TODO: properly validate types and arg counts
            var name = ((LispSymbol)args[0]).Value;
            var functionArgs = ((LispList)args[1]).Value.Cast<LispSymbol>().Select(a => a.Value);
            var commands = args.Skip(2);
            var function = new LispFunction(name, functionArgs, commands);
            host.SetValue(name, function);
            return host.Nil;
        }

        [LispMacro("setq")]
        public LispObject SetValue(LispHost host, LispObject[] args)
        {
            // TODO: properly validate types
            LispObject last = host.Nil;
            for (int i = 0; i < args.Length - 1; i += 2)
            {
                var name = ((LispSymbol)args[i]).Value;
                var value = host.Eval(args[i + 1]);
                host.SetValue(name, value);
                last = value;
            }

            return last;
        }

        [LispFunction("numberp")]
        public LispObject NumberP(LispHost host, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispNumber _:
                    return host.T;
                default:
                    return host.Nil;
            }
        }

        [LispFunction("symbolp")]
        public LispObject SymbolP(LispHost host, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispSymbol _:
                    return host.T;
                default:
                    return host.Nil;
            }
        }

        [LispFunction("zerop")]
        public LispObject ZeroP(LispHost host, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispNumber num when num.IsZero:
                    return host.T;
                default:
                    return host.Nil;
            }
        }

        [LispFunction("evenp")]
        public LispObject EvenP(LispHost host, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispNumber num when num.IsEven:
                    return host.T;
                default:
                    return host.Nil;
            }
        }

        [LispFunction("oddp")]
        public LispObject OddP(LispHost host, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispNumber num when num.IsOdd:
                    return host.T;
                default:
                    return host.Nil;
            }
        }

        [LispFunction("not")]
        public LispObject Not(LispHost host, LispObject[] args)
        {
            // TODO: validate argument count
            return host.Nil.Equals(args[0])
                ? host.T
                : host.Nil;
        }

        [LispFunction("<")]
        public LispObject LessThan(LispHost host, LispObject[] args)
        {
            return Fold(host, args, (a, b) => a < b);
        }

        [LispFunction("<=")]
        public LispObject LessThanOrEqual(LispHost host, LispObject[] args)
        {
            return Fold(host, args, (a, b) => a <= b);
        }

        [LispFunction(">")]
        public LispObject GreaterThan(LispHost host, LispObject[] args)
        {
            return Fold(host, args, (a, b) => a > b);
        }

        [LispFunction(">=")]
        public LispObject GreaterThanOrEqual(LispHost host, LispObject[] args)
        {
            return Fold(host, args, (a, b) => a >= b);
        }

        [LispFunction("=")]
        public LispObject Equal(LispHost host, LispObject[] args)
        {
            return FoldObj(host, args, (a, b) => a.Equals(b));
        }

        [LispFunction("!=")]
        public LispObject NotEqual(LispHost host, LispObject[] args)
        {
            return Fold(host, args, (a, b) => a != b);
        }

        [LispMacro("&&")]
        public LispObject And(LispHost host, LispObject[] args)
        {
            return Fold(host, args, true, false, (a, b) => a && b);
        }

        [LispMacro("||")]
        public LispObject Or(LispHost host, LispObject[] args)
        {
            return Fold(host, args, true, true, (a, b) => a || b);
        }

        [LispMacro("if")]
        public LispObject If(LispHost host, LispObject[] args)
        {
            LispObject result;
            if (args.Length != 3)
            {
                result = new LispError("Expected 3 arguments");
            }
            else
            {
                var condition = host.Eval(args[0]);
                var resultExpressions = condition.Equals(host.Nil)
                    ? (LispList)args[2] // nil means follow the false path
                    : (LispList)args[1]; // everything else is true
                                         // TODO: numerical 0 should probably follow the false path
                result = host.Eval(resultExpressions.Value);
            }

            return result;
        }

        [LispFunction("+")]
        public LispObject Add(LispHost host, LispObject[] args)
        {
            return Fold(args, 0.0, (a, b) => a + b);
        }

        [LispFunction("-")]
        public LispObject Subtract(LispHost host, LispObject[] args)
        {
            if (args.Length == 1)
            {
                // simple negation
                var value = args[0];
                switch (value)
                {
                    case LispError error:
                        return error;
                    case LispNumber num:
                        return new LispNumber(num.Value * -1);
                    default:
                        return new LispError($"Expected type number but found {value.GetType()}");
                }
            }
            else
            {
                return Fold(args, 0.0, (a, b) => a - b, useFirstAsInit: true);
            }
        }

        [LispFunction("*")]
        public LispObject Multiply(LispHost host, LispObject[] args)
        {
            return Fold(args, 1.0, (a, b) => a * b);
        }

        [LispFunction("/")]
        public LispObject Divide(LispHost host, LispObject[] args)
        {
            return Fold(args, 1.0, (a, b) => a / b, useFirstAsInit: true);
        }

        private static LispObject Fold(LispObject[] args, double init, Func<double, double, double> operation, bool useFirstAsInit = false)
        {
            if (args.Length == 0)
            {
                return new LispError("Missing arguments");
            }

            double result;
            int skip;
            if (useFirstAsInit)
            {
                skip = 1;
                var first = args[0];
                switch (first)
                {
                    case LispError error:
                        return error;
                    case LispNumber num:
                        result = num.Value;
                        break;
                    default:
                        return new LispError($"Expected type number but found {first.GetType()}");
                }
            }
            else
            {
                skip = 0;
                result = init;
            }

            foreach (var value in args.Skip(skip))
            {
                switch (value)
                {
                    case LispError error:
                        return error;
                    case LispNumber num:
                        result = operation(result, num.Value);
                        break;
                    default:
                        return new LispError($"Expected type number but found {value.GetType()}");
                }
            }

            return new LispNumber(result);
        }

        private static LispObject Fold(LispHost host, LispObject[] args, bool init, bool shortCircuitValue, Func<bool, bool, bool> operation)
        {
            LispObject result;
            if (args.Length == 0)
            {
                result = new LispError("Missing arguments");
            }
            else
            {
                var collected = init;
                foreach (var value in args)
                {
                    switch (value)
                    {
                        case LispError error:
                            result = error;
                            goto done;
                        default:
                            // TODO: non zero
                            var nextValue = host.Eval(value);
                            var next = nextValue.Equals(host.Nil) ? false : true;
                            if (next == shortCircuitValue)
                            {
                                collected = shortCircuitValue;
                                goto done;
                            }
                            collected = operation(collected, next);
                            break;
                    }
                }
            done:
                result = collected ? host.T : host.Nil;
            }

            return result;
        }

        private static LispObject FoldObj(LispHost host, LispObject[] args, Func<LispObject, LispObject, bool> operation)
        {
            if (args.Length < 2)
            {
                return new LispError("At least 2 arguments needed");
            }

            if (args[0] is LispError)
            {
                return args[0];
            }

            var result = true;
            for (int i = 0; i < args.Length - 1; i++)
            {
                result &= operation(args[i], args[i + 1]);
                if (!result)
                {
                    return host.Nil;
                }
            }

            return host.T;
        }

        private static LispObject Fold(LispHost host, LispObject[] args, Func<double, double, bool> operation)
        {
            if (args.Length < 2)
            {
                return new LispError("At least 2 arguments needed");
            }

            var value = args[0];
            double lastValue;
            switch (value)
            {
                case LispNumber num:
                    lastValue = num.Value;
                    break;
                case LispError error:
                    return error;
                default:
                    return new LispError($"Expected type number but found {value.GetType()}");
            }

            foreach (var arg in args.Skip(1))
            {
                value = host.Eval(arg);
                switch (value)
                {
                    case LispNumber num:
                        var result = operation(lastValue, num.Value);
                        if (!result)
                        {
                            return host.Nil;
                        }
                        lastValue = num.Value;
                        break;
                    case LispError error:
                        return error;
                    default:
                        return new LispError($"Expected type number but found {value.GetType()}");
                }
            }

            return host.T;
        }
    }
}
