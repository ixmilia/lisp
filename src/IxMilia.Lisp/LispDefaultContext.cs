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
            var macroArgs = ((LispList)args[1]).ToList().Cast<LispSymbol>().Select(s => s.Value);
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
            var functionArgs = ((LispList)args[1]).ToList().Cast<LispSymbol>().Select(s => s.Value);
            var commands = args.Skip(2);
            var function = new LispFunction(name, functionArgs, commands);
            host.SetValue(name, function);
            return host.Nil;
        }

        [LispFunction("eval")]
        public LispObject Eval(LispHost host, LispObject[] args)
        {
            if (args.Length == 1)
            {
                return host.Eval(args[0]);
            }
            else
            {
                return new LispError("Expected single argument");
            }
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

        [LispFunction("listp")]
        public LispObject ListP(LispHost host, LispObject[] args)
        {
            // TODO: validate single argument
            return args[0] is LispList
                ? host.T
                : host.Nil;
        }

        [LispFunction("consp")]
        public LispObject ConsP(LispHost host, LispObject[] args)
        {
            return args[0] is LispList list && !list.IsNil
                ? host.T
                : host.Nil;
        }

        [LispMacro("quote")]
        public LispObject Quote(LispHost host, LispObject[] args)
        {
            // TODO: validate argument count
            return args[0];
        }

        [LispFunction("cons")]
        public LispObject Cons(LispHost host, LispObject[] args)
        {
            // TODO: validate arguments
            return new LispList(args[0], args[1]);
        }

        [LispFunction("list")]
        public LispObject List(LispHost host, LispObject[] args)
        {
            return LispList.FromEnumerable(args);
        }

        [LispFunction("length")]
        public LispObject Length(LispHost host, LispObject[] args)
        {
            // TODO: validate single argument
            if (args[0] is LispList list)
            {
                return new LispNumber(list.Length);
            }
            else
            {
                return new LispError("Expected a list");
            }
        }

        [LispFunction("car")]
        [LispFunction("first")]
        public LispObject First(LispHost host, LispObject[] args)
        {
            // TODO: validate single argument
            if (args[0] is LispList list)
            {
                return list.Value;
            }
            else
            {
                return new LispError("Expected a list");
            }
        }

        [LispFunction("cdr")]
        [LispFunction("rest")]
        public LispObject Rest(LispHost host, LispObject[] args)
        {
            // TODO: validate single argument
            if (args[0] is LispList list)
            {
                return list.Next;
            }
            else
            {
                return new LispError("Expected a list");
            }
        }

        [LispFunction("<")]
        public LispObject LessThan(LispHost host, LispObject[] args)
        {
            return FoldComparison(host, args, (a, b) => a < b);
        }

        [LispFunction("<=")]
        public LispObject LessThanOrEqual(LispHost host, LispObject[] args)
        {
            return FoldComparison(host, args, (a, b) => a <= b);
        }

        [LispFunction(">")]
        public LispObject GreaterThan(LispHost host, LispObject[] args)
        {
            return FoldComparison(host, args, (a, b) => a > b);
        }

        [LispFunction(">=")]
        public LispObject GreaterThanOrEqual(LispHost host, LispObject[] args)
        {
            return FoldComparison(host, args, (a, b) => a >= b);
        }

        [LispFunction("=")]
        [LispFunction("equal")]
        public LispObject Equal(LispHost host, LispObject[] args)
        {
            return FoldObj(host, args, (a, b) => a.Equals(b));
        }

        [LispFunction("!=")]
        [LispFunction("<>")]
        public LispObject NotEqual(LispHost host, LispObject[] args)
        {
            return FoldObj(host, args, (a, b) => !a.Equals(b));
        }

        [LispMacro("and")]
        public LispObject And(LispHost host, LispObject[] args)
        {
            return FoldBoolean(host, args, true, false, (a, b) => a && b);
        }

        [LispMacro("or")]
        public LispObject Or(LispHost host, LispObject[] args)
        {
            return FoldBoolean(host, args, true, true, (a, b) => a || b);
        }

        [LispMacro("cond")]
        public LispObject Cond(LispHost host, LispObject[] args)
        {
            foreach (var arg in args)
            {
                if (arg is LispList list && list.Length == 2)
                {
                    var values = list.ToList();
                    var predicate = host.Eval(values[0]);
                    switch (predicate)
                    {
                        case LispError error:
                            return predicate;
                        case LispNilList _:
                            break;
                        default:
                            return host.Eval(values[1]);
                    }
                }
                else
                {
                    return new LispError("Expected list of length 2")
                    {
                        Line = arg.Line,
                        Column = arg.Column
                    };
                }
            }

            return host.Nil;
        }

        [LispFunction("+")]
        public LispObject Add(LispHost host, LispObject[] args)
        {
            return FoldNumber(args, 0.0, (a, b) => a + b);
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
                    case LispNumber num:
                        return new LispNumber(num.Value * -1);
                    default:
                        return new LispError($"Expected type number but found {value.GetType()}");
                }
            }
            else
            {
                return FoldNumber(args, 0.0, (a, b) => a - b, useFirstAsInit: true);
            }
        }

        [LispFunction("*")]
        public LispObject Multiply(LispHost host, LispObject[] args)
        {
            return FoldNumber(args, 1.0, (a, b) => a * b);
        }

        [LispFunction("/")]
        public LispObject Divide(LispHost host, LispObject[] args)
        {
            return FoldNumber(args, 1.0, (a, b) => a / b, useFirstAsInit: true);
        }

        private static LispObject FoldNumber(LispObject[] args, double init, Func<double, double, double> operation, bool useFirstAsInit = false)
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
                    case LispNumber num:
                        result = operation(result, num.Value);
                        break;
                    default:
                        return new LispError($"Expected type number but found {value.GetType()}");
                }
            }

            return new LispNumber(result);
        }

        private static LispObject FoldBoolean(LispHost host, LispObject[] args, bool init, bool shortCircuitValue, Func<bool, bool, bool> operation)
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
                    var evaluated = host.Eval(value);
                    if (evaluated is LispError)
                    {
                        return evaluated;
                    }
                    // TODO: non zero
                    var next = evaluated.Equals(host.Nil) ? false : true;
                    if (next == shortCircuitValue)
                    {
                        collected = shortCircuitValue;
                        goto done;
                    }
                    collected = operation(collected, next);
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

        private static LispObject FoldComparison(LispHost host, LispObject[] args, Func<double, double, bool> operation)
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
                default:
                    return new LispError($"Expected type number but found {value.GetType()}");
            }

            foreach (var arg in args.Skip(1))
            {
                value = arg;
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
                    default:
                        return new LispError($"Expected type number but found {value.GetType()}");
                }
            }

            return host.T;
        }
    }
}
