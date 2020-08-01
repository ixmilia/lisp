using System;
using System.Collections.Generic;
using System.Linq;

namespace IxMilia.Lisp
{
    public class LispDefaultContext
    {
        [LispMacro("defmacro")]
        public LispObject DefineMacro(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate arg types and count
            var macroNameSymbol = (LispSymbol)args[0];
            var macroName = macroNameSymbol.Value;
            var macroArgs = ((LispList)args[1]).ToList().Cast<LispSymbol>().Select(s => s.Value);
            // TODO: allow docstring
            var macroBody = args.Skip(2);
            var macro = new LispCodeMacro(macroName, macroArgs, macroBody)
            {
                Line = macroNameSymbol.Line,
                Column = macroNameSymbol.Column
            };
            frame.SetValueInParentScope(macroName, macro);
            return macro;
        }

        [LispMacro("defun")]
        public LispObject DefineFunction(LispStackFrame frame, LispObject[] args)
        {
            // TODO: properly validate types and arg counts
            var name = ((LispSymbol)args[0]).Value;
            var functionArgs = ((LispList)args[1]).ToList().Cast<LispSymbol>().Select(s => s.Value);
            var commands = args.Skip(2).ToList();
            string documentation = null;
            if (commands[0] is LispString str)
            {
                documentation = str.Value;
                commands.RemoveAt(0);
            }

            var function = new LispCodeFunction(name, documentation, functionArgs, commands);
            frame.SetValueInParentScope(name, function);
            return frame.Nil;
        }

        [LispMacro("defvar")]
        public LispObject DefineVariable(LispStackFrame frame, LispObject[] args)
        {
            // TODO: properly validage single symbol argument
            var symbol = (LispSymbol)args[0];
            var name = symbol.Value;
            frame.SetValueInParentScope(name, symbol);
            return symbol;
        }

        [LispMacro("let")]
        [LispMacro("let*")]
        public LispObject Let(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate arguments
            var values = ((LispList)args[0]).ToList();
            var body = args[1];
            foreach (var valuePair in values)
            {
                // TODO: validate shape
                var valuePairList = (LispList)valuePair;
                var varName = ((LispSymbol)valuePairList.Value).Value;
                var varRawValue = ((LispList)valuePairList.Next).Value;
                var varValue = frame.Eval(varRawValue);
                if (varValue is LispError error)
                {
                    return error;
                }

                frame.SetValue(varName, varValue);
            }

            var result = frame.Eval(body);
            return result;
        }

        [LispFunction("eval")]
        public LispObject Eval(LispStackFrame frame, LispObject[] args)
        {
            if (args.Length == 1)
            {
                return frame.Eval(args[0]);
            }
            else
            {
                return new LispError("Expected single argument");
            }
        }

        [LispMacro("setf")]
        [LispMacro("setq")]
        public LispObject SetValue(LispStackFrame frame, LispObject[] args)
        {
            // TODO: properly validate types
            LispObject last = frame.Nil;
            for (int i = 0; i < args.Length - 1; i += 2)
            {
                var name = ((LispSymbol)args[i]).Value;
                var value = frame.Eval(args[i + 1]);
                frame.SetValueInParentScope(name, value);
                last = value;
            }

            return last;
        }

        [LispFunction("numberp")]
        public LispObject NumberP(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispInteger _:
                case LispFloat _:
                case LispRatio _:
                    return frame.T;
                default:
                    return frame.Nil;
            }
        }

        [LispFunction("keywordp")]
        public LispObject KeywordP(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispKeyword _:
                    return frame.T;
                default:
                    return frame.Nil;
            }
        }

        [LispFunction("symbolp")]
        public LispObject SymbolP(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispKeyword _:
                case LispSymbol _:
                    return frame.T;
                default:
                    return frame.Nil;
            }
        }

        [LispFunction("zerop")]
        public LispObject ZeroP(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispInteger num when num.IsZero:
                    return frame.T;
                case LispFloat num when num.IsZero:
                    return frame.T;
                default:
                    return frame.Nil;
            }
        }

        [LispFunction("evenp")]
        public LispObject EvenP(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispInteger num when num.IsEven:
                    return frame.T;
                case LispFloat num when num.IsEven:
                    return frame.T;
                default:
                    return frame.Nil;
            }
        }

        [LispFunction("oddp")]
        public LispObject OddP(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispInteger num when num.IsOdd:
                    return frame.T;
                case LispFloat num when num.IsOdd:
                    return frame.T;
                default:
                    return frame.Nil;
            }
        }

        [LispFunction("listp")]
        public LispObject ListP(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate single argument
            return args[0] is LispList
                ? frame.T
                : frame.Nil;
        }

        [LispFunction("consp")]
        public LispObject ConsP(LispStackFrame frame, LispObject[] args)
        {
            return args[0] is LispList list && !list.IsNil
                ? frame.T
                : frame.Nil;
        }

        [LispMacro("quote")]
        public LispObject Quote(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument count
            return args[0];
        }

        [LispFunction("cons")]
        public LispObject Cons(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate arguments
            return new LispList(args[0], args[1]);
        }

        [LispFunction("list")]
        public LispObject List(LispStackFrame frame, LispObject[] args)
        {
            return LispList.FromEnumerable(args);
        }

        [LispFunction("length")]
        public LispObject Length(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate single argument
            if (args[0] is LispList list)
            {
                return new LispInteger(list.Length);
            }
            else
            {
                return new LispError("Expected a list");
            }
        }

        [LispFunction("car")]
        [LispFunction("first")]
        public LispObject First(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate single argument
            if (args[0] is LispList list)
            {
                return list.Value;
            }
            else
            {
                return new LispError($"Expected a list, found {args[0]}");
            }
        }

        [LispFunction("cdr")]
        [LispFunction("rest")]
        public LispObject Rest(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate single argument
            if (args[0] is LispList list)
            {
                return list.Next;
            }
            else
            {
                return new LispError($"Expected a list, found {args[0]}");
            }
        }

        [LispFunction("append")]
        public LispObject Append(LispStackFrame frame, LispObject[] args)
        {
            if (args.Length != 2)
            {
                return new LispError("Expected exactly 2 arguments");
            }

            if (args[0] is LispList list)
            {
                var items = list.ToList();
                var last = args[1];
                foreach (var item in items.Reverse())
                {
                    var next = new LispList(item, last);
                    last = next;
                }

                return last;
            }
            else
            {
                return new LispError("First argument must be a list");
            }
        }

        [LispFunction("reverse")]
        public LispObject Reverse(LispStackFrame frame, LispObject[] args)
        {
            if (args.Length == 1 && args[0] is LispList list)
            {
                var values = list.ToList().Reverse();
                var result = LispList.FromEnumerable(values);
                return result;
            }
            else
            {
                return new LispError("Expected a single list");
            }
        }

        [LispFunction("intersection")]
        public LispObject Intersection(LispStackFrame frame, LispObject[] args)
        {
            if (args.Length == 2 &&
                args[0] is LispList a &&
                args[1] is LispList b)
            {
                var valuesInOne = new HashSet<LispObject>(a.ToList(), LispObject.Comparer);
                var finalSet = b.ToList().Where(i => valuesInOne.Contains(i, LispObject.Comparer));
                return LispList.FromEnumerable(finalSet);
            }
            else
            {
                return new LispError("Expected 2 lists");
            }
        }

        [LispFunction("union")]
        public LispObject Union(LispStackFrame frame, LispObject[] args)
        {
            if (args.Length == 2 &&
                args[0] is LispList a &&
                args[1] is LispList b)
            {
                var finalSet = new HashSet<LispObject>(a.ToList(), LispObject.Comparer);
                foreach (var item in b.ToList())
                {
                    finalSet.Add(item);
                }

                return LispList.FromEnumerable(finalSet);
            }
            else
            {
                return new LispError("Expected 2 lists");
            }
        }

        [LispFunction("set-difference")]
        public LispObject SetDifference(LispStackFrame frame, LispObject[] args)
        {
            if (args.Length == 2 &&
                args[0] is LispList a &&
                args[1] is LispList b)
            {
                var finalSet = new HashSet<LispObject>(a.ToList(), LispObject.Comparer);
                foreach (var item in b.ToList())
                {
                    finalSet.Remove(item);
                }

                return LispList.FromEnumerable(finalSet);
            }
            else
            {
                return new LispError("Expected 2 lists");
            }
        }

        [LispFunction("set-exclusive-or")]
        public LispObject SetExclusiveOr(LispStackFrame frame, LispObject[] args)
        {
            if (args.Length == 2 &&
               args[0] is LispList a &&
               args[1] is LispList b)
            {
                var inA = new HashSet<LispObject>(a.ToList(), LispObject.Comparer);
                var inB = new HashSet<LispObject>(b.ToList(), LispObject.Comparer);
                var inBoth = inA.Intersect(inB, LispObject.Comparer);
                var finalSet = inA.Union(inB, LispObject.Comparer).Except(inBoth, LispObject.Comparer);
                return LispList.FromEnumerable(finalSet);
            }
            else
            {
                return new LispError("Expected 2 lists");
            }
        }

        [LispFunction("remove")]
        public LispObject Remove(LispStackFrame frame, LispObject[] args)
        {
            // TOOD: validate arguments
            var key = args[0];
            var list = ((LispList)args[1]).ToList();
            var result = new List<LispObject>();
            var count = GetKeywordArgument(args.Skip(2), ":count");
            var limit = count is LispInteger number
                ? number.Value
                : list.Count;
            var fromEndArg = GetKeywordArgument(args.Skip(2), ":from-end");
            var fromEnd = !fromEndArg.Equals(frame.Nil);
            if (fromEnd)
            {
                list = list.Reverse().ToList();
            }

            var removeCount = 0;
            foreach (var item in list)
            {
                if (removeCount < limit && key.Equals(item))
                {
                    removeCount++;
                }
                else
                {
                    result.Add(item);
                }
            }

            if (fromEnd)
            {
                result.Reverse();
            }

            return LispList.FromEnumerable(result);
        }

        [LispFunction("remove-duplicates")]
        public LispObject RemoveDuplicates(LispStackFrame frame, LispObject[] args)
        {
            if (args.Length == 1 && args[0] is LispList list)
            {
                var finalItems = new List<LispObject>();
                var seenItems = new HashSet<LispObject>();
                foreach (var item in list.ToList())
                {
                    if (seenItems.Add(item))
                    {
                        finalItems.Add(item);
                    }
                }

                return LispList.FromEnumerable(finalItems);
            }
            else
            {
                return new LispError("Expected a list");
            }
        }

        [LispFunction("<")]
        public LispObject LessThan(LispStackFrame frame, LispObject[] args)
        {
            return FoldComparison(frame, args, (a, b) => LispNumber.LessThan(a, b));
        }

        [LispFunction("<=")]
        public LispObject LessThanOrEqual(LispStackFrame frame, LispObject[] args)
        {
            return FoldComparison(frame, args, (a, b) => LispNumber.LessThanOrEqual(a, b));
        }

        [LispFunction(">")]
        public LispObject GreaterThan(LispStackFrame frame, LispObject[] args)
        {
            return FoldComparison(frame, args, (a, b) => LispNumber.GreaterThan(a, b));
        }

        [LispFunction(">=")]
        public LispObject GreaterThanOrEqual(LispStackFrame frame, LispObject[] args)
        {
            return FoldComparison(frame, args, (a, b) => LispNumber.GreaterThanOrEqual(a, b));
        }

        [LispFunction("=")]
        public LispObject NumberEqual(LispStackFrame frame, LispObject[] args)
        {
            return FoldComparison(frame, args, (a, b) => LispNumber.Equal(a, b));
        }

        [LispFunction("equal")]
        public LispObject Equal(LispStackFrame frame, LispObject[] args)
        {
            return FoldObj(frame, args, (a, b) => a.Equals(b));
        }

        [LispFunction("equalp")]
        public LispObject EqualP(LispStackFrame frame, LispObject[] args)
        {
            return FoldObj(frame, args, (a, b) =>
            {
                if (a is LispString sa && b is LispString sb)
                {
                    return string.Compare(sa.Value, sb.Value, StringComparison.OrdinalIgnoreCase) == 0;
                }

                return a.Equals(b);
            });
        }

        [LispFunction("eq")]
        public LispObject Eq(LispStackFrame frame, LispObject[] args)
        {
            return FoldObj(frame, args, (a, b) => ReferenceEquals(a, b));
        }

        [LispFunction("eql")]
        public LispObject Eql(LispStackFrame frame, LispObject[] args)
        {
            return FoldObj(frame, args, (a, b) =>
            {
                if (a is LispInteger ia && b is LispInteger ib && ia.Value == ib.Value)
                {
                    return true;
                }

                if (a is LispFloat fa && b is LispFloat fb && fa.Value == fb.Value)
                {
                    return true;
                }

                if (a is LispRatio ra && b is LispRatio rb && ra == rb)
                {
                    return true;
                }

                if (a is LispString sa && b is LispString sb && sa == sb)
                {
                    return true;
                }

                if (a is LispKeyword ka && b is LispKeyword kb && ka.Keyword == kb.Keyword)
                {
                    return true;
                }

                if (a is LispSymbol ssa && b is LispSymbol ssb && ssa.Value == ssb.Value)
                {
                    return true;
                }

                return ReferenceEquals(a, b);
            });
        }

        [LispFunction("!=")]
        [LispFunction("<>")]
        public LispObject NotEqual(LispStackFrame frame, LispObject[] args)
        {
            return FoldObj(frame, args, (a, b) => !a.Equals(b));
        }

        [LispMacro("and")]
        public LispObject And(LispStackFrame frame, LispObject[] args)
        {
            return FoldBoolean(frame, args, true, false, (a, b) => a && b);
        }

        [LispMacro("or")]
        public LispObject Or(LispStackFrame frame, LispObject[] args)
        {
            return FoldBoolean(frame, args, true, true, (a, b) => a || b);
        }

        [LispMacro("cond")]
        public LispObject Cond(LispStackFrame frame, LispObject[] args)
        {
            foreach (var arg in args)
            {
                if (arg is LispList list && list.Length == 2)
                {
                    var values = list.ToList();
                    var predicate = frame.Eval(values[0]);
                    switch (predicate)
                    {
                        case LispError _:
                            return predicate;
                        case LispNilList _:
                            break;
                        default:
                            return new LispTailCall(values[1]);
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

            return frame.Nil;
        }

        [LispFunction("abs")]
        public LispObject Abs(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate arguments
            switch (args[0])
            {
                case LispInteger i:
                    return new LispInteger(Math.Abs(i.Value));
                case LispFloat f:
                    return new LispFloat(Math.Abs(f.Value));
                case LispRatio r:
                    return new LispRatio(Math.Abs(r.Numerator), Math.Abs(r.Denominator));
                default:
                    return new LispError($"Expected {nameof(LispInteger)} or {nameof(LispFloat)} but found {args[0].GetType().Name} with value {args[0]}");
            }
        }

        [LispFunction("sqrt")]
        public LispObject Sqrt(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate arguments
            switch (args[0])
            {
                case LispInteger i:
                    return new LispFloat(Math.Sqrt(i.Value));
                case LispFloat f:
                    return new LispFloat(Math.Sqrt(f.Value));
                case LispRatio r:
                    return new LispFloat(Math.Sqrt(((LispFloat)r).Value));
                default:
                    return new LispError($"Expected {nameof(LispInteger)} or {nameof(LispFloat)} but found {args[0].GetType().Name} with value {args[0]}");
            }
        }

        [LispFunction("+")]
        public LispObject Add(LispStackFrame frame, LispObject[] args)
        {
            return FoldNumber(args, LispInteger.Zero, (a, b) => LispNumber.Add(a, b));
        }

        [LispFunction("-")]
        public LispObject Subtract(LispStackFrame frame, LispObject[] args)
        {
            if (args.Length == 1)
            {
                // simple negation
                var value = args[0];
                switch (value)
                {
                    case LispInteger num:
                        return new LispInteger(num.Value * -1);
                    case LispFloat num:
                        return new LispFloat(num.Value * -1.0);
                    case LispRatio num:
                        return new LispRatio(num.Numerator * -1, num.Denominator).Reduce();
                    default:
                        return new LispError($"Expected type number but found {value.GetType()}");
                }
            }
            else
            {
                return FoldNumber(args, LispInteger.Zero, (a, b) => LispNumber.Sub(a, b), useFirstAsInit: true);
            }
        }

        [LispFunction("*")]
        public LispObject Multiply(LispStackFrame frame, LispObject[] args)
        {
            return FoldNumber(args, LispInteger.One, (a, b) => LispNumber.Mul(a, b));
        }

        [LispFunction("/")]
        public LispObject Divide(LispStackFrame frame, LispObject[] args)
        {
            return FoldNumber(args, LispInteger.One, (a, b) => LispNumber.Div(a, b), useFirstAsInit: true);
        }

        private static LispObject FoldNumber(LispObject[] args, LispNumber init, Func<LispNumber, LispNumber, LispNumber> operation, bool useFirstAsInit = false)
        {
            if (args.Length == 0)
            {
                return new LispError("Missing arguments");
            }

            int skip = 0;
            if (useFirstAsInit)
            {
                skip = 1;
                switch (args[0])
                {
                    case LispInteger i:
                        init = i;
                        break;
                    case LispFloat f:
                        init = f;
                        break;
                    case LispRatio r:
                        init = r;
                        break;
                    default:
                        return new LispError($"Expected number, found {args[0].GetType().Name} with value {args[0]}");
                }
            }

            var result = init;
            foreach (var value in args.Skip(skip))
            {
                switch (value)
                {
                    case LispInteger i:
                        result = operation(result, i);
                        break;
                    case LispFloat f:
                        result = operation(result, f);
                        break;
                    case LispRatio r:
                        result = operation(result, r);
                        break;
                    default:
                        return new LispError($"Expected number, found {args[0].GetType().Name} with value {args[0]}");
                }
            }

            return result;
        }

        private static LispObject FoldBoolean(LispStackFrame frame, LispObject[] args, bool init, bool shortCircuitValue, Func<bool, bool, bool> operation)
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
                    var evaluated = frame.Eval(value);
                    if (evaluated is LispError)
                    {
                        return evaluated;
                    }
                    // TODO: non zero
                    var next = evaluated.Equals(frame.Nil) ? false : true;
                    if (next == shortCircuitValue)
                    {
                        collected = shortCircuitValue;
                        goto done;
                    }
                    collected = operation(collected, next);
                }
            done:
                result = collected ? frame.T : frame.Nil;
            }

            return result;
        }

        private static LispObject FoldObj(LispStackFrame frame, LispObject[] args, Func<LispObject, LispObject, bool> operation)
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
                    return frame.Nil;
                }
            }

            return frame.T;
        }

        private static LispObject FoldComparison(LispStackFrame frame, LispObject[] args, Func<LispNumber, LispNumber, bool> operation)
        {
            if (args.Length < 2)
            {
                return new LispError("At least 2 arguments needed");
            }

            LispNumber lastValue;
            switch (args[0])
            {
                case LispInteger i:
                    lastValue = i;
                    break;
                case LispFloat f:
                    lastValue = f;
                    break;
                case LispRatio r:
                    lastValue = r;
                    break;
                default:
                    return new LispError($"Expected number, got {args[0].GetType().Name} with value {args[0]}");
            }

            foreach (var value in args.Skip(1))
            {
                LispNumber nextValue;
                switch (value)
                {
                    case LispInteger i:
                        nextValue = i;
                        break;
                    case LispFloat f:
                        nextValue = f;
                        break;
                    case LispRatio r:
                        nextValue = r;
                        break;
                    default:
                        return new LispError($"Expected number, got {value.GetType().Name} with value {value}");
                }

                var result = operation(lastValue, nextValue);
                if (!result)
                {
                    return frame.Nil;
                }

                lastValue = nextValue;
            }

            return frame.T;
        }

        private static LispObject GetKeywordArgument(IEnumerable<LispObject> args, string keyword)
        {
            LispObject result = LispNilList.Instance;
            var enumerator = args.GetEnumerator();
            while (enumerator.MoveNext())
            {
                if (enumerator.Current is LispKeyword keywordSpecifier && keywordSpecifier.Keyword == keyword)
                {
                    if (enumerator.MoveNext())
                    {
                        result = enumerator.Current;
                    }

                    break;
                }
            }

            return result;
        }
    }
}
