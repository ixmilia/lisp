using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;
using IxMilia.Lisp.Tokens;

namespace IxMilia.Lisp
{
    public abstract class LispObject
    {
        public static IEqualityComparer<LispObject> Comparer { get; } = new LispObjectEqualityComparer();

        public int Line { get; internal set; }
        public int Column { get; internal set; }

        private class LispObjectEqualityComparer : IEqualityComparer<LispObject>
        {
            public bool Equals(LispObject x, LispObject y)
            {
                return x.Equals(y);
            }

            public int GetHashCode(LispObject obj)
            {
                return obj.GetHashCode();
            }
        }
    }

    public class LispQuotedObject : LispObject
    {
        public LispObject Value { get; }

        public LispQuotedObject(LispObject value)
        {
            Value = value;
        }

        public override string ToString()
        {
            return string.Concat("'", Value.ToString());
        }

        public static bool operator ==(LispQuotedObject a, LispQuotedObject b)
        {
            if (a is null && b is null)
            {
                return true;
            }
            else if (a is null || b is null)
            {
                return false;
            }
            else
            {
                return a.Value.Equals(b.Value);
            }
        }

        public static bool operator !=(LispQuotedObject a, LispQuotedObject b)
        {
            return !(a == b);
        }

        public override bool Equals(object obj)
        {
            return obj is LispQuotedObject && this == (LispQuotedObject)obj;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }
    }

    public class LispError : LispObject
    {
        public string Message { get; }
        public LispStackFrame StackFrame { get; internal set; }

        public LispError(string message)
            : this(message, null)
        {
        }

        internal LispError(string message, LispStackFrame stackFrame)
        {
            Message = message;
            StackFrame = stackFrame;
        }

        internal void TryApplyStackFrame(LispStackFrame frame)
        {
            if (StackFrame == null)
            {
                StackFrame = frame;
            }
        }

        public override string ToString()
        {
            var frame = StackFrame == null ? string.Empty : $":\n{StackFrame}";
            return $"{Message}{frame}";
        }
    }

    public class LispSymbol : LispObject
    {
        public string Value { get; set; }

        public LispSymbol(string value)
        {
            Value = value;
        }

        public override string ToString()
        {
            return Value;
        }

        public static bool operator ==(LispSymbol a, LispSymbol b)
        {
            return a?.Value == b?.Value;
        }

        public static bool operator !=(LispSymbol a, LispSymbol b)
        {
            return !(a == b);
        }

        public override bool Equals(object obj)
        {
            return obj is LispSymbol && this == (LispSymbol)obj;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }
    }

    public class LispKeyword : LispObject
    {
        public string Keyword { get; }

        public LispKeyword(string keyword)
        {
            Keyword = keyword;
        }

        public override string ToString()
        {
            return Keyword;
        }
    }

    public enum LispNumberType
    {
        Integer = 0,
        Float = 1,
        Ratio = 2,
    }

    public abstract class LispNumber : LispObject
    {
        public abstract LispNumberType Type { get; }

        internal static LispNumber Add(LispNumber a, LispNumber b)
        {
            switch (a)
            {
                case LispInteger ia:
                    switch (b)
                    {
                        case LispInteger ib:
                            return ia + ib;
                        case LispFloat fb:
                            return ia + fb;
                        case LispRatio rb:
                            return ia + rb;
                        default:
                            throw new InvalidOperationException("Not possible, expected a number.");
                    }
                case LispFloat fa:
                    switch (b)
                    {
                        case LispInteger ib:
                            return fa + ib;
                        case LispFloat fb:
                            return fa + fb;
                        case LispRatio rb:
                            return fa + rb;
                        default:
                            throw new InvalidOperationException("Not possible, expected a number.");
                    }
                case LispRatio ra:
                    switch (b)
                    {
                        case LispInteger ib:
                            return ra + ib;
                        case LispFloat fb:
                            return ra + fb;
                        case LispRatio rb:
                            return ra + rb;
                        default:
                            throw new InvalidOperationException("Not possible, expected a number.");
                    }
                default:
                    throw new InvalidOperationException("Not possible, expected a number.");
            }
        }

        internal static LispNumber Sub(LispNumber a, LispNumber b)
        {
            switch (a)
            {
                case LispInteger ia:
                    switch (b)
                    {
                        case LispInteger ib:
                            return ia - ib;
                        case LispFloat fb:
                            return ia - fb;
                        case LispRatio rb:
                            return ia - rb;
                        default:
                            throw new InvalidOperationException("Not possible, expected a number.");
                    }
                case LispFloat fa:
                    switch (b)
                    {
                        case LispInteger ib:
                            return fa - ib;
                        case LispFloat fb:
                            return fa - fb;
                        case LispRatio rb:
                            return fa - rb;
                        default:
                            throw new InvalidOperationException("Not possible, expected a number.");
                    }
                case LispRatio ra:
                    switch (b)
                    {
                        case LispInteger ib:
                            return ra - ib;
                        case LispFloat fb:
                            return ra - fb;
                        case LispRatio rb:
                            return ra - rb;
                        default:
                            throw new InvalidOperationException("Not possible, expected a number.");
                    }
                default:
                    throw new InvalidOperationException("Not possible, expected a number.");
            }
        }

        internal static LispNumber Mul(LispNumber a, LispNumber b)
        {
            switch (a)
            {
                case LispInteger ia:
                    switch (b)
                    {
                        case LispInteger ib:
                            return ia * ib;
                        case LispFloat fb:
                            return ia * fb;
                        case LispRatio rb:
                            return ia * rb;
                        default:
                            throw new InvalidOperationException("Not possible, expected a number.");
                    }
                case LispFloat fa:
                    switch (b)
                    {
                        case LispInteger ib:
                            return fa * ib;
                        case LispFloat fb:
                            return fa * fb;
                        case LispRatio rb:
                            return fa * rb;
                        default:
                            throw new InvalidOperationException("Not possible, expected a number.");
                    }
                case LispRatio ra:
                    switch (b)
                    {
                        case LispInteger ib:
                            return ra * ib;
                        case LispFloat fb:
                            return ra * fb;
                        case LispRatio rb:
                            return ra * rb;
                        default:
                            throw new InvalidOperationException("Not possible, expected a number.");
                    }
                default:
                    throw new InvalidOperationException("Not possible, expected a number.");
            }
        }

        internal static LispNumber Div(LispNumber a, LispNumber b)
        {
            switch (a)
            {
                case LispInteger ia:
                    switch (b)
                    {
                        case LispInteger ib:
                            return (LispRatio)ia / ib;
                        case LispFloat fb:
                            return ia / fb;
                        case LispRatio rb:
                            return ia / rb;
                        default:
                            throw new InvalidOperationException("Not possible, expected a number.");
                    }
                case LispFloat fa:
                    switch (b)
                    {
                        case LispInteger ib:
                            return fa / ib;
                        case LispFloat fb:
                            return fa / fb;
                        case LispRatio rb:
                            return fa / rb;
                        default:
                            throw new InvalidOperationException("Not possible, expected a number.");
                    }
                case LispRatio ra:
                    switch (b)
                    {
                        case LispInteger ib:
                            return ra / ib;
                        case LispFloat fb:
                            return ra / fb;
                        case LispRatio rb:
                            return ra / rb;
                        default:
                            throw new InvalidOperationException("Not possible, expected a number.");
                    }
                default:
                    throw new InvalidOperationException("Not possible, expected a number.");
            }
        }

        internal static bool Equal(LispNumber a, LispNumber b)
        {
            return DoComparison(a, b, (x, y) => Math.Abs(x - y) < double.Epsilon);
        }

        internal static bool LessThan(LispNumber a, LispNumber b)
        {
            return DoComparison(a, b, (x, y) => x < y);
        }

        internal static bool LessThanOrEqual(LispNumber a, LispNumber b)
        {
            return DoComparison(a, b, (x, y) => x <= y);
        }

        internal static bool GreaterThan(LispNumber a, LispNumber b)
        {
            return DoComparison(a, b, (x, y) => x > y);
        }

        internal static bool GreaterThanOrEqual(LispNumber a, LispNumber b)
        {
            return DoComparison(a, b, (x, y) => x >= y);
        }

        private static bool DoComparison(LispNumber a, LispNumber b, Func<double, double, bool> comparison)
        {
            var da = AsDouble(a);
            var db = AsDouble(b);
            return comparison(da, db);
        }

        private static double AsDouble(LispNumber num)
        {
            switch (num)
            {
                case LispInteger i:
                    return i.Value;
                case LispFloat f:
                    return f.Value;
                case LispRatio r:
                    return (double)r.Numerator / r.Denominator;
                default: throw new InvalidOperationException("Not possible, expected a number.");
            }
        }
    }

    public class LispInteger : LispNumber
    {
        public override LispNumberType Type => LispNumberType.Integer;

        public int Value { get; set; }

        public LispInteger(int value)
        {
            Value = value;
        }

        public bool IsZero => Value == 0;
        public bool IsEven => Value % 2 == 0;
        public bool IsOdd => Value % 2 != 0;

        public override string ToString()
        {
            return Value.ToString();
        }

        public static LispInteger Zero => new LispInteger(0);
        public static LispInteger One => new LispInteger(1);

        public static bool operator ==(LispInteger a, LispInteger b)
        {
            return a?.Value == b?.Value;
        }

        public static bool operator !=(LispInteger a, LispInteger b)
        {
            return !(a == b);
        }

        public static LispInteger operator +(LispInteger a, LispInteger b)
        {
            return new LispInteger(a.Value + b.Value);
        }

        public static LispInteger operator -(LispInteger a, LispInteger b)
        {
            return new LispInteger(a.Value - b.Value);
        }

        public static LispInteger operator *(LispInteger a, LispInteger b)
        {
            return new LispInteger(a.Value * b.Value);
        }

        public static LispInteger operator /(LispInteger a, LispInteger b)
        {
            return new LispInteger(a.Value / b.Value);
        }

        public override bool Equals(object obj)
        {
            return obj is LispInteger && this == (LispInteger)obj;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }
    }

    public class LispFloat : LispNumber
    {
        public override LispNumberType Type => LispNumberType.Float;

        public double Value { get; set; }

        public LispFloat(double value)
        {
            Value = value;
        }

        public bool IsZero => Value == 0.0;
        public bool IsEven => (Value - (int)Value == 0.0) && (int)Value % 2 == 0;
        public bool IsOdd => (Value - (int)Value == 0.0) && (int)Value % 2 != 0;

        public static implicit operator LispFloat(LispInteger i)
        {
            return new LispFloat(i.Value);
        }

        public override string ToString()
        {
            return Value.ToString();
        }

        public static bool operator ==(LispFloat a, LispFloat b)
        {
            return a?.Value == b?.Value;
        }

        public static bool operator !=(LispFloat a, LispFloat b)
        {
            return !(a == b);
        }

        public static LispFloat operator +(LispFloat a, LispFloat b)
        {
            return new LispFloat(a.Value + b.Value);
        }

        public static LispFloat operator -(LispFloat a, LispFloat b)
        {
            return new LispFloat(a.Value - b.Value);
        }

        public static LispFloat operator *(LispFloat a, LispFloat b)
        {
            return new LispFloat(a.Value * b.Value);
        }

        public static LispFloat operator /(LispFloat a, LispFloat b)
        {
            return new LispFloat(a.Value / b.Value);
        }

        public override bool Equals(object obj)
        {
            return obj is LispFloat && this == (LispFloat)obj;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }
    }

    public class LispRatio : LispNumber
    {
        public override LispNumberType Type => LispNumberType.Ratio;

        public int Numerator { get; }
        public int Denominator { get; }

        public LispRatio(int num, int denom)
        {
            Numerator = num;
            Denominator = denom;
        }

        public bool IsZero => Numerator == 0;
        public bool IsEven => false;
        public bool IsOdd => false;

        public LispNumber Reduce()
        {
            Reduce(Numerator, Denominator, out var num, out var denom);

            if (num == 0)
            {
                return LispInteger.Zero;
            }

            if (denom == 1)
            {
                return new LispInteger(num);
            }

            if (Numerator == num && Denominator == denom)
            {
                return this;
            }

            return new LispRatio(num, denom);
        }

        private static void Reduce(int numerator, int denominator, out int finalNum, out int finalDenom)
        {
            var gcd = Gcd(numerator, denominator);
            finalNum = numerator / gcd;
            finalDenom = denominator / gcd;
        }

        private static int Gcd(int a, int b)
        {
        top:
            if (b == 0)
            {
                return a;
            }

            var temp = a % b;
            a = b;
            b = temp;
            goto top;
        }

        public override string ToString()
        {
            return $"{Numerator}/{Denominator}";
        }

        public static implicit operator LispRatio(LispInteger i)
        {
            return new LispRatio(i.Value, 1);
        }

        public static implicit operator LispFloat(LispRatio r)
        {
            return new LispFloat((double)r.Numerator / r.Denominator);
        }

        public static bool operator ==(LispRatio a, LispRatio b)
        {
            return a?.Numerator == b?.Numerator && a?.Denominator == b?.Denominator;
        }

        public static bool operator !=(LispRatio a, LispRatio b)
        {
            return !(a == b);
        }

        public static LispNumber operator +(LispRatio a, LispRatio b)
        {
            return new LispRatio(a.Numerator * b.Denominator + b.Numerator * a.Denominator, a.Denominator * b.Denominator).Reduce();
        }

        public static LispNumber operator -(LispRatio a, LispRatio b)
        {
            return new LispRatio(a.Numerator * b.Denominator - b.Numerator * a.Denominator, a.Denominator * b.Denominator).Reduce();
        }

        public static LispNumber operator *(LispRatio a, LispRatio b)
        {
            return new LispRatio(a.Numerator * b.Numerator, a.Denominator * b.Denominator).Reduce();
        }

        public static LispNumber operator /(LispRatio a, LispRatio b)
        {
            return new LispRatio(a.Numerator * b.Denominator, a.Denominator * b.Numerator).Reduce();
        }

        public override bool Equals(object obj)
        {
            return obj is LispRatio && this == (LispRatio)obj;
        }

        public override int GetHashCode()
        {
            return (Numerator.GetHashCode() << 13) | Denominator.GetHashCode();
        }
    }

    public class LispString : LispObject
    {
        public string Value { get; set; }

        public LispString(string value)
        {
            Value = value;
        }

        public override string ToString()
        {
            return LispStringToken.ToRoundTrippable(Value);
        }

        public static bool operator ==(LispString a, LispString b)
        {
            return a?.Value == b?.Value;
        }

        public static bool operator !=(LispString a, LispString b)
        {
            return !(a == b);
        }

        public override bool Equals(object obj)
        {
            return obj is LispString && this == (LispString)obj;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }
    }

    internal class LispForwardListReference : LispObject
    {
        public LispForwardReferenceToken ForwardReference { get; }
        public LispList List { get; }

        public LispForwardListReference(LispForwardReferenceToken forwardReference, LispList list)
        {
            ForwardReference = forwardReference;
            List = list;
        }
    }

    public class LispList : LispObject
    {
        public virtual LispObject Value { get; }
        public virtual LispObject Next { get; }
        public virtual int Length { get; }
        public virtual bool IsNil { get; }

        public virtual bool IsProperList
        {
            get
            {
                var list = this;
                while (!list.IsNil)
                {
                    if (list.Next is LispList next)
                    {
                        list = next;
                    }
                    else
                    {
                        return false;
                    }
                }

                return true;
            }
        }

        internal LispList()
        {
        }

        public LispList(LispObject value)
            : this(value, LispNilList.Instance)
        {
        }

        public LispList(LispObject value, LispObject next)
        {
            Value = value;
            Next = next;
            IsNil = false;
            if (next is LispList list)
            {
                Length = list.Length + 1;
            }
            else
            {
                Length = 1; // `Value`; improper tail isn't counted
            }
        }

        public static LispList FromItems(params LispObject[] items)
        {
            return FromEnumerable(items);
        }

        public static LispList FromEnumerable(IEnumerable<LispObject> items)
        {
            LispList list = LispNilList.Instance;
            foreach (var item in items.Reverse())
            {
                list = new LispList(item, list);
            }

            return list;
        }

        public static LispList FromItemsImproper(LispObject first, LispObject second, params LispObject[] rest)
        {
            return FromEnumerableImproper(first, second, rest);
        }

        public static LispList FromEnumerableImproper(LispObject first, LispObject second, IEnumerable<LispObject> rest)
        {
            // guaranteed to contain at least 2 items
            var allItems = new[] { first, second }.Concat(rest).ToList();

            var list = new LispList(allItems[allItems.Count - 2], allItems[allItems.Count - 1]);
            for (int i = allItems.Count - 3; i >= 0; i--)
            {
                list = new LispList(allItems[i], list);
            }

            return list;
        }

        public virtual IList<LispObject> ToList()
        {
            var items = new List<LispObject>();
            var head = this;
            while (!head.IsNil)
            {
                items.Add(head.Value);
                if (head.Next is LispList next)
                {
                    head = next;
                }
                else
                {
                    items.Add(head.Next);
                    break;
                }
            }

            return items;
        }

        public override string ToString()
        {
            return $"({Value}{NextToString()}";
        }

        protected virtual string ToStringTail()
        {
            return $" {Value}{NextToString()}";
        }

        private string NextToString()
        {
            string nextString;
            if (Next is LispList list)
            {
                nextString = list.ToStringTail();
            }
            else
            {
                nextString = $" . {Next})";
            }

            return nextString;
        }

        public static bool operator ==(LispList a, LispList b)
        {
            if ((object)a == null && (object)b == null)
            {
                return true;
            }
            if ((object)a == null || (object)b == null)
            {
                return false;
            }
            if (ReferenceEquals(a, b))
            {
                return true;
            }
            if (a.IsNil && b.IsNil)
            {
                return true;
            }
            if (a.Value.Equals(b.Value))
            {
                return a.Next.Equals(b.Next);
            }
            else
            {
                return false;
            }
        }

        public static bool operator !=(LispList a, LispList b)
        {
            return !(a == b);
        }

        public override bool Equals(object obj)
        {
            return obj is LispList && this == (LispList)obj;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }
    }

    [DebuggerDisplay("{DebuggerDisplay,nq}")]
    internal class LispCircularList : LispList
    {
        private static int _refNumber = 1;

        private LispObject _value;
        private LispObject _next;
        private int _length;
        private bool _isProperList;

        public override LispObject Value => _value;
        public override LispObject Next => _next;
        public override int Length => _length;
        public override bool IsProperList => _isProperList;

        private string DebuggerDisplay => "(...)";

        public LispCircularList()
        {
        }

        public void ApplyForCircularReference(LispList otherList, bool isProperList)
        {
            _value = otherList.Value;
            _next = otherList.Next;
            _length = -Math.Abs(otherList.Length);
            _isProperList = isProperList;
        }

        public override IList<LispObject> ToList()
        {
            var result = new List<LispObject>();
            var itemCount = Math.Abs(Length);
            var current = (LispObject)this;
            for (int i = 0; i < itemCount; i++)
            {
                LispObject next;
                if (current is LispList list)
                {
                    result.Add(list.Value);
                    next = list.Next;
                }
                else
                {
                    result.Add(current);
                    next = null;
                }

                current = next;
            }

            return result;
        }

        public override string ToString()
        {
            var isFirstInvocation = _refNumber == 1;
            var thisRefNumber = _refNumber++;

            var selfWrittenIndices = new List<int>();
            var values = new List<string>();
            var current = (LispObject)this;
            var itemCount = Math.Abs(Length);
            if (!IsProperList)
            {
                itemCount++;
            }

            for (int i = 0; i < itemCount; i++)
            {
                LispObject currentValue;
                LispObject next;
                if (current is LispList list)
                {
                    currentValue = list.Value;
                    next = list.Next;
                }
                else
                {
                    currentValue = current;
                    next = null;
                }

                if (ReferenceEquals(this, current) || ReferenceEquals(this, currentValue))
                {
                    selfWrittenIndices.Add(i);
                    values.Add($"#{thisRefNumber}#");
                }
                else
                {
                    values.Add(currentValue.ToString());
                }

                current = next;
            }

            if (selfWrittenIndices.Count > 1 && !ReferenceEquals(this, Value))
            {
                values[selfWrittenIndices[0]] = Value.ToString();
            }

            if (!IsProperList && values.Count >= 2)
            {
                var penult = values[values.Count - 2];
                var ultima = values[values.Count - 1];
                values.RemoveAt(values.Count - 1);
                values.RemoveAt(values.Count - 1);
                values.Add($"{penult} . {ultima}");
            }

            if (isFirstInvocation)
            {
                _refNumber = 1;
            }

            var result = $"#{thisRefNumber}=({string.Join(" ", values)})";
            return result;
        }

        protected override string ToStringTail()
        {
            // TODO: find a better way of displaying this
            return " ...)";
        }
    }

    public class LispNilList : LispList
    {
        public readonly static LispNilList Instance = new LispNilList();

        public override LispObject Value => this;
        public override LispObject Next => this;
        public override int Length => 0;
        public override bool IsNil => true;

        private LispNilList()
        {
        }

        public override string ToString()
        {
            return "()";
        }

        protected override string ToStringTail()
        {
            return ")";
        }
    }

    public abstract class LispFunction : LispObject
    {
        public string Name { get; }
        public string Documentation { get; }

        public LispFunction(string name, string documentation)
        {
            Name = name;
            Documentation = documentation;
        }
    }

    public class LispCodeFunction : LispFunction
    {
        // TODO: make these read only collections
        public string[] Arguments { get; }
        public LispObject[] Commands { get; }

        public LispCodeFunction(string name, string documentation, IEnumerable<string> arguments, IEnumerable<LispObject> commands)
            : base(name, documentation)
        {
            Arguments = arguments.ToArray();
            Commands = commands.ToArray();
        }

        public override string ToString()
        {
            return $"{Name} ({string.Join(" ", Arguments)})";
        }

        public void BindArguments(LispObject[] args, LispStackFrame frame)
        {
            // bind arguments
            // TODO: validate argument count
            for (int i = 0; i < args.Length; i++)
            {
                frame.SetValue(Arguments[i], args[i]);
            }
        }
    }

    public class LispNativeFunction : LispFunction
    {
        public LispFunctionDelegate Function { get; }

        public LispNativeFunction(string name, string documentation, LispFunctionDelegate function)
            : base(name, documentation)
        {
            Function = function;
        }

        public override string ToString()
        {
            return $"{Name} <native>";
        }
    }

    public abstract class LispMacro : LispObject
    {
        public string Name { get; }

        public LispMacro(string name)
        {
            Name = name;
        }
    }

    public class LispCodeMacro : LispMacro
    {
        // TODO: make these read only collections
        public string[] Arguments { get; }
        public LispObject[] Body { get; }

        public LispCodeMacro(string name, IEnumerable<string> arguments, IEnumerable<LispObject> body)
            : base(name)
        {
            Arguments = arguments.ToArray();
            Body = body.ToArray();
        }

        public override string ToString()
        {
            return $"{Name} ({string.Join(" ", Arguments)})";
        }

        internal void ExpandBody(LispObject[] args, LispStackFrame frame)
        {
            // bind arguments
            // TODO: validate argument count
            for (int i = 0; i < args.Length; i++)
            {
                frame.SetMacroExpansion(Arguments[i], args[i]);
            }
        }
    }

    public class LispNativeMacro : LispMacro
    {
        public LispMacroDelegate Macro { get; }

        public LispNativeMacro(string name, LispMacroDelegate macro)
            : base(name)
        {
            Macro = macro;
        }

        public override string ToString()
        {
            return $"{Name} <native>";
        }
    }

    internal class LispTailCall : LispObject
    {
        public LispObject Value { get; }

        public LispTailCall(LispObject value)
        {
            Value = value;
        }
    }
}
