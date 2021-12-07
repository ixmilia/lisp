using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using IxMilia.Lisp.Parser;
using IxMilia.Lisp.Tokens;

namespace IxMilia.Lisp
{
    internal delegate void ObjectPointerValueSetter(LispObject value);

    public abstract class LispObject
    {
        public static IEqualityComparer<LispObject> Comparer { get; } = new LispObjectEqualityComparer();

        internal ObjectPointerValueSetter SetPointerValue { get; set; } = null;

        public LispSourceLocation? SourceLocation { get; internal set; }
        public LispObject Parent { get; internal set; }
        public abstract IEnumerable<LispObject> GetChildren();

        protected abstract LispObject CloneProtected();

        public LispObject Clone()
        {
            var result = CloneProtected();
            result.SourceLocation = SourceLocation;
            return result;
        }

        public virtual string ToString(bool useEscapeCharacters)
        {
            return ToString();
        }

        public LispObject PerformMacroReplacements(IDictionary<string, LispObject> replacements)
        {
            LispObject result;
            switch (this)
            {
                // this is the only real replacement possibility
                case LispSymbol symbol:
                    if (!replacements.TryGetValue(symbol.Value, out result))
                    {
                        result = this;
                    }
                    break;

                // these go into the definitions
                // TODO: replace in arguments?
                case LispCodeMacro codeMacro:
                    result = new LispCodeMacro(codeMacro.Name, codeMacro.ArgumentCollection, codeMacro.Body.PerformMacroReplacements(replacements).ToList());
                    break;
                case LispCodeFunction codeFunction:
                    result = new LispCodeFunction(codeFunction.Name, codeFunction.Documentation, codeFunction.ArgumentCollection, codeFunction.Commands.PerformMacroReplacements(replacements).ToList());
                    break;

                case LispQuotedLambdaFunctionReference lambdaFunction:
                    result = new LispQuotedLambdaFunctionReference((LispCodeFunction)lambdaFunction.Definition.PerformMacroReplacements(replacements));
                    break;
                case LispQuotedNamedFunctionReference quotedFunction:
                    if (!replacements.TryGetValue(quotedFunction.Name, out result))
                    {
                        result = this;
                    }
                    break;

                // these get no replacement
                case LispError _:
                case LispKeyword _:
                case LispNumber _:
                case LispString _:
                case LispNativeMacro _:
                case LispNativeFunction _:
                case LispMacro _:
                case LispNilList _:
                case LispStream _:
                    result = this;
                    break;

                // recurse into these
                case LispForwardListReference forwardList:
                    result = new LispForwardListReference(forwardList.ForwardReference, (LispList)forwardList.List.PerformMacroReplacements(replacements));
                    break;
                case LispList list:
                    result = new LispList(list.Value.PerformMacroReplacements(replacements), list.Next.PerformMacroReplacements(replacements));
                    break;

                // error
                default:
                    throw new InvalidOperationException($"Unable to perform macro replacement on {this.GetType().Name}");
            }

            result.SourceLocation = SourceLocation;
            return result;
        }

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

        //internal void TryApplyStackFrame(LispStackFrame frame)
        //{
        //    if (StackFrame == null)
        //    {
        //        StackFrame = frame;
        //        SourceLocation = frame.SourceLocation;
        //    }
        //}

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        protected override LispObject CloneProtected()
        {
            return new LispError(Message, StackFrame);
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

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        protected override LispObject CloneProtected()
        {
            return new LispSymbol(Value);
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

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        protected override LispObject CloneProtected()
        {
            return new LispKeyword(Keyword);
        }

        public override string ToString()
        {
            return Keyword;
        }
    }

    public class LispLambdaListKeyword : LispObject
    {
        public string Keyword { get; }

        public LispLambdaListKeyword(string keyword)
        {
            Keyword = keyword;
        }

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        protected override LispObject CloneProtected()
        {
            return new LispLambdaListKeyword(Keyword);
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

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        protected override LispObject CloneProtected()
        {
            return new LispInteger(Value);
        }

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

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        protected override LispObject CloneProtected()
        {
            return new LispFloat(Value);
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

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        protected override LispObject CloneProtected()
        {
            return new LispRatio(Numerator, Denominator);
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

    public class LispCharacter : LispObject
    {
        public char Value { get; set; }

        public LispCharacter(char value)
        {
            Value = value;
        }

        public override string ToString(bool useEscapeCharacters)
        {
            // TODO: handle stuff like SPACE, etc.
            return $"#\\{Value}";
        }

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        protected override LispObject CloneProtected()
        {
            return new LispCharacter(Value);
        }

        public override string ToString()
        {
            return ToString(true);
        }

        public static bool operator ==(LispCharacter a, LispCharacter b)
        {
            return a?.Value == b?.Value;
        }

        public static bool operator !=(LispCharacter a, LispCharacter b)
        {
            return !(a == b);
        }

        public override bool Equals(object obj)
        {
            return obj is LispCharacter && this == (LispCharacter)obj;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }
    }

    public class LispString : LispObject
    {
        public string Value { get; set; }

        public LispString(string value)
        {
            Value = value;
        }

        public override string ToString(bool useEscapeCharacters)
        {
            return useEscapeCharacters
                ? ToString()
                : Value;
        }

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        protected override LispObject CloneProtected()
        {
            return new LispString(Value);
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

        public override IEnumerable<LispObject> GetChildren()
        {
            yield return List;
        }

        protected override LispObject CloneProtected()
        {
            return new LispForwardListReference(ForwardReference, (LispList)List.Clone());
        }
    }

    public class LispList : LispObject
    {
        public virtual LispObject Value { get; internal set; }
        public virtual LispObject Next { get; internal set; }
        public virtual int Length { get; }

        public virtual bool IsProperList
        {
            get
            {
                var list = this;
                while (!list.IsNil())
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
            if (next is LispList list)
            {
                Length = list.Length + 1;
            }
            else
            {
                Length = 1; // `Value`; improper tail isn't counted
            }
        }

        public override IEnumerable<LispObject> GetChildren()
        {
            return ToList();
        }

        protected override LispObject CloneProtected()
        {
            return new LispList(Value.Clone(), Next.Clone());
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
            while (!head.IsNil())
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
            if (a.IsNil() && b.IsNil())
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

        private LispNilList()
        {
        }

        protected override LispObject CloneProtected()
        {
            return this;
        }

        public override string ToString()
        {
            return "()";
        }

        protected override string ToStringTail()
        {
            return ")";
        }

        internal static LispNilList CreateForParser()
        {
            return new LispNilList();
        }
    }

    public abstract class LispInvocableObject : LispObject
    {
        public string Name { get; }

        protected LispInvocableObject(string name)
        {
            Name = name;
        }
    }

    public abstract class LispFunction : LispInvocableObject
    {
        public string Documentation { get; }

        public LispFunction(string name, string documentation)
            : base(name)
        {
            Documentation = documentation;
        }
    }

    public class LispCodeFunction : LispFunction
    {
        // TODO: make these read only collections
        public LispArgumentCollection ArgumentCollection { get; }
        public LispObject[] Commands { get; internal set; }

        public LispCodeFunction(string name, string documentation, LispArgumentCollection argumentCollection, IEnumerable<LispObject> commands)
            : base(name, documentation)
        {
            ArgumentCollection = argumentCollection;
            Commands = commands.ToArray();
        }

        public override IEnumerable<LispObject> GetChildren()
        {
            return ArgumentCollection.GetChildren().Concat(Commands);
        }

        protected override LispObject CloneProtected()
        {
            return new LispCodeFunction(Name, Documentation, ArgumentCollection, Commands);
        }

        public override string ToString()
        {
            return $"{Name} ({ArgumentCollection})";
        }

        public bool TryBindArguments(LispObject[] args, LispHost host, LispStackFrame frame, out LispError error)
        {
            if (!ArgumentCollection.TryMatchInvocationArguments(args, out var matchedArguments, out error))
            {
                return false;
            }

            foreach (var matchedArgument in matchedArguments)
            {
                var argumentName = matchedArgument.Item1.Name;
                var argumentValue = matchedArgument.Item2;
                switch (matchedArgument.Item1)
                {
                    case LispAuxiliaryInvocationArgument _:
                    case LispKeywordInvocationArgument _:
                    case LispOptionalInvocationArgument _:
                        // `&aux`, `&key`, and `&optional` arguments need to be evaluated
                        argumentValue = host.EvalAtStackFrame(frame, argumentValue);
                        break;
                }

                if (argumentValue is LispError argumentError)
                {
                    error = argumentError;
                    return false;
                }

                frame.SetValue(argumentName, argumentValue);
            }

            return true;
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

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        protected override LispObject CloneProtected()
        {
            return new LispNativeFunction(Name, Documentation, Function);
        }

        public override string ToString()
        {
            return $"{Name} <native>";
        }
    }

    public abstract class LispFunctionReference : LispObject
    {
    }

    public class LispQuotedNamedFunctionReference : LispFunctionReference
    {
        public string Name { get; }

        public LispQuotedNamedFunctionReference(string name)
        {
            Name = name;
        }

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        protected override LispObject CloneProtected()
        {
            return new LispQuotedNamedFunctionReference(Name);
        }

        public override string ToString()
        {
            return $"#'{Name}";
        }
    }

    public class LispQuotedLambdaFunctionReference : LispFunctionReference
    {
        public LispCodeFunction Definition { get; }
        internal LispStackFrame StackFrame { get; set; }

        public LispQuotedLambdaFunctionReference(LispCodeFunction definition)
        {
            Definition = definition;
        }

        public override IEnumerable<LispObject> GetChildren()
        {
            yield return Definition;
        }

        protected override LispObject CloneProtected()
        {
            return new LispQuotedLambdaFunctionReference((LispCodeFunction)Definition.Clone());
        }

        public override string ToString()
        {
            return Definition.ToString();
        }
    }

    public class LispSpecialOperator : LispInvocableObject
    {
        public LispSpecialOperatorDelegate Delegate { get; }

        public LispSpecialOperator(string name, LispSpecialOperatorDelegate del)
            : base(name)
        {
            Delegate = del;
        }

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        protected override LispObject CloneProtected()
        {
            return new LispSpecialOperator(Name, Delegate);
        }

        public override string ToString()
        {
            return $"{Name} <native-special-op>";
        }
    }

    public abstract class LispMacro : LispInvocableObject
    {
        public LispMacro(string name)
            : base(name)
        {
        }
    }

    public class LispCodeMacro : LispMacro
    {
        public LispArgumentCollection ArgumentCollection { get; }
        public LispObject[] Body { get; }

        public LispCodeMacro(string name, LispArgumentCollection argumentCollection, IEnumerable<LispObject> body)
            : base(name)
        {
            ArgumentCollection = argumentCollection;
            Body = body.ToArray();
        }

        public override IEnumerable<LispObject> GetChildren()
        {
            return ArgumentCollection.GetChildren().Concat(Body);
        }

        protected override LispObject CloneProtected()
        {
            return new LispCodeMacro(Name, ArgumentCollection, Body);
        }

        public override string ToString()
        {
            return $"{Name} ({ArgumentCollection})";
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

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        protected override LispObject CloneProtected()
        {
            return new LispNativeMacro(Name, Macro);
        }

        public override string ToString()
        {
            return $"{Name} <native>";
        }
    }

    public class LispStream : LispObject
    {
        public string Name { get; }

        public TextReader Input { get; }
        public TextWriter Output { get; }

        public LispStream(string name, TextReader input, TextWriter output)
        {
            Name = name;
            Input = input;
            Output = output;
        }

        public LispObject ReadObject(LispObject eofMarker = null)
        {
            eofMarker = eofMarker ?? new LispError("EOF");
            var input = Input.ReadLine();
            if (input is null)
            {
                return eofMarker;
            }

            var tokenizer = new LispTokenizer(Name, input);
            var tokens = tokenizer.GetTokens();
            var parser = new LispParser();
            parser.AddTokens(tokens);
            var result = parser.Parse();
            if (result.RemainingTokens.Any())
            {
                return new LispError("Incomplete expression");
            }

            var nodes = result.Nodes.ToList();
            if (nodes.Count == 0)
            {
                return eofMarker;
            }

            if (nodes.Count > 1)
            {
                return new LispError("More than one expression read");
            }

            return nodes[0];
        }

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        protected override LispObject CloneProtected()
        {
            return new LispStream(Name, Input, Output);
        }
    }

    public class LispFileStream : LispStream
    {
        public FileStream FileStream { get; }

        public LispFileStream(string name, FileStream fileStream)
            : base(name, new StreamReader(fileStream), new StreamWriter(fileStream))
        {
            FileStream = fileStream;
        }

        protected override LispObject CloneProtected()
        {
            return new LispFileStream(Name, FileStream);
        }
    }
}
