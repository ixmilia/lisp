using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;

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
            result.Parent = Parent;
            result.SourceLocation = SourceLocation;
            return result;
        }

        public virtual string ToString(bool useEscapeCharacters)
        {
            return ToString();
        }

        public virtual string ToDisplayString(LispPackage currentPackage) => ToString();

        public LispObject PerformMacroReplacements(LispPackage currentPackage, IDictionary<string, LispObject> replacements)
        {
            LispObject result;
            switch (this)
            {
                // this is the only real replacement possibility
                case LispSymbol symbol:
                    var resolvedSymbol = symbol.Resolve(currentPackage);
                    if (!replacements.TryGetValue(resolvedSymbol.Value, out result))
                    {
                        result = this;
                    }
                    break;

                // these go into the definitions
                // TODO: replace in arguments?
                case LispCodeMacro codeMacro:
                    result = new LispCodeMacro(codeMacro.NameSymbol, codeMacro.Documentation, codeMacro.ArgumentCollection, codeMacro.Body.PerformMacroReplacements(currentPackage, replacements).ToList());
                    break;
                case LispCodeFunction codeFunction:
                    result = new LispCodeFunction(codeFunction.NameSymbol, codeFunction.Documentation, codeFunction.ArgumentCollection, codeFunction.Commands.PerformMacroReplacements(currentPackage, replacements).ToList());
                    break;

                case LispQuotedLambdaFunctionReference lambdaFunction:
                    result = new LispQuotedLambdaFunctionReference((LispCodeFunction)lambdaFunction.Definition.PerformMacroReplacements(currentPackage, replacements));
                    break;
                case LispQuotedNamedFunctionReference quotedFunction:
                    if (!replacements.TryGetValue(quotedFunction.Name, out result))
                    {
                        result = this;
                    }
                    break;

                // these get no replacement
                case LispError _:
                case LispNumber _:
                case LispCharacter _:
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
                    result = new LispForwardListReference(forwardList.SymbolReference, (LispList)forwardList.List.PerformMacroReplacements(currentPackage, replacements));
                    break;
                case LispList list:
                    result = new LispList(list.Value.PerformMacroReplacements(currentPackage, replacements), list.Next.PerformMacroReplacements(currentPackage, replacements));
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

    public abstract class LispSymbol : LispObject
    {
        public string LocalName { get; }

        protected LispSymbol(string localName)
        {
            LocalName = localName;
        }

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        public LispResolvedSymbol Resolve(LispPackage currentPackage)
        {
            switch (this)
            {
                case LispUnresolvedSymbol unresolvedSymbol:
                    return currentPackage.ResolveSymbol(unresolvedSymbol);
                case LispResolvedSymbol resolvedSymbol:
                    return resolvedSymbol;
                default:
                    throw new NotSupportedException("Not a supported symbol type");
            }
        }

        public static LispSymbol CreateFromString(string name, string defaultPackageName = null)
        {
            var nameParts = SplitPackageAndSymbolName(name);
            if (nameParts.Item1 == null && defaultPackageName == null)
            {
                return new LispUnresolvedSymbol(nameParts.Item2);
            }
            else
            {
                return new LispResolvedSymbol(nameParts.Item1 ?? defaultPackageName, nameParts.Item2, nameParts.Item3);
            }
        }

        /// <summary>
        /// Splits a name into a 3-part tuple of (packageName, localName, isPrivate).
        /// </summary>
        public static Tuple<string, string, bool> SplitPackageAndSymbolName(string name)
        {
            var colonIndex = name.IndexOf(':');
            string packageName = null;
            string localName;
            var isPublic = true;
            if (colonIndex >= 0)
            {
                packageName = name.Substring(0, colonIndex);
                if (string.IsNullOrEmpty(packageName))
                {
                    packageName = "KEYWORD";
                }

                // double colon = private symbol
                if (name.Length > colonIndex + 1 &&
                    name[colonIndex + 1] == ':')
                {
                    isPublic = false;
                    colonIndex++;
                }

                localName = name.Substring(colonIndex + 1);
            }
            else
            {
                localName = name;
            }

            return Tuple.Create(packageName, localName, isPublic);
        }
    }

    public class LispUnresolvedSymbol : LispSymbol, IEquatable<LispUnresolvedSymbol>
    {
        public LispUnresolvedSymbol(string name)
            : base(name)
        {
        }

        protected override LispObject CloneProtected()
        {
            return new LispUnresolvedSymbol(LocalName);
        }

        public override string ToString() => LocalName;

        public override bool Equals(object obj)
        {
            return Equals(obj as LispUnresolvedSymbol);
        }

        public bool Equals(LispUnresolvedSymbol other)
        {
            return other != null &&
                   LocalName == other.LocalName;
        }

        public override int GetHashCode()
        {
            return LocalName.GetHashCode();
        }

        public static bool operator==(LispUnresolvedSymbol a, LispUnresolvedSymbol b)
        {
            return a?.LocalName == b?.LocalName;
        }

        public static bool operator!=(LispUnresolvedSymbol a, LispUnresolvedSymbol b)
        {
            return !(a == b);
        }
    }

    public class LispResolvedSymbol : LispSymbol, IEquatable<LispResolvedSymbol>
    {
        public string PackageName { get; }
        public bool IsPublic { get; }

        public string Value => IsKeyword ? string.Concat(":", LocalName) : string.Concat(PackageName, IsPublic ? ":" : "::", LocalName);

        public bool IsKeyword => PackageName == "KEYWORD";

        public LispResolvedSymbol(string packageName, string localName, bool isPublic)
            : base(localName)
        {
            PackageName = packageName;
            IsPublic = isPublic;
        }

        protected override LispObject CloneProtected()
        {
            return new LispResolvedSymbol(PackageName, LocalName, IsPublic);
        }

        public override string ToString() => Value;

        public override string ToDisplayString(LispPackage currentPackage) => currentPackage.HasSymbolWithName(LocalName) || PackageName == currentPackage.Name ? LocalName : Value;

        public static bool operator ==(LispResolvedSymbol a, LispResolvedSymbol b)
        {
            return a?.Value == b?.Value;
        }

        public static bool operator !=(LispResolvedSymbol a, LispResolvedSymbol b)
        {
            return !(a == b);
        }

        public override bool Equals(object obj)
        {
            return obj is LispResolvedSymbol other && this == other;
        }

        public bool Equals(LispResolvedSymbol other)
        {
            return other != null &&
                   Value == other.Value;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
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
        Complex = 3,
    }

    public abstract class LispNumber : LispObject
    {
        public abstract LispNumberType Type { get; }

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
                        case LispComplexNumber cb:
                            return LispComplexNumber.AddComplex(ia.AsComplex(), cb);
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
                        case LispComplexNumber cb:
                            return LispComplexNumber.AddComplex(fa.AsComplex(), cb);
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
                        case LispComplexNumber cb:
                            return LispComplexNumber.AddComplex(ra.AsComplex(), cb);
                        default:
                            throw new InvalidOperationException("Not possible, expected a number.");
                    }
                case LispComplexNumber ca:
                    switch (b)
                    {
                        case LispSimpleNumber sb:
                            return LispComplexNumber.AddComplex(ca, sb.AsComplex());
                        case LispComplexNumber cb:
                            return LispComplexNumber.AddComplex(ca, cb);
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
                        case LispComplexNumber cb:
                            return LispComplexNumber.SubComplex(ia.AsComplex(), cb);
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
                        case LispComplexNumber cb:
                            return LispComplexNumber.SubComplex(fa.AsComplex(), cb);
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
                        case LispComplexNumber cb:
                            return LispComplexNumber.SubComplex(ra.AsComplex(), cb);
                        default:
                            throw new InvalidOperationException("Not possible, expected a number.");
                    }
                case LispComplexNumber ca:
                    switch (b)
                    {
                        case LispSimpleNumber sb:
                            return LispComplexNumber.SubComplex(ca, sb.AsComplex());
                        case LispComplexNumber cb:
                            return LispComplexNumber.SubComplex(ca, cb);
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
                        case LispComplexNumber cb:
                            return LispComplexNumber.MulComplex(ia.AsComplex(), cb);
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
                        case LispComplexNumber cb:
                            return LispComplexNumber.MulComplex(fa.AsComplex(), cb);
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
                        case LispComplexNumber cb:
                            return LispComplexNumber.MulComplex(ra.AsComplex(), cb);
                        default:
                            throw new InvalidOperationException("Not possible, expected a number.");
                    }
                case LispComplexNumber ca:
                    switch (b)
                    {
                        case LispSimpleNumber sb:
                            return LispComplexNumber.MulComplex(ca, sb.AsComplex());
                        case LispComplexNumber cb:
                            return LispComplexNumber.MulComplex(ca, cb);
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
                        case LispComplexNumber cb:
                            return LispComplexNumber.DivComplex(ia.AsComplex(), cb);
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
                        case LispComplexNumber cb:
                            return LispComplexNumber.DivComplex(fa.AsComplex(), cb);
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
                        case LispComplexNumber cb:
                            return LispComplexNumber.DivComplex(ra.AsComplex(), cb);
                        default:
                            throw new InvalidOperationException("Not possible, expected a number.");
                    }
                case LispComplexNumber ca:
                    switch (b)
                    {
                        case LispSimpleNumber sb:
                            return LispComplexNumber.DivComplex(ca, sb.AsComplex());
                        case LispComplexNumber cb:
                            return LispComplexNumber.DivComplex(ca, cb);
                        default:
                            throw new InvalidOperationException("Not possible, expected a number.");
                    }
                default:
                    throw new InvalidOperationException("Not possible, expected a number.");
            }
        }
    }

    public abstract class LispSimpleNumber : LispNumber
    {
        public LispComplexNumber AsComplex() => new LispComplexNumber(this, LispInteger.Zero);
    }

    public class LispInteger : LispSimpleNumber
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

    public class LispFloat : LispSimpleNumber
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

    public class LispRatio : LispSimpleNumber
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

        public LispSimpleNumber Reduce()
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
            var gcd = Gcd(Math.Abs(numerator), Math.Abs(denominator));
            finalNum = Math.Abs(numerator / gcd);
            finalDenom = Math.Abs(denominator / gcd);

            if (Math.Sign(numerator) != Math.Sign(denominator))
            {
                finalNum *= -1;
            }
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

        public static LispSimpleNumber operator +(LispRatio a, LispRatio b)
        {
            return new LispRatio(a.Numerator * b.Denominator + b.Numerator * a.Denominator, a.Denominator * b.Denominator).Reduce();
        }

        public static LispSimpleNumber operator -(LispRatio a, LispRatio b)
        {
            return new LispRatio(a.Numerator * b.Denominator - b.Numerator * a.Denominator, a.Denominator * b.Denominator).Reduce();
        }

        public static LispSimpleNumber operator *(LispRatio a, LispRatio b)
        {
            return new LispRatio(a.Numerator * b.Numerator, a.Denominator * b.Denominator).Reduce();
        }

        public static LispSimpleNumber operator /(LispRatio a, LispRatio b)
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

    public class LispComplexNumber : LispNumber, IEquatable<LispComplexNumber>
    {
        public override LispNumberType Type => LispNumberType.Complex;

        public LispSimpleNumber RealPart { get; }
        public LispSimpleNumber ImaginaryPart { get; }

        public LispComplexNumber(LispSimpleNumber realPart, LispSimpleNumber imaginaryPart)
        {
            RealPart = realPart;
            ImaginaryPart = imaginaryPart;
        }

        public LispNumber Reduce()
        {
            switch (ImaginaryPart)
            {
                case LispInteger i when i.IsZero:
                case LispFloat f when f.IsZero:
                case LispRatio r when r.IsZero:
                    return RealPart;
                default:
                    return this;
            }
        }

        public override IEnumerable<LispObject> GetChildren()
        {
            yield return RealPart;
            yield return ImaginaryPart;
        }

        protected override LispObject CloneProtected()
        {
            return new LispComplexNumber(RealPart, ImaginaryPart);
        }

        public override string ToString()
        {
            return $"#C({RealPart} {ImaginaryPart})";
        }

        public static bool operator ==(LispComplexNumber a, LispComplexNumber b)
        {
            return a?.RealPart == b?.RealPart && a?.ImaginaryPart == b?.ImaginaryPart;
        }

        public static bool operator !=(LispComplexNumber a, LispComplexNumber b)
        {
            return !(a == b);
        }

        public override bool Equals(object obj)
        {
            if (obj is LispComplexNumber c)
            {
                return this == c;
            }

            return false;
        }

        internal static LispNumber AddComplex(LispComplexNumber a, LispComplexNumber b)
        {
            var real = (LispSimpleNumber)Add(a.RealPart, b.RealPart);
            var img = (LispSimpleNumber)Add(a.ImaginaryPart, b.ImaginaryPart);
            var result = new LispComplexNumber(real, img).Reduce();
            return result;
        }

        internal static LispNumber SubComplex(LispComplexNumber a, LispComplexNumber b)
        {
            var real = (LispSimpleNumber)Sub(a.RealPart, b.RealPart);
            var img = (LispSimpleNumber)Sub(a.ImaginaryPart, b.ImaginaryPart);
            var result = new LispComplexNumber(real, img).Reduce();
            return result;
        }

        internal static LispNumber MulComplex(LispComplexNumber a, LispComplexNumber b)
        {
            var real = (LispSimpleNumber)Sub(Mul(a.RealPart, b.RealPart), Mul(a.ImaginaryPart, b.ImaginaryPart));
            var img = (LispSimpleNumber)Add(Mul(a.RealPart, b.ImaginaryPart), Mul(a.ImaginaryPart, b.RealPart));
            var result = new LispComplexNumber(real, img).Reduce();
            return result;
        }

        internal static LispNumber DivComplex(LispComplexNumber a, LispComplexNumber b)
        {
            var denom = Add(Mul(b.RealPart, b.RealPart), Mul(b.ImaginaryPart, b.ImaginaryPart));
            var real = (LispSimpleNumber)Div(Add(Mul(a.RealPart, b.RealPart), Mul(a.ImaginaryPart, b.ImaginaryPart)), denom);
            var img = (LispSimpleNumber)Div(Sub(Mul(a.ImaginaryPart, b.RealPart), Mul(a.RealPart, b.ImaginaryPart)), denom);
            var result = new LispComplexNumber(real, img).Reduce();
            return result;
        }

        public bool Equals(LispComplexNumber other)
        {
            return other != null &&
                   EqualityComparer<LispSimpleNumber>.Default.Equals(RealPart, other.RealPart) &&
                   EqualityComparer<LispSimpleNumber>.Default.Equals(ImaginaryPart, other.ImaginaryPart);
        }

        public override int GetHashCode()
        {
            int hashCode = 1382181547;
            hashCode = hashCode * -1521134295 + EqualityComparer<LispSimpleNumber>.Default.GetHashCode(RealPart);
            hashCode = hashCode * -1521134295 + EqualityComparer<LispSimpleNumber>.Default.GetHashCode(ImaginaryPart);
            return hashCode;
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
            var sb = new StringBuilder();
            sb.Append('"');
            foreach (var c in Value)
            {
                switch (c)
                {
                    case '"':
                    case '\\':
                        sb.Append('\\');
                        sb.Append(c);
                        break;
                    case '\n':
                        sb.Append("\\n");
                        break;
                    case '\t':
                        sb.Append("\\t");
                        break;
                    default:
                        sb.Append(c);
                        break;
                }
            }

            sb.Append('"');
            return sb.ToString();
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
        public LispResolvedSymbol SymbolReference { get; }
        public LispList List { get; }

        public LispForwardListReference(LispResolvedSymbol symbolReference, LispList list)
        {
            SymbolReference = symbolReference;
            List = list;
        }

        public override IEnumerable<LispObject> GetChildren()
        {
            yield return List;
        }

        protected override LispObject CloneProtected()
        {
            return new LispForwardListReference(SymbolReference, (LispList)List.Clone());
        }

        public override string ToString()
        {
            var symbolName = SymbolReference.Value;
            var leadingText = symbolName.Substring(0, symbolName.Length - 1) + "=";
            return string.Concat(leadingText, List.ToString());
        }

        public override string ToDisplayString(LispPackage currentPackage)
        {
            var symbolName = SymbolReference.ToDisplayString(currentPackage);
            var leadingText = symbolName.Substring(0, symbolName.Length - 1) + "=";
            return String.Concat(leadingText, List.ToDisplayString(currentPackage));
        }
    }

    public class LispVector : LispObject
    {
        private List<LispObject> _items;

        public bool IsAdjustable { get; }
        public int Size { get; }
        public int Count => _items.Count;

        private LispVector(int size, bool isAdjustable, IEnumerable<LispObject> items)
        {
            Size = size;
            IsAdjustable = isAdjustable;
            _items = items.ToList();
        }

        public LispObject this[int index]
        {
            get => _items[index];
            internal set => _items[index] = value;
        }

        public bool TryAdd(LispObject value, out LispError error)
        {
            if (!IsAdjustable)
            {
                error = new LispError("Vector is not adjustable");
                return false;
            }

            if (_items.Count >= Size)
            {
                error = new LispError("Vector is full");
                return false;
            }

            error = default;
            _items.Add(value);
            return true;
        }

        public bool TryPop(out LispObject value)
        {
            if (!IsAdjustable)
            {
                value = new LispError("Vector is not adjustable");
                return false;
            }

            if (Count == 0)
            {
                value = new LispError("Vector is empty");
                return false;
            }

            value = _items[_items.Count - 1];
            _items.RemoveAt(_items.Count - 1);
            return true;
        }

        public static LispVector CreateFixed(IEnumerable<LispObject> items)
        {
            return new LispVector(items.Count(), false, items);
        }

        public static LispVector CreateAdjustable(int size, IEnumerable<LispObject> items)
        {
            if (size < items.Count())
            {
                throw new ArgumentException(nameof(size), "Maximum size cannot be less than the number of items.");
            }

            return new LispVector(size, true, items);
        }

        protected override LispObject CloneProtected()
        {
            return new LispVector(Size, IsAdjustable, _items);
        }

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        public override string ToString()
        {
            return $"#({string.Join(" ", _items.Select(i => i.ToString()))})";
        }

        public override string ToDisplayString(LispPackage currentPackage)
        {
            return $"#({string.Join(" ", _items.Select(i => i.ToDisplayString(currentPackage)))})";
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

        public override string ToDisplayString(LispPackage currentPackage)
        {
            return $"({Value.ToDisplayString(currentPackage)}{NextToDisplayString(currentPackage)}";
        }

        protected virtual string ToDisplayStringTail(LispPackage currentPackage)
        {
            return $" {Value.ToDisplayString(currentPackage)}{NextToDisplayString(currentPackage)}";
        }

        private string NextToDisplayString(LispPackage currentPackage)
        {
            string nextString;
            if (Next is LispList list)
            {
                nextString = list.ToDisplayStringTail(currentPackage);
            }
            else
            {
                nextString = $" . {Next.ToDisplayString(currentPackage)}";
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

        public override string ToString() => ToString(null);

        public override string ToDisplayString(LispPackage currentPackage) => ToString(currentPackage);

        private string ToString(LispPackage currentOrNullPackage)
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
                    values.Add(currentOrNullPackage == null ? currentValue.ToString() : currentValue.ToDisplayString(currentOrNullPackage));
                }

                current = next;
            }

            if (selfWrittenIndices.Count > 1 && !ReferenceEquals(this, Value))
            {
                values[selfWrittenIndices[0]] = currentOrNullPackage == null ? Value.ToString() : Value.ToDisplayString(currentOrNullPackage);
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

        public override string ToDisplayString(LispPackage currentPackage)
        {
            return ToString();
        }

        protected override string ToStringTail()
        {
            return ")";
        }

        protected override string ToDisplayStringTail(LispPackage currentPackage)
        {
            return ToStringTail();
        }

        internal static LispNilList CreateForParser()
        {
            return new LispNilList();
        }
    }

    public abstract class LispInvocableObject : LispObject
    {
        public LispResolvedSymbol NameSymbol { get; }
        public string Documentation { get; }

        protected LispInvocableObject(LispResolvedSymbol nameSymbol, string documentation)
        {
            NameSymbol = nameSymbol;
            Documentation = documentation;
        }
    }

    public abstract class LispFunction : LispInvocableObject
    {
        public LispFunction(LispResolvedSymbol nameSymbol, string documentation)
            : base(nameSymbol, documentation)
        {
        }
    }

    public class LispCodeFunction : LispFunction
    {
        // TODO: make these read only collections
        public LispArgumentCollection ArgumentCollection { get; }
        public LispObject[] Commands { get; internal set; }

        public LispCodeFunction(LispResolvedSymbol nameSymbol, string documentation, LispArgumentCollection argumentCollection, IEnumerable<LispObject> commands)
            : base(nameSymbol, documentation)
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
            return new LispCodeFunction(NameSymbol, Documentation, ArgumentCollection, Commands);
        }

        public override string ToString() => $"{NameSymbol.LocalName} ({ArgumentCollection})";

        public override string ToDisplayString(LispPackage currentPackage) => $"{NameSymbol.ToDisplayString(currentPackage)} ({ArgumentCollection})";

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

                var resolvedArgument = LispSymbol.CreateFromString(argumentName).Resolve(host.CurrentPackage);
                frame.SetValue(resolvedArgument, argumentValue);
            }

            return true;
        }
    }

    public class LispNativeFunction : LispFunction
    {
        public string Signature { get; }
        public LispFunctionDelegate Function { get; }

        public LispNativeFunction(LispResolvedSymbol nameSymbol, string documentation, string signature, LispFunctionDelegate function)
            : base(nameSymbol, documentation)
        {
            Signature = signature;
            Function = function;
        }

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        protected override LispObject CloneProtected()
        {
            return new LispNativeFunction(NameSymbol, Documentation, Signature, Function);
        }

        public override string ToString() => $"{NameSymbol.LocalName} <native>";

        public override string ToDisplayString(LispPackage currentPackage) => $"{NameSymbol.ToDisplayString(currentPackage)} <native>";
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

        public override string ToString() => Definition.ToString();

        public override string ToDisplayString(LispPackage currentPackage) => Definition.ToDisplayString(currentPackage);
    }

    public class LispSpecialOperator : LispInvocableObject
    {
        public string Signature { get; }
        public LispSpecialOperatorDelegate Delegate { get; }

        public LispSpecialOperator(LispResolvedSymbol nameSymbol, string documentation, string signature, LispSpecialOperatorDelegate del)
            : base(nameSymbol, documentation)
        {
            Signature = signature;
            Delegate = del;
        }

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        protected override LispObject CloneProtected()
        {
            return new LispSpecialOperator(NameSymbol, Documentation, Signature, Delegate);
        }

        public override string ToString() => $"{NameSymbol.LocalName} <native-special-op>";

        public override string ToDisplayString(LispPackage currentPackage) => $"{NameSymbol.ToDisplayString(currentPackage)} <native-special-op>";
    }

    public abstract class LispMacro : LispInvocableObject
    {
        public LispMacro(LispResolvedSymbol nameSymbol, string documentation)
            : base(nameSymbol, documentation)
        {
        }
    }

    public class LispCodeMacro : LispMacro
    {
        public LispArgumentCollection ArgumentCollection { get; }
        public LispObject[] Body { get; }

        public LispCodeMacro(LispResolvedSymbol nameSymbol, string documentation, LispArgumentCollection argumentCollection, IEnumerable<LispObject> body)
            : base(nameSymbol, documentation)
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
            return new LispCodeMacro(NameSymbol, Documentation, ArgumentCollection, Body);
        }

        public override string ToString() => $"{NameSymbol.LocalName} ({ArgumentCollection})";

        public override string ToDisplayString(LispPackage currentPackage) => $"{NameSymbol.ToDisplayString(currentPackage)} ({ArgumentCollection})";
    }

    public class LispNativeMacro : LispMacro
    {
        public string Signature { get; }
        public LispMacroDelegate Macro { get; }

        public LispNativeMacro(LispResolvedSymbol nameSymbol, string documentation, string signature, LispMacroDelegate macro)
            : base(nameSymbol, documentation)
        {
            Signature = signature;
            Macro = macro;
        }

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        protected override LispObject CloneProtected()
        {
            return new LispNativeMacro(NameSymbol, Documentation, Signature, Macro);
        }

        public override string ToString() => $"{NameSymbol} <native>";

        public override string ToDisplayString(LispPackage currentPackage) => $"{NameSymbol.ToDisplayString(currentPackage)} <native>";
    }

    public abstract class LispStream : LispObject
    {
        public string Name { get; }

        protected LispStream(string name)
        {
            Name = name;
        }

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        public override string ToString() => $"<stream> {Name}";
    }

    public class LispTextStream : LispStream
    {
        private ReportingTextReader _input;
        private ReportingTextWriter _output;

        public TextReader Input => _input;
        public TextWriter Output => _output;

        public LispSourcePosition CurrentPosition => _input.CurrentPosition;
        public bool IsInputComplete => _input.Peek() == -1;

        public event EventHandler<LispCharacter> CharacterRead;

        public LispTextStream(string name, TextReader input, TextWriter output)
            : this(name, new ReportingTextReader(input), new ReportingTextWriter(output))
        {
        }

        private LispTextStream(string name, ReportingTextReader input, ReportingTextWriter output)
            : base(name)
        {
            _input = input;
            _output = output;
        }

        public LispCharacter Peek()
        {
            var i = _input.Peek();
            if (i == -1)
            {
                return null;
            }

            var c = (char)i;
            var lc = new LispCharacter(c);
            var startPosition = CurrentPosition;
            lc.SourceLocation = new LispSourceLocation(Name, startPosition, new LispSourcePosition(startPosition.Line, startPosition.Column + 1));
            return lc;
        }

        public LispCharacter Read()
        {
            var lc = Peek();
            if (lc is object)
            {
                // swallow and advance
                _input.Read();
                CharacterRead?.Invoke(this, lc);
            }

            return lc;
        }

        public string GetOutputContents() => _output.GetContents();

        public override IEnumerable<LispObject> GetChildren()
        {
            yield break;
        }

        protected override LispObject CloneProtected()
        {
            return new LispTextStream(Name, _input, _output);
        }
    }

    public class LispFileStream : LispTextStream
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
