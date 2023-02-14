namespace IxMilia.Lisp
{
    public abstract class LispInvocationArgument
    {
        public LispSymbol Declaration { get; }
        public string Name => Declaration.ToString();

        protected LispInvocationArgument(LispSymbol declaration)
        {
            Declaration = declaration;
        }

        public virtual string ToDisplayString(LispPackage currentPackage) => Declaration.ToDisplayString(currentPackage);
    }

    public class LispRegularInvocationArgument : LispInvocationArgument
    {
        internal LispRegularInvocationArgument(LispSymbol declaration)
            : base(declaration)
        {
        }

        public override string ToString() => Name;
    }

    public class LispOptionalInvocationArgument : LispInvocationArgument
    {
        public LispObject DefaultValue { get; }

        internal LispOptionalInvocationArgument(LispSymbol declaration, LispObject defaultValue)
            : base(declaration)
        {
            DefaultValue = defaultValue;
        }

        public override string ToString() => $"&OPTIONAL ({Name} {DefaultValue})";

        public override string ToDisplayString(LispPackage currentPackage) => $"&OPTIONAL ({Name} {DefaultValue.ToDisplayString(currentPackage)})";
    }

    public class LispKeywordInvocationArgument : LispInvocationArgument
    {
        public LispObject DefaultValue { get; }

        internal LispKeywordInvocationArgument(LispSymbol declaration, LispObject defaultValue)
            : base(declaration)
        {
            DefaultValue = defaultValue;
        }

        public override string ToString() => $"&KEY ({Name} {DefaultValue})";

        public override string ToDisplayString(LispPackage currentPackage) => $"&KEY ({Name} {DefaultValue.ToDisplayString(currentPackage)})";
    }

    public class LispAuxiliaryInvocationArgument : LispInvocationArgument
    {
        public LispObject InitialValue { get; }

        internal LispAuxiliaryInvocationArgument(LispSymbol declaration, LispObject initialValue)
            : base(declaration)
        {
            InitialValue = initialValue;
        }

        public override string ToString() => $"&AUX ({Name} {InitialValue})";

        public override string ToDisplayString(LispPackage currentPackage) => $"&AUX ({Name} {InitialValue.ToDisplayString(currentPackage)})";
    }

    public class LispRestInvocationArgument : LispInvocationArgument
    {
        internal LispRestInvocationArgument(LispSymbol declaration)
            : base(declaration)
        {
        }

        public override string ToString() => $"&REST {Name}";
    }
}
