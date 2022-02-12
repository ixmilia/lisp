namespace IxMilia.Lisp
{
    public abstract class LispInvocationArgument
    {
        public LispSymbol Declaration { get; }
        public string Name => Declaration.LocalName;

        protected LispInvocationArgument(LispSymbol declaration)
        {
            Declaration = declaration;
        }
    }

    public class LispRegularInvocationArgument : LispInvocationArgument
    {
        internal LispRegularInvocationArgument(LispSymbol declaration)
            : base(declaration)
        {
        }

        public override string ToString()
        {
            return Name;
        }
    }

    public class LispOptionalInvocationArgument : LispInvocationArgument
    {
        public LispObject DefaultValue { get; }

        internal LispOptionalInvocationArgument(LispSymbol declaration, LispObject defaultValue)
            : base(declaration)
        {
            DefaultValue = defaultValue;
        }

        public override string ToString()
        {
            return $"&OPTIONAL ({Name} {DefaultValue})";
        }
    }

    public class LispKeywordInvocationArgument : LispInvocationArgument
    {
        public LispObject DefaultValue { get; }

        internal LispKeywordInvocationArgument(LispSymbol declaration, LispObject defaultValue)
            : base(declaration)
        {
            DefaultValue = defaultValue;
        }

        public override string ToString()
        {
            return $"&KEY ({Name} {DefaultValue})";
        }
    }

    public class LispAuxiliaryInvocationArgument : LispInvocationArgument
    {
        public LispObject InitialValue { get; }

        internal LispAuxiliaryInvocationArgument(LispSymbol declaration, LispObject initialValue)
            : base(declaration)
        {
            InitialValue = initialValue;
        }

        public override string ToString()
        {
            return $"&AUX ({Name} {InitialValue})";
        }
    }

    public class LispRestInvocationArgument : LispInvocationArgument
    {
        internal LispRestInvocationArgument(LispSymbol declaration)
            : base(declaration)
        {
        }

        public override string ToString()
        {
            return $"&REST {Name}";
        }
    }
}
