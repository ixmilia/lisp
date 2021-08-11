namespace IxMilia.Lisp
{
    public abstract class LispFunctionArgument
    {
        public LispSymbol Declaration { get; }
        public string Name => Declaration.Value;

        protected LispFunctionArgument(LispSymbol declaration)
        {
            Declaration = declaration;
        }
    }

    public class LispRegularFunctionArgument : LispFunctionArgument
    {
        internal LispRegularFunctionArgument(LispSymbol declaration)
            : base(declaration)
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

        internal LispOptionalFunctionArgument(LispSymbol declaration, LispObject defaultValue)
            : base(declaration)
        {
            DefaultValue = defaultValue;
        }

        public override string ToString()
        {
            return $"&optional ({Name} {DefaultValue})";
        }
    }

    public class LispKeywordFunctionArgument : LispFunctionArgument
    {
        public LispObject DefaultValue { get; }

        internal LispKeywordFunctionArgument(LispSymbol declaration, LispObject defaultValue)
            : base(declaration)
        {
            DefaultValue = defaultValue;
        }

        public override string ToString()
        {
            return $"&key ({Name} {DefaultValue})";
        }
    }

    public class LispAuxiliaryFunctionArgument : LispFunctionArgument
    {
        public LispObject InitialValue { get; }

        internal LispAuxiliaryFunctionArgument(LispSymbol declaration, LispObject initialValue)
            : base(declaration)
        {
            InitialValue = initialValue;
        }

        public override string ToString()
        {
            return $"&aux ({Name} {InitialValue})";
        }
    }

    public class LispRestFunctionArgument : LispFunctionArgument
    {
        internal LispRestFunctionArgument(LispSymbol declaration)
            : base(declaration)
        {
        }

        public override string ToString()
        {
            return $"&rest {Name}";
        }
    }
}
