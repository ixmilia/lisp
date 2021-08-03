namespace IxMilia.Lisp
{
    public abstract class LispFunctionArgument
    {
        public string Name { get; }

        protected LispFunctionArgument(string name)
        {
            Name = name;
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

    public class LispKeywordFunctionArgument : LispFunctionArgument
    {
        public LispObject DefaultValue { get; }

        internal LispKeywordFunctionArgument(string name, LispObject defaultValue)
            : base(name)
        {
            DefaultValue = defaultValue;
        }

        public override string ToString()
        {
            return $"&key ({Name} {DefaultValue})";
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
