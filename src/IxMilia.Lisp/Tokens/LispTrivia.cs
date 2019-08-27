namespace IxMilia.Lisp.Tokens
{
    public abstract class LispTrivia
    {
        public abstract LispTriviaType Type { get; }
        public string Value { get; }

        protected LispTrivia(string value)
        {
            Value = value;
        }

        public override string ToString()
        {
            return Value.ToString();
        }
    }

    public class LispWhitespaceTrivia : LispTrivia
    {
        public override LispTriviaType Type => LispTriviaType.Whitespace;

        public LispWhitespaceTrivia(string value)
            : base(value)
        {
        }
    }

    public class LispNewlineTrivia : LispTrivia
    {
        public override LispTriviaType Type => LispTriviaType.Whitespace;

        public LispNewlineTrivia(string value)
            : base(value)
        {
        }
    }

    public class LispCommentTrivia : LispTrivia
    {
        public override LispTriviaType Type => LispTriviaType.Whitespace;

        public LispCommentTrivia(string value)
            : base(value)
        {
        }
    }

    public class LispShebangTrivia : LispTrivia
    {
        public override LispTriviaType Type => LispTriviaType.Whitespace;

        public LispShebangTrivia(string value)
            : base(value)
        {
        }
    }
}
