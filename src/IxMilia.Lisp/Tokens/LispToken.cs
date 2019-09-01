namespace IxMilia.Lisp.Tokens
{
    public abstract class LispToken
    {
        public abstract LispTokenType Type { get; }
        public int Line { get; internal set; }
        public int Column { get; internal set; }
        public LispTriviaCollection LeadingTrivia { get; internal set; }
        public LispTriviaCollection TrailingTrivia { get; internal set; }

        protected LispToken()
        {
            Line = 0;
            Column = 0;
            LeadingTrivia = new LispTriviaCollection();
            TrailingTrivia = new LispTriviaCollection();
        }
    }

    public class LispLeftParenToken : LispToken
    {
        public override LispTokenType Type => LispTokenType.LeftParen;

        public override string ToString()
        {
            return "(";
        }
    }

    public class LispRightParenToken : LispToken
    {
        public override LispTokenType Type => LispTokenType.RightParen;

        public override string ToString()
        {
            return ")";
        }
    }

    public class LispAtomToken : LispToken
    {
        public override LispTokenType Type => LispTokenType.Atom;
        public string Value { get; }

        public LispAtomToken(string value)
        {
            Value = value;
        }

        public override string ToString()
        {
            return Value;
        }
    }

    public class LispNumberToken : LispToken
    {
        public override LispTokenType Type => LispTokenType.Number;
        public double Value { get; }
        public string Text { get; }

        public LispNumberToken(double value, string text)
        {
            Value = value;
            Text = text;
        }

        public override string ToString()
        {
            return Text;
        }
    }

    public class LispStringToken : LispToken
    {
        public override LispTokenType Type => LispTokenType.String;
        public string Value { get; }

        public LispStringToken(string text)
        {
            Value = text;
        }

        public override string ToString()
        {
            return Value;
        }
    }
}
