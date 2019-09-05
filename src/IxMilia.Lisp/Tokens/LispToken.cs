using System.Diagnostics;

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

    public class LispErrorToken : LispToken
    {
        public override LispTokenType Type => LispTokenType.Error;
        public string Value { get; }
        public string Message { get; }

        public LispErrorToken(string value, string message)
        {
            Value = value;
            Message = message;
        }
    }

    public class LispLeftParenToken : LispToken
    {
        public override LispTokenType Type => LispTokenType.LeftParen;
        public bool IsQuoted { get; }

        public LispLeftParenToken(bool isQuoted)
        {
            IsQuoted = isQuoted;
        }

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

    public class LispDotToken : LispToken
    {
        public override LispTokenType Type => LispTokenType.Dot;

        public override string ToString()
        {
            return ".";
        }
    }

    internal class LispForwardReferenceToken : LispToken
    {
        public override LispTokenType Type => (LispTokenType)(-1); // not used
        public string SymbolReference { get; }

        public LispForwardReferenceToken(string value)
        {
            // e.g., value = "#123="
            Debug.Assert(value.StartsWith("#"));
            Debug.Assert(value.EndsWith("="));
            SymbolReference = value.Substring(0, value.Length - 1) + "#"; // e.g., #123#
        }
    }

    public class LispSymbolToken : LispToken
    {
        public override LispTokenType Type => LispTokenType.Symbol;
        public bool IsQuoted { get; }
        public string Value { get; }

        public LispSymbolToken(bool isQuoted, string value)
        {
            IsQuoted = isQuoted;
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
