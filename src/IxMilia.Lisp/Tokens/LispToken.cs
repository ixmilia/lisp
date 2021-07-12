using System.Diagnostics;
using System.Text;

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

    public class LispSingleQuoteToken : LispToken
    {
        public override LispTokenType Type => LispTokenType.SingleQuote;

        public override string ToString()
        {
            return "'";
        }
    }

    public class LispKeywordToken : LispToken
    {
        public override LispTokenType Type => LispTokenType.Keyword;

        public string Keyword { get; }

        public LispKeywordToken(string keyword)
        {
            Debug.Assert(keyword.StartsWith(":"));
            Keyword = keyword;
        }

        public override string ToString()
        {
            return Keyword;
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

    internal class LispQuotedFunctionToken : LispToken
    {
        public override LispTokenType Type => (LispTokenType)(-1); // not used

        public LispQuotedFunctionToken()
        {
        }
    }

    public class LispSymbolToken : LispToken
    {
        public override LispTokenType Type => LispTokenType.Symbol;
        public string Value { get; }

        public LispSymbolToken(string value)
        {
            Value = value;
        }

        public override string ToString()
        {
            return Value;
        }
    }

    public class LispIntegerToken : LispToken
    {
        public override LispTokenType Type => LispTokenType.Integer;
        public int Value { get; }
        public string Text { get; }

        public LispIntegerToken(int value, string text)
        {
            Value = value;
            Text = text;
        }

        public override string ToString()
        {
            return Text;
        }
    }

    public class LispFloatToken : LispToken
    {
        public override LispTokenType Type => LispTokenType.Float;
        public double Value { get; }
        public string Text { get; }

        public LispFloatToken(double value, string text)
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
            return ToRoundTrippable(Value);
        }

        internal static string ToRoundTrippable(string s)
        {
            var sb = new StringBuilder();
            sb.Append('"');
            foreach (var c in s)
            {
                switch (c)
                {
                    case '"':
                    case '\\':
                        sb.Append("\\");
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
    }
}
