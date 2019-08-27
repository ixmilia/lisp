using System.Collections.Generic;
using IxMilia.Lisp.Tokens;

namespace IxMilia.Lisp.Parser
{
    public abstract class LispSyntax
    {
        public abstract LispSyntaxType Type { get; }
    }

    public class LispAtomSyntax : LispSyntax
    {
        public override LispSyntaxType Type => LispSyntaxType.Atom;
        public LispAtomToken Atom { get; }

        public LispAtomSyntax(LispAtomToken atom)
        {
            Atom = atom;
        }

        public override string ToString()
        {
            return Atom.ToString();
        }
    }

    public class LispNumberSyntax : LispSyntax
    {
        public override LispSyntaxType Type => LispSyntaxType.Number;
        public LispNumberToken Number { get; }

        public LispNumberSyntax(LispNumberToken number)
        {
            Number = number;
        }

        public override string ToString()
        {
            return Number.ToString();
        }
    }

    public class LispStringSyntax : LispSyntax
    {
        public override LispSyntaxType Type => LispSyntaxType.String;
        public LispStringToken String { get; }

        public LispStringSyntax(LispStringToken str)
        {
            String = str;
        }

        public override string ToString()
        {
            return String.ToString();
        }
    }

    public class LispAbstractListSyntax : LispSyntax
    {
        public override LispSyntaxType Type => LispSyntaxType.List;
        public LispToken OpenToken { get; }
        public IEnumerable<LispSyntax> Elements { get; }
        public LispRightParenToken RightParen { get; }

        public LispAbstractListSyntax(LispToken open, IEnumerable<LispSyntax> elements, LispRightParenToken rightParen)
        {
            OpenToken = open;
            Elements = elements;
            RightParen = rightParen;
        }

        public override string ToString()
        {
            return string.Concat(OpenToken, string.Join(" ", Elements), RightParen);
        }
    }

    public class LispListSyntax : LispAbstractListSyntax
    {
        public LispLeftParenToken LeftParen { get; }
        public LispListSyntax(LispLeftParenToken leftParen, IEnumerable<LispSyntax> elements, LispRightParenToken rightParen)
            : base(leftParen, elements, rightParen)
        {
            LeftParen = leftParen;
        }
    }

    public class LispRawListSyntax : LispAbstractListSyntax
    {
        public override LispSyntaxType Type => LispSyntaxType.RawList;
        public LispSingleQuotedLeftParenToken LeftParen { get; }
        public LispRawListSyntax(LispSingleQuotedLeftParenToken leftParen, IEnumerable<LispSyntax> elements, LispRightParenToken rightParen)
            : base(leftParen, elements, rightParen)
        {
            LeftParen = leftParen;
        }
    }
}
