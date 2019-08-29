using System.Collections.Generic;
using IxMilia.Lisp.Tokens;

namespace IxMilia.Lisp.Parser
{
    public abstract class LispSyntax
    {
        public abstract LispSyntaxType Type { get; }
        internal abstract LispToken FirstToken { get; }
    }

    public class LispAtomSyntax : LispSyntax
    {
        public override LispSyntaxType Type => LispSyntaxType.Atom;
        public LispAtomToken Atom { get; }
        internal override LispToken FirstToken => Atom;

        public LispAtomSyntax(LispAtomToken atom)
        {
            Atom = atom;
        }

        public override string ToString()
        {
            return Atom.ToString();
        }
    }

    public class LispNilSyntax : LispAtomSyntax
    {
        public LispNilSyntax(LispNilToken nil)
            : base(nil)
        {
        }
    }

    public class LispTSyntax : LispAtomSyntax
    {
        public LispTSyntax(LispTToken t)
            : base(t)
        {
        }
    }

    public class LispNumberSyntax : LispSyntax
    {
        public override LispSyntaxType Type => LispSyntaxType.Number;
        public LispNumberToken Number { get; }
        internal override LispToken FirstToken => Number;

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
        internal override LispToken FirstToken => String;

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
        internal override LispToken FirstToken => OpenToken;

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
