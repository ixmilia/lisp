using System;
using System.Collections.Generic;
using System.Linq;
using IxMilia.Lisp.Tokens;

namespace IxMilia.Lisp.Parser
{
    public class LispParser
    {
        private int _index;
        private LispToken[] _tokens;

        public LispParser(IEnumerable<LispToken> tokens)
        {
            _index = 0;
            _tokens = tokens.ToArray();
        }

        public IEnumerable<LispSyntax> Parse()
        {
            while (TryParseExpression(out var expression))
            {
                yield return expression;
            }
        }

        private bool TryParseExpression(out LispSyntax result)
        {
            result = default(LispSyntax);
            while (TryPeek(out var token))
            {
                switch (token.Type)
                {
                    case LispTokenType.Atom:
                        Advance();
                        result = new LispAtomSyntax((LispAtomToken)token);
                        break;
                    case LispTokenType.Number:
                        Advance();
                        result = new LispNumberSyntax((LispNumberToken)token);
                        break;
                    case LispTokenType.String:
                        Advance();
                        result = new LispStringSyntax((LispStringToken)token);
                        break;
                    case LispTokenType.LeftParen:
                    case LispTokenType.SingleQuotedLeftParen:
                        Advance();
                        result = ParseList(token);
                        break;
                }

                if (result != null)
                {
                    return true;
                }
            }

            return false;
        }

        private LispSyntax ParseList(LispToken first)
        {
            var elements = new List<LispSyntax>();
            LispRightParenToken rightParen = null;
            while (rightParen == null && TryPeek(out var token))
            {
                switch (token.Type)
                {
                    case LispTokenType.RightParen:
                        Advance();
                        rightParen = (LispRightParenToken)token;
                        break;
                    default:
                        if (TryParseExpression(out var element))
                        {
                            elements.Add(element);
                        }
                        break;
                }
            }

            switch (first.Type)
            {
                case LispTokenType.LeftParen:
                    return new LispListSyntax((LispLeftParenToken)first, elements, rightParen);
                case LispTokenType.SingleQuotedLeftParen:
                    return new LispRawListSyntax((LispSingleQuotedLeftParenToken)first, elements, rightParen);
                default:
                    throw new Exception("improper list start");
            }
        }

        private bool TryPeek(out LispToken token)
        {
            if (_index < _tokens.Length)
            {
                token = _tokens[_index];
                return true;
            }

            token = default(LispToken);
            return false;
        }

        private void Advance()
        {
            _index++;
        }
    }
}
