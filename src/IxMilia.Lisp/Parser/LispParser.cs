using System.Collections.Generic;
using System.Linq;
using IxMilia.Lisp.Tokens;

namespace IxMilia.Lisp.Parser
{
    public class LispParser
    {
        private int _index;
        private LispToken[] _tokens;
        private Stack<LispLeftParenToken> _leftParens = new Stack<LispLeftParenToken>();

        public LispParser(IEnumerable<LispToken> tokens)
        {
            _index = 0;
            _tokens = tokens.ToArray();
        }

        public IEnumerable<LispObject> Parse()
        {
            while (TryParseExpression(out var expression))
            {
                yield return expression;
            }
        }

        private bool TryParseExpression(out LispObject result)
        {
            result = default(LispObject);
            while (TryPeek(out var token))
            {
                switch (token)
                {
                    case LispSymbolToken symbol:
                        Advance();
                        result = new LispSymbol(symbol.IsQuoted, symbol.Value);
                        break;
                    case LispNumberToken num:
                        Advance();
                        result = new LispNumber(num.Value);
                        break;
                    case LispStringToken str:
                        Advance();
                        result = new LispString(str.Value);
                        break;
                    case LispLeftParenToken left:
                        Advance();
                        _leftParens.Push(left);
                        result = ParseList(left);
                        break;
                    case LispRightParenToken _:
                        // This should have been handled in `ParseList()`
                        Advance();
                        result = new LispError($"Unexpected ')' at ({token.Line}, {token.Column})");
                        break;
                }

                if (result != null)
                {
                    result.Line = token.Line;
                    result.Column = token.Column;
                    return true;
                }
            }

            return false;
        }

        private LispObject ParseList(LispLeftParenToken left)
        {
            var elements = new List<LispObject>();
            LispRightParenToken rightParen = null;
            while (rightParen == null && TryPeek(out var token))
            {
                switch (token.Type)
                {
                    case LispTokenType.RightParen:
                        Advance();
                        _leftParens.Pop();
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

            if (rightParen == null)
            {
                return new LispError($"Unmatched '(' at ({_leftParens.Peek().Line}, {_leftParens.Peek().Column}) (depth {_leftParens.Count})");
            }

            if (elements.Any())
            {
                var result = LispList.FromEnumerable(elements);
                result.IsQuoted = left.IsQuoted;
                return result;
            }
            else
            {
                return LispNilList.Instance;
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
