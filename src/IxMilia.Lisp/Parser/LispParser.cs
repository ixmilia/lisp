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
                    case LispDotToken _:
                        // This should have been handled in `ParseList()`
                        Advance();
                        result = new LispError($"Unexpected '.' at ({token.Line}, {token.Column})");
                        break;
                    case LispRightParenToken _:
                        // This should have been handled in `ParseList()`
                        Advance();
                        result = new LispError($"Unexpected ')' at ({token.Line}, {token.Column})");
                        break;
                }

                if (result != null)
                {
                    if (result.Line == 0 && result.Column == 0)
                    {
                        result.Line = token.Line;
                        result.Column = token.Column;
                    }

                    return true;
                }
            }

            return false;
        }

        private LispObject ParseList(LispLeftParenToken left)
        {
            var elements = new List<LispObject>();
            var tailElements = new List<LispObject>();
            LispDotToken dot = null;
            LispRightParenToken rightParen = null;
            while (rightParen == null && TryPeek(out var token))
            {
                switch (token.Type)
                {
                    case LispTokenType.Dot:
                        Advance();
                        if (dot == null)
                        {
                            dot = (LispDotToken)token;
                        }
                        else
                        {
                            return new LispError($"Unexpected duplicate '.' in list at ({token.Line}, {token.Column}); first '.' at ({dot.Line}, {dot.Column})")
                            {
                                Line = token.Line,
                                Column = token.Column
                            };
                        }
                        break;
                    case LispTokenType.RightParen:
                        Advance();
                        _leftParens.Pop();
                        rightParen = (LispRightParenToken)token;
                        break;
                    default:
                        if (TryParseExpression(out var element))
                        {
                            if (dot == null)
                            {
                                elements.Add(element);
                            }
                            else
                            {
                                tailElements.Add(element);
                            }
                        }
                        break;
                }
            }

            if (rightParen == null)
            {
                return new LispError($"Unmatched '(' at ({_leftParens.Peek().Line}, {_leftParens.Peek().Column}) (depth {_leftParens.Count})");
            }

            if (elements.Any() || tailElements.Any())
            {
                LispList result;
                if (dot == null)
                {
                    result = LispList.FromEnumerable(elements);
                }
                else
                {
                    if (!tailElements.Any())
                    {
                        return new LispError("");
                    }
                    else if (tailElements.Count > 1)
                    {
                        return new LispError("");
                    }
                    else
                    {
                        // there are at least 2 items in total here
                        var allItems = elements.Concat(tailElements).ToList();
                        result = LispList.FromEnumerableImproper(allItems[0], allItems[1], allItems.Skip(2));
                    }
                }

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
