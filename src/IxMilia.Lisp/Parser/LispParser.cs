using System.Collections.Generic;
using System.Linq;
using IxMilia.Lisp.Tokens;

namespace IxMilia.Lisp.Parser
{
    public class LispParser
    {
        private int _index;
        private List<LispToken> _tokens;
        private Stack<LispLeftParenToken> _leftParens = new Stack<LispLeftParenToken>();
        private bool _errorOnIncompleteExpressions;

        public LispParser(IEnumerable<LispToken> tokens = null, bool errorOnIncompleteExpressions = true)
        {
            tokens = tokens ?? Enumerable.Empty<LispToken>();
            _index = 0;
            _tokens = tokens.ToList();
            _errorOnIncompleteExpressions = errorOnIncompleteExpressions;
        }

        public LispParseResult Parse()
        {
            _leftParens.Clear();
            var nodes = ParseNodes().ToList();
            return new LispParseResult(nodes, _leftParens.Count);
        }

        private IEnumerable<LispObject> ParseNodes()
        {
            var startIndex = _index;
            while (TryParseNullExpression(out var expression) && expression != null)
            {
                startIndex = _index;
                yield return expression;
            }

            _index = startIndex;
            TrimUsedTokens();
        }

        private void TrimUsedTokens()
        {
            _tokens.RemoveRange(0, _index);
            _index = 0;
        }

        public void AddTokens(IEnumerable<LispToken> tokens)
        {
            _tokens.AddRange(tokens);
        }

        private bool TryParseNullExpression(out LispObject result)
        {
            result = null;
            while (TryPeek(out var token))
            {
                switch (token)
                {
                    case LispErrorToken error:
                        Advance();
                        result = new LispError(error.Message);
                        break;
                    case LispSingleQuoteToken _:
                        Advance();
                        result = ParseQuotedObject();
                        break;
                    case LispSymbolToken symbol:
                        Advance();
                        result = new LispSymbol(symbol.Value);
                        break;
                    case LispKeywordToken keyword:
                        Advance();
                        result = new LispKeyword(keyword.Keyword);
                        break;
                    case LispIntegerToken num:
                        Advance();
                        result = new LispInteger(num.Value);
                        break;
                    case LispFloatToken num:
                        Advance();
                        result = new LispFloat(num.Value);
                        break;
                    case LispStringToken str:
                        Advance();
                        result = new LispString(str.Value);
                        break;
                    case LispLeftParenToken left:
                        Advance();
                        _leftParens.Push(left);
                        result = ParseList();
                        break;
                    case LispForwardReferenceToken forwardRef:
                        Advance();
                        result = ParseForwardReferenceList(forwardRef);
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
                else if (_errorOnIncompleteExpressions)
                {
                    result = new LispError("Expected expression");
                    return true;
                }
                else
                {
                    return false;
                }
            }

            return false;
        }

        private LispObject ParseQuotedObject()
        {
            if (TryParseNullExpression(out var value) && value != null)
            {
                return new LispQuotedObject(value);
            }
            else if (_errorOnIncompleteExpressions)
            {
                return new LispError("Expected expression");
            }
            else
            {
                return null;
            }
        }

        private LispObject ParseForwardReferenceList(LispForwardReferenceToken forwardRef)
        {
            if (TryPeek(out var token) && token is LispLeftParenToken leftParen)
            {
                Advance();
                _leftParens.Push(leftParen);
                var next = ParseList();
                if (next is LispList innerList)
                {
                    return new LispForwardListReference(forwardRef, innerList);
                }
                else if (_errorOnIncompleteExpressions)
                {
                    return new LispError("Expected list");
                }
                else
                {
                    return null;
                }
            }
            else if (_errorOnIncompleteExpressions)
            {
                return new LispError("Expected following list");
            }
            else
            {
                return null;
            }
        }

        private LispObject ParseList()
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
                        if (TryParseNullExpression(out var element))
                        {
                            if (element == null)
                            {
                                if (_errorOnIncompleteExpressions)
                                {
                                    return new LispError("Expected element");
                                }
                                else
                                {
                                    return null;
                                }
                            }
                            else if (dot == null)
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
                if (_errorOnIncompleteExpressions)
                {
                    return new LispError($"Unmatched '(' at ({_leftParens.Peek().Line}, {_leftParens.Peek().Column}) (depth {_leftParens.Count})");
                }
                else
                {
                    return null;
                }
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
                        return new LispError("Unexpected trailing objects");
                    }
                    else if (tailElements.Count > 1)
                    {
                        return new LispError("Unexpected multiple trailing objects");
                    }
                    else
                    {
                        // there are at least 2 items in total here
                        var allItems = elements.Concat(tailElements).ToList();
                        result = LispList.FromEnumerableImproper(allItems[0], allItems[1], allItems.Skip(2));
                    }
                }

                return result;
            }
            else
            {
                return LispNilList.Instance;
            }
        }

        private bool TryPeek(out LispToken token)
        {
            if (_index < _tokens.Count)
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
