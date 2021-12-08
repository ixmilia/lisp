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
            return new LispParseResult(nodes, _leftParens.Count, _tokens);
        }

        private IEnumerable<LispObject> ParseNodes()
        {
            var startIndex = _index;
            while (TryParseNullExpression(null, out var expression) && expression != null)
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

        private bool TryParseNullExpression(LispObject parent, out LispObject result)
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
                        result = ParseQuotedObject(parent);
                        break;
                    case LispSymbolToken symbol:
                        Advance();
                        result = new LispSymbol(symbol.Value);
                        break;
                    case LispKeywordToken keyword:
                        Advance();
                        result = new LispKeyword(keyword.Keyword);
                        break;
                    case LispLambdaListKeywordToken lambdaListKeyword:
                        Advance();
                        result = new LispLambdaListKeyword(lambdaListKeyword.Keyword);
                        break;
                    case LispIntegerToken num:
                        Advance();
                        result = new LispInteger(num.Value);
                        break;
                    case LispFloatToken num:
                        Advance();
                        result = new LispFloat(num.Value);
                        break;
                    case LispCharacterToken c:
                        Advance();
                        result = new LispCharacter(c.Value);
                        break;
                    case LispStringToken str:
                        Advance();
                        result = new LispString(str.Value);
                        break;
                    case LispLeftParenToken left:
                        Advance();
                        _leftParens.Push(left);
                        result = ParseList(parent);
                        break;
                    case LispForwardReferenceToken forwardRef:
                        Advance();
                        result = ParseForwardReferenceList(parent, forwardRef);
                        break;
                    case LispQuotedFunctionToken functionQuote:
                        Advance();
                        result = ParseQuotedFunction(parent, functionQuote);
                        break;
                    case LispDotToken _:
                        // This should have been handled in `ParseList()`
                        Advance();
                        result = new LispError($"Unexpected '.' at ({token.SourceLocation?.Line}, {token.SourceLocation?.Column})");
                        break;
                    case LispRightParenToken _:
                        // This should have been handled in `ParseList()`
                        Advance();
                        result = new LispError($"Unexpected ')' at ({token.SourceLocation?.Line}, {token.SourceLocation?.Column})");
                        break;
                }

                if (result != null)
                {
                    result.Parent = parent;
                    if (!result.SourceLocation.HasValue)
                    {
                        result.SourceLocation = token.SourceLocation;
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

        private LispObject ParseQuotedObject(LispObject parent)
        {
            if (TryParseNullExpression(parent, out var value) && value != null)
            {
                return LispList.FromItems(new LispSymbol("QUOTE"), value);
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

        private LispObject ParseForwardReferenceList(LispObject parent, LispForwardReferenceToken forwardRef)
        {
            if (TryPeek(out var token) && token is LispLeftParenToken leftParen)
            {
                Advance();
                _leftParens.Push(leftParen);
                var next = ParseList(parent);
                if (next is LispList innerList)
                {
                    return new LispForwardListReference(forwardRef.SymbolReference, innerList);
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

        private LispObject ParseQuotedFunction(LispObject parent, LispToken functionQuote)
        {
            if (TryPeek(out var token))
            {
                if (token is LispSymbolToken symbol)
                {
                    // named function
                    Advance();
                    var functionName = symbol.Value;
                    return new LispQuotedNamedFunctionReference(functionName);
                }
                else if (token is LispLeftParenToken leftParen)
                {
                    // lambda function
                    _leftParens.Push(leftParen);
                    Advance();
                    var lambdaCandidate = ParseList(parent);
                    if (lambdaCandidate is LispError error)
                    {
                        return error;
                    }

                    if (lambdaCandidate is LispList lambdaList &&
                        lambdaList.Value is LispSymbol lambdaSymbol &&
                        lambdaSymbol.Value == "LAMBDA")
                    {
                        var name = $"(LAMBDA-{functionQuote.SourceLocation?.Line}-{functionQuote.SourceLocation?.Column})"; // surrounded by parens to make it un-utterable
                        var lambdaItems = new List<LispObject>();
                        lambdaItems.Add(new LispSymbol(name));
                        lambdaItems.AddRange(lambdaList.ToList().Skip(1));

                        if (!LispDefaultContext.TryGetCodeFunctionFromItems(lambdaItems.ToArray(), out var lambdaFunction, out error))
                        {
                            return error;
                        }

                        return new LispQuotedLambdaFunctionReference(lambdaFunction);
                    }

                    return new LispError("Expected lambda definition");
                }
            }

            return new LispError("Expected function symbol or lambda expression");
        }

        private LispObject ParseList(LispObject parent)
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
                            return new LispError($"Unexpected duplicate '.' in list at ({token.SourceLocation?.Line}, {token.SourceLocation?.Column}); first '.' at ({dot.SourceLocation?.Line}, {dot.SourceLocation?.Column})")
                            {
                                SourceLocation = token.SourceLocation,
                            };
                        }
                        break;
                    case LispTokenType.RightParen:
                        Advance();
                        _leftParens.Pop();
                        rightParen = (LispRightParenToken)token;
                        break;
                    default:
                        if (TryParseNullExpression(parent, out var element))
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
                    return new LispError($"Unmatched '(' at ({_leftParens.Peek().SourceLocation?.Line}, {_leftParens.Peek().SourceLocation?.Column}) (depth {_leftParens.Count})");
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

                // set the newly created list as the parent
                foreach (var element in elements)
                {
                    element.Parent = result;
                }

                foreach (var element in tailElements)
                {
                    element.Parent = result;
                }

                return result;
            }
            else
            {
                return LispNilList.CreateForParser();
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
