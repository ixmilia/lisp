﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace IxMilia.Lisp
{
    internal class LispObjectReader
    {
        private LispHost _host;
        private LispStream _input;
        private LispObject _nextValue;
        private bool _errorOnEof;
        private LispObject _eofValue;
        private bool _isRecursive;
        private int _line = 1;
        private int _column = 1;
        private int _leftParenCount = 0;

        private static List<Tuple<Regex, Func<Match, LispObject>>> RegexMatchers = new List<Tuple<Regex, Func<Match, LispObject>>>();
        private static Regex ListForwardReferenceRegex = new Regex(@"^#(\d+)=$", RegexOptions.Compiled);

        static LispObjectReader()
        {
            // integer
            RegexMatchers.Add(Tuple.Create<Regex, Func<Match, LispObject>>(new Regex(@"^((\+|-)?\d+)$", RegexOptions.Compiled), (match) =>
            {
                var i = int.Parse(match.Groups[1].Value);
                return new LispInteger(i);
            }));

            // ratio
            RegexMatchers.Add(Tuple.Create<Regex, Func<Match, LispObject>>(new Regex(@"^((\+|-)?\d+)/((\+|-)?\d+)$", RegexOptions.Compiled), (match) =>
            {
                var numerator = int.Parse(match.Groups[1].Value);
                var denominator = int.Parse(match.Groups[3].Value);
                return new LispRatio(numerator, denominator).Reduce();
            }));

            // float
            RegexMatchers.Add(Tuple.Create<Regex, Func<Match, LispObject>>(new Regex(@"^((\+|-)?\d+(\.\d+)?(e(\+|-)?\d+)?)$", RegexOptions.Compiled), (match) =>
            {
                var d = double.Parse(match.Value);
                return new LispFloat(d);
            }));
        }

        public LispObjectReader(LispHost host, LispStream input, bool errorOnEof, LispObject eofValue, bool isRecursive)
        {
            _host = host;
            _input = input;
            _errorOnEof = errorOnEof;
            _eofValue = eofValue;
            _isRecursive = isRecursive;
            Advance();
        }

        public LispObject Read()
        {
            ConsumeTrivia();
            if (!TryPeek(out var next))
            {
                if (_errorOnEof)
                {
                    return new LispError("EOF");
                }

                return _eofValue;
            }

            if (!(next is LispCharacter lc))
            {
                return new LispError("Expected a character");
            }

            LispObject result = null;
            var c = lc.Value;
            if (IsTrivia(c))
            {
                ConsumeTrivia();
            }
            else if (IsLeftParen(c))
            {
                result = ReadList();
            }
            else if (IsSingleQuote(c))
            {
                Advance();
                var innerObject = Read();
                result = LispList.FromItems(new LispSymbol("QUOTE"), innerObject);
            }
            else if (IsDoubleQuote(c))
            {
                Advance();
                result = ReadString();
            }
            else
            {
                var text = ReadUntilTriviaOrListMarker();
                if (text.StartsWith(":"))
                {
                    result = new LispKeyword(text.ToUpperInvariant());
                }
                else if (text.StartsWith("&"))
                {
                    result = new LispLambdaListKeyword(text.ToUpperInvariant());
                }
                else if (text.StartsWith("#'"))
                {
                    result = new LispQuotedNamedFunctionReference(text.Substring(2).ToUpperInvariant());
                }
                else if (text.StartsWith(@"#\"))
                {
                    result = TryAssignCharacter(text.Substring(2));
                }
                else if (ListForwardReferenceRegex.IsMatch(text))
                {
                    var candidateInnerList = Read();
                    if (candidateInnerList is LispList innerList)
                    {
                        var symbolReference = text.Substring(0, text.Length - 1).ToUpperInvariant() + "#";
                        result = new LispForwardListReference(symbolReference, innerList);
                    }
                    else
                    {
                        result = new LispError("Expected list");
                    }
                }
                else
                {
                    var foundRegex = false;
                    foreach (var regexPair in RegexMatchers)
                    {
                        var regex = regexPair.Item1;
                        var creator = regexPair.Item2;
                        var collection = regex.Matches(text);
                        if (collection.Count >= 1)
                        {
                            var match = collection[0];
                            result = creator(match);
                            foundRegex = true;
                            break;
                        }
                    }

                    if (!foundRegex)
                    {
                        if (!IsRightParen(lc.Value))
                        {
                            result = new LispSymbol(text.ToUpperInvariant());
                        }
                        else
                        {
                            result = new LispError($"Unexpected character '{c}' at position ({lc.SourceLocation?.Line}, {lc.SourceLocation?.Column})");
                        }
                    }
                }
            }

            if (result is object &&
                result.SourceLocation == null)
            {
                result.SourceLocation = next.SourceLocation;
            }

            return result;
        }

        private LispObject ReadList()
        {
            var items = new List<LispObject>();
            var tailItems = new List<LispObject>();
            LispSourceLocation? dotLocation = null;
            var isListComplete = false;
            var first = Peek(); // should be `(`
            switch (first)
            {
                case LispCharacter firstCharacter when firstCharacter.Value == '(':
                    _leftParenCount++;
                    break;
                default:
                    throw new Exception("First character should always be `(`");
            }

            Advance();
            ConsumeTrivia();
            var next = Peek();
            while (next is LispCharacter lc)
            {
                var c = lc.Value;
                if (IsRightParen(c))
                {
                    // done
                    Advance();
                    isListComplete = true;
                    _leftParenCount--;
                    break;
                }
                else if (IsPeriod(c))
                {
                    if (dotLocation.HasValue)
                    {
                        var error = new LispError($"Unexpected duplicate '.' in list at ({lc.SourceLocation?.Line}, {lc.SourceLocation?.Column}); first '.' at ({dotLocation.Value.Line}, {dotLocation.Value.Column})");
                        error.SourceLocation = lc.SourceLocation;
                        return error;
                    }

                    Advance();
                    dotLocation = lc.SourceLocation;
                }

                var nextItem = Read();
                if (!dotLocation.HasValue)
                {
                    items.Add(nextItem);
                }
                else
                {
                    tailItems.Add(nextItem);
                }

                ConsumeTrivia();
                next = Peek();
            }

            if (!isListComplete)
            {
                return new LispError($"Unmatched '(' at ({first.SourceLocation?.Line}, {first.SourceLocation?.Column}) (depth {_leftParenCount})");
            }

            LispObject result;
            if (dotLocation.HasValue)
            {
                if (tailItems.Count == 1)
                {
                    // improper list
                    var allItems = items.Concat(tailItems).ToList();
                    result = LispList.FromEnumerableImproper(allItems[0], allItems[1], allItems.Skip(2));
                }
                else
                {
                    result = new LispError("Too many trailing items");
                }
            }
            else
            {
                // proper list
                result = LispList.FromEnumerable(items);
            }

            // set parents
            foreach (var item in items)
            {
                item.Parent = result;
            }

            foreach (var item in tailItems)
            {
                item.Parent = result;
            }

            return result;
        }

        private LispObject ReadString()
        {
            var builder = new StringBuilder();
            var isEscape = false;
            while (TryPeek(out var lc))
            {
                var c = lc.Value;
                if (isEscape)
                {
                    isEscape = false;
                    switch (c)
                    {
                        case 'n':
                            builder.Append('\n');
                            break;
                        case 'f':
                            builder.Append('\f');
                            break;
                        case 't':
                            builder.Append('\t');
                            break;
                        case 'v':
                            builder.Append('\v');
                            break;
                        default:
                            // e.g., \
                            //       "
                            builder.Append(c);
                            break;
                    }
                }
                else
                {
                    if (IsDoubleQuote(c))
                    {
                        Advance();
                        break;
                    }
                    else if (IsBackslash(c))
                    {
                        isEscape = true;
                    }
                    else
                    {
                        builder.Append(c);
                    }
                }

                Advance();
            }

            var text = builder.ToString();
            return new LispString(text);
        }

        private LispObject TryAssignCharacter(string text)
        {
            if (text.Length == 1)
            {
                return new LispCharacter(text[0]);
            }

            // TODO: handle `#\SPACE`, etc.
            return new LispError($"Unexpected character escape sequence '{text}'");
        }

        private LispObject Peek()
        {
            return _nextValue;
        }

        private bool TryPeek(out LispCharacter next)
        {
            if (_nextValue is LispCharacter lc)
            {
                next = lc;
                return true;
            }
            else
            {
                next = null;
                return false;
            }
        }

        private void Advance()
        {
            var executionState = _host.Eval(LispList.FromItems(
                new LispSymbol("READ-CHAR"),
                _input, // input-stream
                _errorOnEof ? _host.T : _host.Nil, // eof-error-p
                _eofValue, // eof-value
                _host.T // recursive-p
            ));
            _nextValue = executionState.LastResult;
            _nextValue.SourceLocation = new LispSourceLocation(_input.Name, _line, _column);
            _column++;
        }

        private static bool IsNewlineLike(char c)
        {
            switch (c)
            {
                case '\r':
                case '\n':
                    return true;
                default:
                    return false;
            }
        }

        private static bool IsWhitespace(char c)
        {
            switch (c)
            {
                case ' ':
                case '\t':
                    return true;
                default:
                    return false;
            }
        }

        private static bool IsSemi(char c)
        {
            return c == ';';
        }

        private static bool IsTrivia(char c)
        {
            return IsNewlineLike(c)
                || IsWhitespace(c)
                || IsSemi(c);
        }

        private static bool IsBackslash(char c)
        {
            return c == '\\';
        }

        private static bool IsPeriod(char c)
        {
            return c == '.';
        }

        private static bool IsLeftParen(char c)
        {
            return c == '(';
        }

        private static bool IsRightParen(char c)
        {
            return c == ')';
        }

        private static bool IsSingleQuote(char c)
        {
            return c == '\'';
        }

        private static bool IsDoubleQuote(char c)
        {
            return c == '"';
        }

        private string ReadUntilTriviaOrListMarker()
        {
            var builder = new StringBuilder();
            while (TryPeek(out var lc))
            {
                // TODO: this is too agressive for `#\(` and `#\)`
                var c = lc.Value;
                if (IsTrivia(c) ||
                    IsLeftParen(c) ||
                    IsRightParen(c))
                {
                    break;
                }

                builder.Append(c);
                Advance();
            }

            var text = builder.ToString();
            return text;
        }

        private void ConsumeTrivia()
        {
            while (TryPeek(out var c))
            {
                if (IsWhitespace(c.Value))
                {
                    ConsumeWhitespaceTrivia();
                }
                else if (IsNewlineLike(c.Value))
                {
                    ConsumeNewlineTrivia();
                }
                else if (IsSemi(c.Value))
                {
                    ConsumeCommentTrivia();
                }
                else
                {
                    return;
                }
            }
        }

        private void ConsumeWhitespaceTrivia()
        {
            if (!TryPeek(out var ws) || !IsWhitespace(ws.Value))
            {
                return;
            }

            Advance();
            while (TryPeek(out var c) && IsWhitespace(c.Value))
            {
                Advance();
            }
        }

        private void ConsumeNewlineTrivia()
        {
            if (!TryPeek(out var c) || !IsNewlineLike(c.Value))
            {
                return;
            }

            Advance();
            if (c.Value != '\n')
            {
                // maybe consume one more character
                if (TryPeek(out c) && c.Value == '\n')
                {
                    Advance();
                }
            }

            _line++;
            _column = 1;
        }

        private void ConsumeCommentTrivia()
        {
            if (!TryPeek(out var semi) || !IsSemi(semi.Value))
            {
                return;
            }

            Advance();
            while (TryPeek(out var c) && c.Value != '\n')
            {
                Advance();
            }
        }
    }
}
