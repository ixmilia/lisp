﻿using System;
using System.Collections.Generic;
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

        private static List<Tuple<Regex, Func<Match, LispObject>>> RegexMatchers = new List<Tuple<Regex, Func<Match, LispObject>>>();

        static LispObjectReader()
        {
            // integer
            RegexMatchers.Add(Tuple.Create<Regex, Func<Match, LispObject>>(new Regex(@"^((\+|-)?\d+)$", RegexOptions.Compiled), (match) =>
            {
                var i = int.Parse(match.Groups[1].Value);
                return new LispInteger(i);
            }));

            // ratio
            RegexMatchers.Add(Tuple.Create<Regex, Func<Match, LispObject>>(new Regex(@"^((\+|-)?\d+)/(\d+)$", RegexOptions.Compiled), (match) =>
            {
                var numerator = int.Parse(match.Groups[1].Value);
                var denominator = int.Parse(match.Groups[3].Value);
                return new LispRatio(numerator, denominator);
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
            var next = Peek();
            if (next.IsNil())
            {
                return _eofValue ?? new LispError("EOF");
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
                Advance();
                result = ReadList();
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
                        result = new LispError($"Unexpected character '{c}' at position ({_line}, {_column})");
                    }
                }
            }

            if (result is object)
            {
                result.SourceLocation = new LispSourceLocation("", _line, _column);
            }

            return result;
        }

        private LispObject ReadList()
        {
            var items = new List<LispObject>();
            ConsumeTrivia();
            var next = Peek();
            while (!next.IsNil())
            {
                if (!(next is LispCharacter lc))
                {
                    return new LispError("Exepcted a character");
                }

                var c = lc.Value;
                if (IsRightParen(c))
                {
                    // done
                    Advance();
                    break;
                }

                var nextItem = Read();
                items.Add(nextItem);
                ConsumeTrivia();
                next = Peek();
            }

            var result = LispList.FromEnumerable(items);
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
                    else if (c == '\\')
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
                _eofValue is object ? _host.T : _host.Nil, // eof-error-p
                _eofValue ?? _host.Nil, // eof-value
                _host.T // recursive-p
            ));
            _nextValue = executionState.LastResult;
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

        private static bool IsLeftParen(char c)
        {
            return c == '(';
        }

        private static bool IsRightParen(char c)
        {
            return c == ')';
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
