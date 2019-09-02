using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace IxMilia.Lisp.Tokens
{
    public class LispTokenizer
    {
        private int _line;
        private int _column;
        private bool _tokenStartSet;
        private int _tokenStartLine;
        private int _tokenStartColumn;
        private int _offset;
        private char[] _characters;
        private bool _isQuoting;
        private List<LispTrivia> _triviaBuilder;

        public LispTokenizer(char[] chars)
        {
            _characters = chars;
            _line = 1;
            _column = 1;
            _offset = 0;
            _triviaBuilder = new List<LispTrivia>();
        }

        public LispTokenizer(string code)
            : this(code.ToCharArray())
        {
        }

        public IEnumerable<LispToken> GetTokens()
        {
            while (TryPeek(out var c))
            {
                if (IsTrivia(c))
                {
                    ConsumeTrivia();
                }
                else if (IsSingleQuote(c))
                {
                    Advance();
                    _isQuoting = true;
                }
                else if (IsLeftParen(c))
                {
                    MarkTokenStart();
                    yield return ApplyProperties(ParseLeftParen());
                }
                else if (IsRightParen(c))
                {
                    MarkTokenStart();
                    yield return ApplyProperties(ParseRightParen());
                }
                else if (IsMinus(c))
                {
                    MarkTokenStart();
                    Advance();
                    if (TryPeek(out c) && IsDigit(c))
                    {
                        yield return ApplyProperties(ParseNumber("-"));
                    }
                    else
                    {
                        yield return ApplyProperties(ParseSymbol("-"));
                    }
                }
                else if (IsPlus(c))
                {
                    MarkTokenStart();
                    Advance();
                    if (TryPeek(out c) && IsDigit(c))
                    {
                        yield return ApplyProperties(ParseNumber("+"));
                    }
                    else
                    {
                        yield return ApplyProperties(ParseSymbol("+"));
                    }
                }
                else if (IsDigit(c))
                {
                    MarkTokenStart();
                    yield return ApplyProperties(ParseNumber());
                }
                else if (IsDoubleQuote(c))
                {
                    MarkTokenStart();
                    Advance();
                    yield return ApplyProperties(ParseString());
                }
                else
                {
                    MarkTokenStart();
                    yield return ApplyProperties(ParseSymbol());
                }
            }
        }

        private void MarkTokenStart()
        {
            _tokenStartSet = true;
            _tokenStartLine = _line;
            _tokenStartColumn = _column;
        }

        private TToken ApplyProperties<TToken>(TToken token) where TToken : LispToken
        {
            Debug.Assert(_tokenStartSet, $"Expected {nameof(_tokenStartSet)}.  Did you forget to call {nameof(MarkTokenStart)}()?");
            _tokenStartSet = false;
            token.Line = _tokenStartLine;
            token.Column = _tokenStartColumn;
            token.LeadingTrivia = new LispTriviaCollection(_triviaBuilder);
            _triviaBuilder = new List<LispTrivia>();
            return token;
        }

        private bool TryPeek(out char c)
        {
            if (_offset < _characters.Length)
            {
                c = _characters[_offset];
                return true;
            }

            c = default(char);
            return false;
        }

        private void Advance()
        {
            _column++; // newlines are handled elsewhere
            _offset++;
        }

        private void ConsumeTrivia()
        {
            while (TryPeek(out var c))
            {
                if (IsWhitespace(c))
                {
                    ConsumeWhitespaceTrivia();
                }
                else if (IsNewlineLike(c))
                {
                    ConsumeNewlineTrivia();
                }
                else if (IsSemi(c))
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
            if (!TryPeek(out var ws) || !IsWhitespace(ws))
            {
                return;
            }

            Advance();
            var whitespace = new StringBuilder();
            whitespace.Append(ws);
            while (TryPeek(out var c) && IsWhitespace(c))
            {
                whitespace.Append(c);
                Advance();
            }

            _triviaBuilder.Add(new LispWhitespaceTrivia(whitespace.ToString()));
        }

        private void ConsumeNewlineTrivia()
        {
            if (!TryPeek(out var c) || !IsNewlineLike(c))
            {
                return;
            }

            Advance();
            var newline = new StringBuilder();
            newline.Append(c);
            if (c != '\n')
            {
                // maybe consume one more character
                if (TryPeek(out c) && c == '\n')
                {
                    newline.Append(c);
                    Advance();
                }
            }

            _line++;
            _column = 1;
            _triviaBuilder.Add(new LispNewlineTrivia(newline.ToString()));
        }

        private void ConsumeCommentTrivia()
        {
            if (!TryPeek(out var semi) || !IsSemi(semi))
            {
                return;
            }

            Advance();
            var comment = new StringBuilder();
            comment.Append(semi);
            while (TryPeek(out var c) && c != '\n')
            {
                comment.Append(c);
                Advance();
            }

            _triviaBuilder.Add(new LispCommentTrivia(comment.ToString()));
        }

        private LispLeftParenToken ParseLeftParen()
        {
            Debug.Assert(TryPeek(out var c) && IsLeftParen(c));
            Advance();
            var left = new LispLeftParenToken(_isQuoting);
            _isQuoting = false;
            return left;
        }

        private LispRightParenToken ParseRightParen()
        {
            Debug.Assert(TryPeek(out var c) && IsRightParen(c));
            Advance();
            return new LispRightParenToken();
        }

        private LispSymbolToken ParseSymbol(string existing = null)
        {
            var builder = new StringBuilder(existing);
            while (TryPeek(out var c) && !IsEndOfToken(c))
            {
                builder.Append(c);
                Advance();
            }

            var text = builder.ToString();
            var token = new LispSymbolToken(_isQuoting, text);
            _isQuoting = false;
            return token;
        }

        private LispNumberToken ParseNumber(string existing = null)
        {
            // sign is already pre-populated in `existing`
            Debug.Assert(existing == null || existing == "-" || existing == "+");
            var builder = new StringBuilder(existing);

            // 1.2e+5
            // ^
            char c;
            while (TryPeek(out c) && IsDigit(c))
            {
                Advance();
                builder.Append(c);
            }

            // 1.2e+5
            //  ^
            if (TryPeek(out c) && IsDot(c))
            {
                Advance();
                builder.Append(c);

                // 1.2e+5
                //   ^
                while (TryPeek(out c) && IsDigit(c))
                {
                    Advance();
                    builder.Append(c);
                }

                // 1.2e+5
                //    ^
                if (TryPeek(out c) && IsE(c))
                {
                    Advance();
                    builder.Append(c);

                    // 1.2e+5
                    //     ^
                    if (TryPeek(out c) && IsSign(c))
                    {
                        Advance();
                        builder.Append(c);
                    }

                    // 1.2e+5
                    //      ^
                    while (TryPeek(out c) && IsDigit(c))
                    {
                        Advance();
                        builder.Append(c);
                    }
                }
            }

            var text = builder.ToString();
            var value = double.Parse(text);
            return new LispNumberToken(value, text);
        }

        private LispStringToken ParseString()
        {
            var builder = new StringBuilder();
            var isEscape = false;
            while (TryPeek(out var c))
            {
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
            return new LispStringToken(text);
        }

        private static bool IsEndOfToken(char c)
        {
            return IsTrivia(c)
                || IsLeftParen(c)
                || IsRightParen(c);
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

        private static bool IsSingleQuote(char c)
        {
            return c == '\'';
        }

        private static bool IsMinus(char c)
        {
            return c == '-';
        }

        private static bool IsPlus(char c)
        {
            return c == '+';
        }

        private static bool IsSign(char c)
        {
            return IsPlus(c)
                || IsMinus(c);
        }

        private static bool IsDigit(char c)
        {
            switch (c)
            {
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    return true;
                default:
                    return false;
            }
        }

        private static bool IsDot(char c)
        {
            return c == '.';
        }

        private static bool IsE(char c)
        {
            return c == 'e' || c == 'E';
        }

        private static bool IsDoubleQuote(char c)
        {
            return c == '"';
        }
    }
}
