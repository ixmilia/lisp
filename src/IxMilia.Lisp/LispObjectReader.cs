using System;
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
        private int _line = 1;
        private int _column = 1;
        private int _leftParenCount = 0;
        private StringBuilder _incompleteInput = null;
        private Dictionary<char, LispFunctionReference> _macroFunctions = new Dictionary<char, LispFunctionReference>();

        private static List<Tuple<Regex, Func<Match, LispObject>>> RegexMatchers = new List<Tuple<Regex, Func<Match, LispObject>>>();

        public LispStream InputStream => _input;

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

        public LispObjectReader(LispHost host)
        {
            _host = host;

            _host.SetValue("STRING-EMPTY", new LispString(string.Empty));
            _host.AddFunction("SET-MACRO-CHARACTER", (__host, executionState, args) =>
            {
                if (args.Length == 2 &&
                    args[0] is LispCharacter character &&
                    args[1] is LispFunctionReference functionRef)
                {
                    _macroFunctions.Add(character.Value, functionRef);
                    return __host.Nil;
                }

                return new LispError("Expected character and function reference");
            });
            _host.AddFunction("KERNEL:MAKE-LAMBDA-FUNCTION", (__host, executionState, args) =>
            {
                if (args.Length == 1 &&
                    args[0] is LispList lambdaList &&
                    lambdaList.Value is LispSymbol lambdaSymbol &&
                    lambdaSymbol.Value == "LAMBDA")
                {
                    var lambdaName = $"(LAMBDA-{lambdaList.SourceLocation?.Start.Line}-{lambdaList.SourceLocation?.Start.Column})"; // surrounded by parens to make it un-utterable
                    var lambdaItems = new List<LispObject>();
                    lambdaItems.Add(new LispSymbol(lambdaName));
                    lambdaItems.AddRange(lambdaList.ToList().Skip(1));

                    if (!LispDefaultContext.TryGetCodeFunctionFromItems(lambdaItems.ToArray(), out var lambdaFunction, out var error))
                    {
                        return error;
                    }
                    else
                    {
                        return new LispQuotedLambdaFunctionReference(lambdaFunction);
                    }
                }

                return new LispError("Expected a lambda");
            });
            _host.AddFunction("KERNEL:PROCESS-LIST-FORWARD-REFERENCE", (__host, executionState, args) =>
            {
                var forwardReferenceId = ReadUntilCharMatches(true, null, true, c => IsEquals(c) || IsHash(c));
                var trailingCharacter = Peek(true, null, true);
                Advance(true, null, true); // swallow `=` or `#`

                var symbolReference = string.Concat("#", forwardReferenceId, "#");
                switch (trailingCharacter)
                {
                    case LispCharacter lc when lc.Value == '#':
                        return new LispSymbol(symbolReference);
                    case LispCharacter lc when lc.Value == '=':
                        var candidateInnerListReaderResult = Read(true, null, true);
                        var candidateInnerList = candidateInnerListReaderResult.LastResult;
                        if (candidateInnerList is LispList innerList)
                        {

                            return new LispForwardListReference(symbolReference, innerList);
                        }
                        else if (candidateInnerList is LispError)
                        {
                            return candidateInnerList;
                        }
                        else
                        {
                            return new LispError("Expected list");
                        }
                    default:
                        return trailingCharacter; // probably an error
                }
            });
            _host.AddFunction("KERNEL:APPEND-CHAR-TO-STRING", (__host, executionState, args) =>
            {
                if (args.Length == 2 &&
                    args[0] is LispString s &&
                    args[1] is LispCharacter c)
                {
                    var result = s.Value + c.Value;
                    return new LispString(result);
                }

                return new LispError("Expected the string so far and the next character");
            });
        }

        public void SetReaderStream(LispStream input)
        {
            _input = input;
            _line = 1;
            _column = 1;
            _leftParenCount = 0;
        }

        public LispObjectReaderResult Read(bool errorOnEof, LispObject eofValue, bool isRecursive)
        {
            var isRootInvocation = false;
            if (_incompleteInput is null)
            {
                isRootInvocation = true;
                _incompleteInput = new StringBuilder();
            }

            LispObject result = null;
            ConsumeTrivia(errorOnEof, eofValue, isRecursive);
            if (!TryPeek(errorOnEof, eofValue, isRecursive, out var next))
            {
                result = errorOnEof ? new LispError("EOF") : eofValue;
            }
            else if (!(next is LispCharacter lc))
            {
                result = new LispError("Expected a character");
            }
            else
            {
                var c = lc.Value;
                if (isRootInvocation && _incompleteInput.Length == 0)
                {
                    // don't lose the first character
                    //_incompleteInput.Append(c);
                }

                if (_macroFunctions.TryGetValue(c, out var readerFunction))
                {
                    Advance(errorOnEof, eofValue, isRecursive);
                    var originalReader = _input.Input;
                    try
                    {
                        // calls to `(read-char ...)` in the reader function can't be observed, but we _can_ peek at every character that's read and report it back
                        var mirroringReader = new MirroringTextReader(originalReader, rc =>
                        {
                            _incompleteInput?.Append(rc);
                        });
                        _input.Input = mirroringReader;
                        result = LispDefaultContext.FunCall(_host, _host.RootFrame, readerFunction, new LispObject[] { _input, lc });
                    }
                    finally
                    {
                        _input.Input = originalReader;
                    }
                }
                else if (IsTrivia(c))
                {
                    ConsumeTrivia(errorOnEof, eofValue, isRecursive);
                }
                else if (IsLeftParen(c))
                {
                    result = ReadList(errorOnEof, eofValue, isRecursive);
                }
                else
                {
                    var text = ReadUntilTriviaOrListMarker(errorOnEof, eofValue, isRecursive);
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
                            if (!IsRightParen(lc.Value))
                            {
                                result = new LispSymbol(text.ToUpperInvariant());
                            }
                            else
                            {
                                result = new LispError($"Unexpected character '{c}' at position ({lc.SourceLocation?.Start.Line}, {lc.SourceLocation?.Start.Column})");
                            }
                        }
                    }
                }
            }

            if (result is object &&
                result.SourceLocation == null &&
                next is object &&
                next.SourceLocation != null)
            {
                result.SourceLocation = new LispSourceLocation(next.SourceLocation.Value.FilePath, next.SourceLocation.Value.Start, new LispSourcePosition(_line, _column));
            }

            var incompleteInput = result is LispError
                ? _incompleteInput.ToString()
                : null;
            if (isRootInvocation)
            {
                _incompleteInput = null;
            }

            return new LispObjectReaderResult(result, incompleteInput, _leftParenCount);
        }

        private LispObject ReadList(bool errorOnEof, LispObject eofValue, bool isRecursive)
        {
            var items = new List<LispObject>();
            var tailItems = new List<LispObject>();
            LispSourceLocation? dotLocation = null;
            var isListComplete = false;
            var first = Peek(errorOnEof, eofValue, isRecursive); // should be `(`
            switch (first)
            {
                case LispCharacter firstCharacter when firstCharacter.Value == '(':
                    _leftParenCount++;
                    break;
                default:
                    throw new Exception("First character should always be `(`");
            }

            Advance(errorOnEof, eofValue, isRecursive);
            ConsumeTrivia(errorOnEof, eofValue, isRecursive);
            var next = Peek(errorOnEof, eofValue, isRecursive);
            while (next is LispCharacter lc)
            {
                var c = lc.Value;
                if (IsRightParen(c))
                {
                    // done
                    Advance(errorOnEof, eofValue, isRecursive);
                    isListComplete = true;
                    _leftParenCount--;
                    break;
                }
                else if (IsPeriod(c))
                {
                    if (dotLocation.HasValue)
                    {
                        var error = new LispError($"Unexpected duplicate '.' in list at ({lc.SourceLocation?.Start.Line}, {lc.SourceLocation?.Start.Column}); first '.' at ({dotLocation.Value.Start.Line}, {dotLocation.Value.Start.Column})");
                        error.SourceLocation = lc.SourceLocation;
                        return error;
                    }

                    Advance(errorOnEof, eofValue, isRecursive);
                    dotLocation = lc.SourceLocation;
                }

                var nextItemResult = Read(errorOnEof, eofValue, isRecursive);
                var nextItem = nextItemResult.LastResult;
                if (nextItem is LispError)
                {
                    return nextItem;
                }

                if (!dotLocation.HasValue)
                {
                    items.Add(nextItem);
                }
                else
                {
                    tailItems.Add(nextItem);
                }

                ConsumeTrivia(errorOnEof, eofValue, isRecursive);
                next = Peek(errorOnEof, eofValue, isRecursive);
            }

            if (!isListComplete)
            {
                return new LispError($"Unmatched '(' at ({first.SourceLocation?.Start.Line}, {first.SourceLocation?.Start.Column}) (depth {_leftParenCount})");
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
                if (items.Any())
                {
                    result = LispList.FromEnumerable(items);
                }
                else
                {
                    result = LispNilList.CreateForParser();
                }
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

        private LispObject Peek(bool errorOnEof, LispObject eofValue, bool isRecursive)
        {
            var result = LispDefaultContext.PeekChar(null, _input, errorOnEof, eofValue, isRecursive);
            if (result.SourceLocation is null)
            {
                result.SourceLocation = new LispSourceLocation(_input.Name, new LispSourcePosition(_line, _column), new LispSourcePosition(_line, _column + 1));
            }

            return result;
        }

        private bool TryPeek(bool errorOnEof, LispObject eofValue, bool isRecursive, out LispCharacter next)
        {
            var result = Peek(errorOnEof, eofValue, isRecursive);
            if (result is LispCharacter lc)
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

        private void Advance(bool errorOnEof, LispObject eofValue, bool isRecursive)
        {
            var result = LispDefaultContext.ReadChar(_input, errorOnEof, eofValue, isRecursive);
            _column++;

            if (result is LispCharacter lc)
            {
                _incompleteInput?.Append(lc.Value);
            }
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

        internal static bool IsSkippableWhitespace(char c)
        {
            return IsNewlineLike(c) || IsWhitespace(c);
        }

        private static bool IsEquals(char c)
        {
            return c == '=';
        }

        private static bool IsHash(char c)
        {
            return c == '#';
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

        private string ReadUntilCharMatches(bool errorOnEof, LispObject eofValue, bool isRecursive, Func<char, bool> stopCondition)
        {
            var builder = new StringBuilder();
            while (TryPeek(errorOnEof, eofValue, isRecursive, out var lc))
            {
                var c = lc.Value;
                if (stopCondition(c))
                {
                    break;
                }

                builder.Append(c);
                Advance(errorOnEof, eofValue, isRecursive);
            }

            var text = builder.ToString();
            return text;
        }

        private string ReadUntilTriviaOrListMarker(bool errorOnEof, LispObject eofValue, bool isRecursive)
        {
            return ReadUntilCharMatches(errorOnEof, eofValue, isRecursive, c =>
            {
                return IsTrivia(c)
                    || IsLeftParen(c)
                    || IsRightParen(c);
            });
        }

        private void ConsumeTrivia(bool errorOnEof, LispObject eofValue, bool isRecursive)
        {
            while (TryPeek(errorOnEof, eofValue, isRecursive, out var c))
            {
                if (IsWhitespace(c.Value))
                {
                    ConsumeWhitespaceTrivia(errorOnEof, eofValue, isRecursive);
                }
                else if (IsNewlineLike(c.Value))
                {
                    ConsumeNewlineTrivia(errorOnEof, eofValue, isRecursive);
                }
                else if (IsSemi(c.Value))
                {
                    ConsumeCommentTrivia(errorOnEof, eofValue, isRecursive);
                }
                else
                {
                    return;
                }
            }
        }

        private void ConsumeWhitespaceTrivia(bool errorOnEof, LispObject eofValue, bool isRecursive)
        {
            if (!TryPeek(errorOnEof, eofValue, isRecursive, out var ws) || !IsWhitespace(ws.Value))
            {
                return;
            }

            Advance(errorOnEof, eofValue, isRecursive);
            while (TryPeek(errorOnEof, eofValue, isRecursive, out var c) && IsWhitespace(c.Value))
            {
                Advance(errorOnEof, eofValue, isRecursive);
            }
        }

        private void ConsumeNewlineTrivia(bool errorOnEof, LispObject eofValue, bool isRecursive)
        {
            while (TryPeek(errorOnEof, eofValue, isRecursive, out var c)
                && IsNewlineLike(c.Value))
            {
                _line++;
                _column = 0; // set to 0 because the next line will increment it
                Advance(errorOnEof, eofValue, isRecursive);
            }
        }

        private void ConsumeCommentTrivia(bool errorOnEof, LispObject eofValue, bool isRecursive)
        {
            if (!TryPeek(errorOnEof, eofValue, isRecursive, out var semi) || !IsSemi(semi.Value))
            {
                return;
            }

            Advance(errorOnEof, eofValue, isRecursive);
            while (TryPeek(errorOnEof, eofValue, isRecursive, out var c) && c.Value != '\n')
            {
                Advance(errorOnEof, eofValue, isRecursive);
            }
        }
    }
}
