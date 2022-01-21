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
        private LispTextStream _input;
        private int _leftParenCount = 0;
        private StringBuilder _incompleteInput = null;
        private Dictionary<char, LispFunctionReference> _macroFunctions = new Dictionary<char, LispFunctionReference>();
        private Stack<Tuple<LispTextStream, int>> _readerStack = new Stack<Tuple<LispTextStream, int>>();

        private static List<Tuple<Regex, Func<Match, LispObject>>> RegexMatchers = new List<Tuple<Regex, Func<Match, LispObject>>>();

        public LispTextStream InputStream => _input;

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
                var forwardReferenceId = ReadUntilCharMatches(c => IsEquals(c) || IsHash(c));
                var trailingCharacter = _input.Read();
                var symbolReference = string.Concat("#", forwardReferenceId, "#");
                if (trailingCharacter == null)
                {
                    return new LispError("Expected character");
                }

                switch (trailingCharacter.Value)
                {
                    case '#':
                        return new LispSymbol(symbolReference);
                    case '=':
                        var candidateInnerListReaderResult = Read(true, null, true);
                        var candidateInnerList = candidateInnerListReaderResult.LastResult;
                        switch (candidateInnerList)
                        {
                            case LispList innerList:
                                return new LispForwardListReference(symbolReference, innerList);
                            case LispError error:
                                return error;
                            default:
                                return new LispError("Expected list");
                        }
                    default:
                        return new LispError($"Unexpected character '{trailingCharacter}'");
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

        public void SetReaderStream(LispTextStream input)
        {
            _input = input;
            _leftParenCount = 0;
        }

        internal void PushReaderStream(LispTextStream input)
        {
            _readerStack.Push(Tuple.Create(_input, _leftParenCount));
            SetReaderStream(input);
        }

        internal void PopReaderStream()
        {
            var t = _readerStack.Pop();
            _input = t.Item1;
            _leftParenCount = t.Item2;
        }

        public LispObjectReaderResult Read(bool errorOnEof, LispObject eofValue, bool isRecursive)
        {
            var handler = new EventHandler<LispCharacter>((s, c) =>
            {
                _incompleteInput?.Append(c.Value);
            });
            var isRootInvocation = false;
            if (_incompleteInput is null)
            {
                isRootInvocation = true;
                _incompleteInput = new StringBuilder();
                _input.CharacterRead += handler;
            }

            LispObject result = null;
            ConsumeTrivia();
            var lc = _input.Peek();
            if (lc == null)
            {
                result = errorOnEof ? new LispError("EOF") : eofValue;
            }
            else
            {
                var c = lc.Value;
                if (_macroFunctions.TryGetValue(c, out var readerFunction))
                {
                    _input.Read();
                    result = LispDefaultContext.FunCall(_host, _host.RootFrame, readerFunction, new LispObject[] { _input, lc });
                }
                else if (IsTrivia(c))
                {
                    ConsumeTrivia();
                }
                else if (IsLeftParen(c))
                {
                    result = ReadList(errorOnEof, eofValue, isRecursive);
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
                lc is object)
            {
                var endSourcePosition = _input.CurrentPosition;
                result.SourceLocation = new LispSourceLocation(_input.Name, lc.SourceLocation.Value.Start, endSourcePosition);
            }

            var incompleteInput = result is LispError
                ? _incompleteInput.ToString()
                : null;
            if (isRootInvocation)
            {
                _incompleteInput = null;
                _input.CharacterRead -= handler;
            }

            return new LispObjectReaderResult(result, incompleteInput, _leftParenCount);
        }

        private LispObject ReadList(bool errorOnEof, LispObject eofValue, bool isRecursive)
        {
            var items = new List<LispObject>();
            var tailItems = new List<LispObject>();
            LispSourceLocation? dotLocation = null;
            var isListComplete = false;
            var startPosition = _input.CurrentPosition;
            var first = _input.Peek();
            if (first == null || !IsLeftParen(first.Value))
            {
                throw new Exception("First character should always be '('");
            }

            _leftParenCount++;
            _input.Read();
            ConsumeTrivia();

            LispCharacter next;
            while ((next = _input.Peek()) != null)
            {
                var c = next.Value;
                if (IsRightParen(c))
                {
                    // done
                    _input.Read();
                    isListComplete = true;
                    _leftParenCount--;
                    break;
                }
                else if (IsPeriod(c))
                {
                    var currentLocation = new LispSourceLocation(_input.Name, _input.CurrentPosition, new LispSourcePosition(_input.CurrentPosition.Line, _input.CurrentPosition.Column));
                    if (dotLocation.HasValue)
                    {
                        var error = new LispError($"Unexpected duplicate '.' in list at ({_input.CurrentPosition.Line}, {_input.CurrentPosition.Column}); first '.' at ({dotLocation.Value.Start.Line}, {dotLocation.Value.Start.Column})");
                        error.SourceLocation = currentLocation;
                        return error;
                    }

                    _input.Read();
                    dotLocation = currentLocation;
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

                ConsumeTrivia();
            }

            if (!isListComplete)
            {
                return new LispError($"Unmatched '(' at ({startPosition.Line}, {startPosition.Column}) (depth {_leftParenCount})");
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

        private string ReadUntilCharMatches(Func<char, bool> stopCondition)
        {
            var builder = new StringBuilder();
            while (_input.Peek() is LispCharacter lc)
            {
                if (stopCondition(lc.Value))
                {
                    break;
                }

                builder.Append(lc.Value);
                _input.Read();
            }

            var text = builder.ToString();
            return text;
        }

        private string ReadUntilTriviaOrListMarker()
        {
            return ReadUntilCharMatches(c =>
            {
                return IsTrivia(c)
                    || IsLeftParen(c)
                    || IsRightParen(c);
            });
        }

        private void ConsumeTrivia()
        {
            while(_input.Peek() is LispCharacter lc)
            {
                var c = lc.Value;
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
            while (_input.Peek() is LispCharacter lc && IsWhitespace(lc.Value))
            {
                _input.Read();
            }
        }

        private void ConsumeNewlineTrivia()
        {
            while (_input.Peek() is LispCharacter lc && IsNewlineLike(lc.Value))
            {
                _input.Read();
            }
        }

        private void ConsumeCommentTrivia()
        {
            if (_input.Peek() is null || !IsSemi(_input.Peek().Value))
            {
                return;
            }

            _input.Read();
            while (_input.Peek() is LispCharacter lc && lc.Value != '\n')
            {
                _input.Read();
            }
        }
    }
}
