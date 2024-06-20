using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace IxMilia.Lisp
{
    internal class LispCompiledParser
    {
        public LispTextStream Stream { get; }
        public Func<LispCharacter, LispFunctionReference> GetReaderMacro { get; }

        internal static LispError RealEof = new LispError("EOF");

        private bool _evalOnComma = false;

        public LispCompiledParser(LispTextStream stream, Func<LispCharacter, LispFunctionReference> getReaderMacro = null)
        {
            Stream = stream;
            GetReaderMacro = getReaderMacro;
        }

        private void Advance()
        {
            if (Stream.Peek() is null)
            {
                return;
            }

            var _c = Stream.Read();
        }

        public async Task<LispObject> ParseItem(LispHost host, LispStackFrame stackFrame, LispObject eofValue, int depth = 0, CancellationToken cancellationToken = default)
        {
            SwallowTrivia();
            var next = Stream.Peek();
            if (next is null)
            {
                return eofValue;
            }

            LispObject result;
            if (depth == 0 && next.Value == ')')
            {
                result = new LispError($"An object cannot start with {next}")
                {
                    SourceLocation = next.SourceLocation
                };
            }
            else if (next.Value == '(')
            {
                // recursing in, always return EOF
                result = await ParseListAsync(host, stackFrame, RealEof, depth + 1, cancellationToken);
            }
            else if (GetReaderMacro?.Invoke(next) is LispFunctionReference readerFunction)
            {
                Advance();
                result = await LispDefaultContext.FunCallAsync(host, stackFrame, readerFunction, new LispObject[] { Stream, next }, cancellationToken);
                // TODO: check for errors
            }
            else if (_evalOnComma && next.Value == ',')
            {
                Advance(); // swallow comma
                var nextItem = await ParseItem(host, stackFrame, eofValue, depth, cancellationToken);
                var executionState = host.CreateExecutionState();
                var evalResult = await host.EvalAsync(nextItem, executionState, cancellationToken: cancellationToken);
                result = evalResult.Value;
            }
            else if (next.Value == '"')
            {
                result = ParseString();
            }
            else if (next.Value == '\'')
            {
                result = await ParseQuotedAsync(host, stackFrame, eofValue, depth, cancellationToken);
            }
            else if (next.Value == '`')
            {
                result = await ParseBackQuotedAsync(host, stackFrame, eofValue, depth, cancellationToken);
            }
            else if (next.Value == '#')
            {
                result = await ParseHashItemAsync(host, stackFrame, eofValue, depth, cancellationToken);
            }
            else
            {
                var allowUnresolvedSymbols = AllowUnresolvedSymbols(host, stackFrame);
                result = LispSymbol.ReadSymbolLike(Stream, host, allowUnresolvedSymbols);
            }

            if (result is not null &&
                !result.SourceLocation.HasValue &&
                next.SourceLocation.HasValue)
            {
                result.SourceLocation = new LispSourceLocation(next.SourceLocation.Value.FilePath, next.SourceLocation.Value.Start, Stream.CurrentPosition);
            }

            return result;
        }

        private async Task<LispObject> ParseListAsync(LispHost host, LispStackFrame stackFrame, LispObject eofValue, int depth, CancellationToken cancellationToken)
        {
            var firstChar = Stream.Peek();
            if (firstChar?.Value != '(')
            {
                throw new Exception("Should have been a '('");
            }

            Advance();
            SwallowTrivia();
            var isComplete = false;
            var items = new List<LispObject>();
            var postDotItems = new List<LispObject>();
            LispSourceLocation? dotLocation = null;
            var item = await ParseItem(host, stackFrame, eofValue, depth, cancellationToken);
            while (item != null)
            {
                if (item.IsEof())
                {
                    return item;
                }

                if (item is LispSymbol symbol &&
                    symbol.LocalName == ".")
                {
                    if (dotLocation.HasValue)
                    {
                        return new LispError("Illegal end of dotted list")
                        {
                            SourceLocation = item.SourceLocation
                        };
                    }

                    dotLocation = item.SourceLocation;
                }
                else
                {
                    if (dotLocation.HasValue)
                    {
                        postDotItems.Add(item);
                    }
                    else
                    {
                        items.Add(item);
                    }

                    SwallowTrivia();
                    var next = Stream.Peek();
                    if (next is null)
                    {
                        // no more content
                        break;
                    }

                    if (next.Value == ')')
                    {
                        // end of list
                        break;
                    }
                }

                item = await ParseItem(host, stackFrame, eofValue, depth, cancellationToken);
            }

            var lastChar = Stream.Peek();
            if (lastChar?.Value == ')')
            {
                isComplete = true;
                Advance();
            }

            if (!isComplete && item is null)
            {
                // in the middle of a list, this is always an error
                return new LispError("EOF");
            }

            if (!isComplete && !AllowIncompleteObjects(host, stackFrame))
            {
                return new LispError("Unmatched #\\(")
                {
                    SourceLocation = firstChar.SourceLocation
                };
            }

            SwallowTrivia();
            var allItems = items.Concat(postDotItems).ToList();
            LispObject result;
            if (dotLocation.HasValue)
            {
                if (postDotItems.Count == 1)
                {
                    // improper list
                    result = LispList.FromEnumerableImproper(allItems[0], allItems[1], allItems.Skip(2));
                }
                else
                {
                    result = new LispError("Too many trailing items");
                }
            }
            else
            {
                if (items.Any())
                {
                    result = LispList.FromEnumerable(items);
                }
                else
                {
                    result = LispNilList.CreateForParser();
                }
            }

            if (result is LispList)
            {
                foreach (var child in allItems)
                {
                    child.Parent = result;
                }
            }

            if (firstChar.SourceLocation.HasValue &&
                lastChar?.SourceLocation.HasValue == true)
            {
                result.SourceLocation = new LispSourceLocation(firstChar.SourceLocation.Value.FilePath, firstChar.SourceLocation.Value.Start, lastChar.SourceLocation.Value.End);
            }

            return result;
        }

        private LispObject ParseString()
        {
            var firstChar = Stream.Peek();
            if (firstChar?.Value != '"')
            {
                throw new Exception("Should have been a '\"'");
            }

            Advance();
            var content = new StringBuilder();
            var nextCharacter = Stream.Peek();
            var keepGoing = true;
            var isEscaped = false;
            while (keepGoing && nextCharacter is { })
            {
                Advance();
                var c = nextCharacter.Value;
                if (isEscaped)
                {
                    content.Append(c);
                    isEscaped = false;
                }
                else
                {
                    switch (c)
                    {
                        case '"':
                            keepGoing = false;
                            break;
                        case '\\':
                            isEscaped = true;
                            break;
                        default:
                            content.Append(c);
                            break;
                    }
                }

                nextCharacter = Stream.Peek();
            }

            if (keepGoing)
            {
                throw new Exception("TODO: error: *end-of-string*");
            }

            var result = new LispString(content.ToString());
            return result;
        }

        private async Task<LispObject> ParseQuotedAsync(LispHost host, LispStackFrame stackFrame, LispObject eofValue, int depth, CancellationToken cancellationToken)
        {
            var firstChar = Stream.Peek();
            if (firstChar?.Value != '\'')
            {
                throw new Exception("Should have been a '''");
            }

            // swallow character and QUOTE the next item
            Advance();
            var item = await ParseItem(host, stackFrame, eofValue, depth, cancellationToken);
            var result = LispList.FromItems(new LispResolvedSymbol("COMMON-LISP", "QUOTE", true), item);
            return result;
        }

        private async Task<LispObject> ParseBackQuotedAsync(LispHost host, LispStackFrame stackFrame, LispObject eofValue, int depth, CancellationToken cancellationToken)
        {
            var firstChar = Stream.Peek();
            if (firstChar?.Value != '`')
            {
                throw new Exception("Should have been a '`'");
            }

            // swallow character and QUOTE the next item, but force eval on comma character
            Advance();
            var oldEvalOnComma = _evalOnComma;
            _evalOnComma = true;

            try
            {
                var item = await ParseItem(host, stackFrame, eofValue, depth, cancellationToken);
                var result = LispList.FromItems(new LispResolvedSymbol("COMMON-LISP", "QUOTE", true), item);
                return result;
            }
            finally
            {
                _evalOnComma = oldEvalOnComma;
            }
        }

        private async Task<LispObject> ParseHashItemAsync(LispHost host, LispStackFrame stackFrame, LispObject eofValue, int depth, CancellationToken cancellationToken)
        {
            // handle hash character
            var firstChar = Stream.Peek();
            if (firstChar?.Value != '#')
            {
                throw new Exception("Should have been a '#'");
            }

            Advance();

            // check for the next character indicating the type of hash item
            var nextChar = Stream.Peek();
            if (nextChar is null)
            {
                return new LispError("TODO: end of stream");
            }

            // read the appropriate hash item type
            switch (nextChar.Value)
            {
                case '\\':
                    // single character
                    Advance(); // swallow backslash
                    var resultCharacter = Stream.Peek();
                    if (resultCharacter is null)
                    {
                        return new LispError("TODO: end of stream");
                    }

                    Advance();
                    return resultCharacter;
                case 'c':
                case 'C':
                    // complex number
                    Advance(); // swallow 'c'
                    var complexNumberItem = await ParseItem(host, stackFrame, eofValue, depth, cancellationToken);
                    var processComplexNumberFunctionReference = new LispQuotedNamedFunctionReference("PROCESS-COMPLEX-NUMBER");
                    var complexArguments = new LispObject[] { LispList.FromItems(new LispUnresolvedSymbol("QUOTE"), complexNumberItem) };
                    var complexNumber = await LispDefaultContext.FunCallAsync(host, stackFrame, processComplexNumberFunctionReference, complexArguments, cancellationToken);
                    return complexNumber;
                case '\'':
                    // quoted named function reference
                    Advance(); // swallow single quote
                    var functionReferenceItem = await ParseItem(host, stackFrame, eofValue, depth, cancellationToken);
                    var functionReferenceEvaluation = LispList.FromItems(
                        new LispUnresolvedSymbol("EVAL"),
                        LispList.FromItems(
                            new LispUnresolvedSymbol("LIST"),
                            LispList.FromItems(
                                new LispUnresolvedSymbol("QUOTE"),
                                new LispUnresolvedSymbol("FUNCTION")),
                            LispList.FromItems(
                                new LispUnresolvedSymbol("QUOTE"),
                                functionReferenceItem)
                        ));
                    var functionReferenceResult = await host.EvalAtStackFrameAsync(stackFrame, functionReferenceEvaluation, cancellationToken);
                    return functionReferenceResult;
                case '(':
                    // vector
                    var vectorItem = await ParseItem(host, stackFrame, eofValue, depth, cancellationToken);
                    var vectorFunctionReference = new LispQuotedNamedFunctionReference("VECTOR");
                    var vectorArguments = LispList.FromItems(new LispUnresolvedSymbol("QUOTE"), vectorItem);
                    var vectorApplication = LispList.FromItems(new LispUnresolvedSymbol("APPLY"), vectorFunctionReference, vectorArguments);
                    return vectorApplication;
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
                    var forwardReferenceEvaluation = LispList.FromItems(
                        new LispResolvedSymbol("KERNEL", "PROCESS-LIST-FORWARD-REFERENCE", true),
                        Stream);
                    var forwardReferenceResult = await host.EvalAtStackFrameAsync(stackFrame, forwardReferenceEvaluation, cancellationToken);
                    return forwardReferenceResult;
                default:
                    Advance();
                    return new LispError("TODO: unknown hash item");
            }
        }

        private static bool AllowIncompleteObjects(LispHost host, LispStackFrame stackFrame)
        {
            var value = stackFrame.GetValue(new LispUnresolvedSymbol("*ALLOW-INCOMPLETE-OBJECTS*").Resolve(host.CurrentPackage));
            return value?.IsTLike() == true;
        }

        internal static bool AllowUnresolvedSymbols(LispHost host, LispStackFrame stackFrame)
        {
            var value = stackFrame.GetValue(new LispUnresolvedSymbol("*ALLOW-UNRESOLVED-SYMBOLS*").Resolve(host.CurrentPackage));
            return value?.IsTLike() == true;
        }

        private void SwallowTrivia()
        {
            LispCharacter next;
            while ((next = Stream.Peek()) is not null)
            {
                if (LispSymbol.IsWhitespace(next.Value))
                {
                    Advance();
                }
                else if (next.Value == ';')
                {
                    // swallow until newline
                    for (; Stream.Peek() is not null;)
                    {
                        next = Stream.Read();
                        if (next.Value == '\n')
                        {
                            break;
                        }
                    }
                }
                else
                {
                    return;
                }
            }
        }
    }
}
