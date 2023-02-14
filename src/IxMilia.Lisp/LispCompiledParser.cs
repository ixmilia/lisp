using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace IxMilia.Lisp
{
    internal class LispCompiledParser
    {
        public LispTextStream Stream { get; }
        public Func<LispCharacter, LispFunctionReference> GetReaderMacro { get; }

        internal static LispError RealEof = new LispError("EOF");

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

        private async Task<LispObject> ParseListAsync(LispHost host, LispStackFrame stackFrame, LispObject eofValue, int depth, CancellationToken cancellationToken = default)
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
