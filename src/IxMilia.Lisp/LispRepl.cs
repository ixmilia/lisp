using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace IxMilia.Lisp
{
    public class LispRepl
    {
        private string _location;
        private TextReader _input;

        public TextWriter Output { get; }

        private TextWriter _traceWriter;
        private string _incompleteInput;

        public LispHost Host { get; }

        public HashSet<string> TracedFunctions { get; } = new HashSet<string>();

        public LispRepl(string location = null, TextReader input = null, TextWriter output = null, TextWriter traceWriter = null, bool useTailCalls = false)
        {
            _location = location;
            _input = input ?? TextReader.Null;
            Output = output ?? TextWriter.Null;
            _traceWriter = traceWriter ?? TextWriter.Null;
            Host = new LispHost(_location, _input, Output, useTailCalls);
            Host.RootFrame.FunctionEntered += FunctionEntered;
            Host.RootFrame.FunctionReturned += FunctionReturned;

            var replContext = new LispReplDefaultContext(this);
            Host.AddContextObject(replContext);
        }

        private void FunctionEntered(object sender, LispFunctionEnteredEventArgs e)
        {
            if (TracedFunctions.Contains(e.Frame.FunctionSymbol.Value))
            {
                WriteTraceLine($"{CreateTracePrefix(e.Frame, isFunctionEnter: true)}: ({e.Frame.FunctionSymbol.LocalName}{(e.FunctionArguments.Length > 0 ? " " + string.Join<LispObject>(" ", e.FunctionArguments) : "")})");
            }
        }

        private void FunctionReturned(object sender, LispFunctionReturnedEventArgs e)
        {
            if (TracedFunctions.Contains(e.Frame.FunctionSymbol.Value))
            {
                WriteTraceLine($"{CreateTracePrefix(e.Frame, isFunctionEnter: false)}: returned {e.ReturnValue}");
            }
        }

        private string CreateTracePrefix(LispStackFrame frame, bool isFunctionEnter)
        {
            var depth = Math.Max(0, frame.Depth - 1);
            var indent = new string(' ', depth);
            return $"{indent}{depth}";
        }

        private void WriteTraceLine(string value)
        {
            _traceWriter.WriteLine(value);
            _traceWriter.Flush();
        }

        public LispParseResult ParseUntilSourceLocation(string code, LispSourcePosition position)
        {
            var boundValues = Host.RootFrame.GetBoundValues();
            LispObject containingObject = null;
            var eofValue = new LispError("EOF");
            var textReader = new StringReader(code);
            var textStream = new LispTextStream("", textReader, TextWriter.Null);
            Host.ObjectReader.SetReaderStream(textStream);
            while (true)
            {
                var oldAllow = Host.ObjectReader.AllowIncompleteObjects;
                Host.ObjectReader.AllowIncompleteObjects = true;
                var result = Host.ObjectReader.Read(false, eofValue, true);
                Host.ObjectReader.AllowIncompleteObjects = oldAllow;
                if (ReferenceEquals(result.LastResult, eofValue))
                {
                    break;
                }

                boundValues.TryAddSourceBinding(Host.CurrentPackage, result.LastResult);

                if (result.LastResult.SourceLocation.HasValue &&
                    result.LastResult.SourceLocation.Value.ContainsPosition(position))
                {
                    containingObject = result.LastResult;
                    break;
                }
            }

            var narrowestChild = containingObject?.GetNarrowestChild(position);
            if (narrowestChild != null && !ReferenceEquals(narrowestChild, containingObject))
            {
                boundValues.TryAddSourceBinding(Host.CurrentPackage, narrowestChild);
            }

            var parseResult = new LispParseResult(Host, narrowestChild, boundValues);
            return parseResult;
        }

        public IEnumerable<LispObject> ParseAll(string code)
        {
            var eofValue = new LispError("EOF");
            var textReader = new StringReader(code);
            var textStream = new LispTextStream("", textReader, TextWriter.Null);
            Host.ObjectReader.SetReaderStream(textStream);
            while (true)
            {
                var oldAllow = Host.ObjectReader.AllowIncompleteObjects;
                Host.ObjectReader.AllowIncompleteObjects = true;
                var result = Host.ObjectReader.Read(false, eofValue, true);
                Host.ObjectReader.AllowIncompleteObjects = oldAllow;
                if (ReferenceEquals(result.LastResult, eofValue))
                {
                    break;
                }

                yield return result.LastResult;
            }
        }

        public LispObject GetValue(string name) => Host.GetValue(name);

        public TObject GetValue<TObject>(string name) where TObject : LispObject => Host.GetValue<TObject>(name);

        public LispReplResult Eval(string code, bool consumeIncompleteInput = true)
        {
            var unconsumedCode = consumeIncompleteInput && !string.IsNullOrEmpty(_incompleteInput)
                ? string.Concat(_incompleteInput, Environment.NewLine)
                : null;
            var fullCode = string.Concat(unconsumedCode, code);
            var evalResult = Host.Eval(fullCode);
            _incompleteInput = evalResult.IncompleteInput;
            var replResult = new LispReplResult(evalResult.ExecutionState, evalResult.ExpressionDepth);
            return replResult;
        }

        internal void Eval(LispExecutionState executionState)
        {
            Host.EvalContinue(executionState);
        }

        internal LispObject Trace(LispObject[] args)
        {
            if (args.Length == 0)
            {
                return LispList.FromEnumerable(TracedFunctions.Select(f => LispSymbol.CreateFromString(f)));
            }
            else
            {
                var addedFunctions = new HashSet<string>();
                var hasInvalidArguments = false;
                foreach (var arg in args)
                {
                    if (arg is LispSymbol symbol)
                    {
                        var resolvedSymbol = symbol.Resolve(Host.CurrentPackage);
                        TracedFunctions.Add(resolvedSymbol.Value);
                        addedFunctions.Add(resolvedSymbol.Value);
                    }
                    else
                    {
                        hasInvalidArguments = true;
                    }
                }

                if (hasInvalidArguments)
                {
                    return new LispError("Expected only symbols");
                }
                else
                {
                    return LispList.FromEnumerable(addedFunctions.Select(f => LispSymbol.CreateFromString(f)));
                }
            }
        }

        internal LispObject Untrace(LispObject[] args)
        {
            if (args.Length == 0)
            {
                var result = LispList.FromEnumerable(TracedFunctions.Select(f => LispSymbol.CreateFromString(f)));
                TracedFunctions.Clear();
                return result;
            }
            else
            {
                var removedFunctions = new HashSet<string>();
                var hasInvalidArguments = false;
                foreach (var arg in args)
                {
                    if (arg is LispSymbol symbol)
                    {
                        var resolvedSymbol = symbol.Resolve(Host.CurrentPackage);
                        if (TracedFunctions.Remove(resolvedSymbol.Value))
                        {
                            removedFunctions.Add(resolvedSymbol.Value);
                        }
                    }
                    else
                    {
                        hasInvalidArguments = true;
                    }
                }

                if (hasInvalidArguments)
                {
                    return new LispError("Expected only symbols");
                }
                else
                {
                    return LispList.FromEnumerable(removedFunctions.Select(f => LispSymbol.CreateFromString(f)));
                }
            }
        }
    }
}
