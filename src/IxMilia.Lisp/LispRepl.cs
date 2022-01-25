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
        private TextWriter _output;
        private TextWriter _traceWriter;
        private string _incompleteInput;

        internal LispHost Host { get; }

        public HashSet<string> TracedFunctions { get; } = new HashSet<string>();

        public LispRepl(string location = null, TextReader input = null, TextWriter output = null, TextWriter traceWriter = null, bool useTailCalls = false)
        {
            _location = location;
            _input = input ?? TextReader.Null;
            _output = output ?? TextWriter.Null;
            _traceWriter = traceWriter ?? TextWriter.Null;
            Host = new LispHost(_location, _input, _output, useTailCalls);
            Host.RootFrame.FunctionEntered += FunctionEntered;
            Host.RootFrame.FunctionReturned += FunctionReturned;

            var replContext = new LispReplDefaultContext(this);
            Host.AddContextObject(replContext);
        }

        private void FunctionEntered(object sender, LispFunctionEnteredEventArgs e)
        {
            if (TracedFunctions.Contains(e.Frame.FunctionName))
            {
                WriteTraceLine($"{CreateTracePrefix(e.Frame, isFunctionEnter: true)}: ({e.Frame.FunctionName}{(e.FunctionArguments.Length > 0 ? " " + string.Join<LispObject>(" ", e.FunctionArguments) : "")})");
            }
        }

        private void FunctionReturned(object sender, LispFunctionReturnedEventArgs e)
        {
            if (TracedFunctions.Contains(e.Frame.FunctionName))
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

        public LispObject GetObjectAtLocation(string code, LispSourcePosition position)
        {
            LispObject containingObject = null;
            var eofValue = new LispError("EOF");
            var textReader = new StringReader(code);
            var textStream = new LispTextStream("", textReader, TextWriter.Null);
            Host.ObjectReader.SetReaderStream(textStream);
            while (true)
            {
                var result = Host.ObjectReader.Read(false, eofValue, true);
                if (ReferenceEquals(result.LastResult, eofValue))
                {
                    break;
                }

                if (result.LastResult.SourceLocation.HasValue &&
                    result.LastResult.SourceLocation.Value.ContainsPosition(position))
                {
                    containingObject = result.LastResult;
                    break;
                }
            }

            return containingObject?.GetNarrowestChild(position);
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
                return LispList.FromEnumerable(TracedFunctions.Select(f => new LispSymbol(f)));
            }
            else
            {
                var addedFunctions = new HashSet<string>();
                var hasInvalidArguments = false;
                foreach (var arg in args)
                {
                    if (arg is LispSymbol symbol)
                    {
                        TracedFunctions.Add(symbol.Value);
                        addedFunctions.Add(symbol.Value);
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
                    return LispList.FromEnumerable(addedFunctions.Select(f => new LispSymbol(f)));
                }
            }
        }

        internal LispObject Untrace(LispObject[] args)
        {
            if (args.Length == 0)
            {
                var result = LispList.FromEnumerable(TracedFunctions.Select(f => new LispSymbol(f)));
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
                        if (TracedFunctions.Remove(symbol.Value))
                        {
                            removedFunctions.Add(symbol.Value);
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
                    return LispList.FromEnumerable(removedFunctions.Select(f => new LispSymbol(f)));
                }
            }
        }
    }
}
