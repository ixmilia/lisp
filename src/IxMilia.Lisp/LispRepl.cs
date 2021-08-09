using System.Collections.Generic;
using System.IO;
using System.Linq;
using IxMilia.Lisp.Parser;
using IxMilia.Lisp.Tokens;

namespace IxMilia.Lisp
{
    public class LispRepl
    {
        private string _location;
        private LispHost _host;
        private LispParser _parser;
        private TextReader _input;
        private TextWriter _output;
        private TextWriter _traceWriter;

        public HashSet<string> TracedFunctions { get; } = new HashSet<string>();

        public LispRepl(string location = null, TextReader input = null, TextWriter output = null, TextWriter traceWriter = null)
        {
            _location = location;
            _input = input ?? TextReader.Null;
            _output = output ?? TextWriter.Null;
            _traceWriter = traceWriter ?? TextWriter.Null;
            _host = new LispHost(_location, _input, _output);
            _host.RootFrame.FunctionEntered += FunctionEntered;
            _host.RootFrame.FunctionReturned += FunctionReturned;
            _parser = new LispParser(errorOnIncompleteExpressions: false);

            var replContext = new LispReplDefaultContext(this);
            _host.AddContextObject(replContext);
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
            var depth = frame.Depth;
            if (isFunctionEnter)
            {
                depth--;
            }

            var indent = new string(' ', depth);
            return $"{indent}{depth}";
        }

        private void WriteTraceLine(string value)
        {
            _traceWriter.WriteLine(value);
            _traceWriter.Flush();
        }

        public LispReplResult Eval(string code)
        {
            var tokenizer = new LispTokenizer(_location, code);
            var tokens = tokenizer.GetTokens();
            _parser.AddTokens(tokens);
            var result = _parser.Parse();
            var executionState = _host.Eval(result.Nodes);
            var lastValue = executionState.LastResult;
            return new LispReplResult(lastValue, result.ParseDepth);
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
