using System.IO;
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

        public LispRepl(string location = null, TextReader input = null, TextWriter output = null, TextWriter traceWriter = null)
        {
            _location = location;
            _input = input ?? TextReader.Null;
            _output = output ?? TextWriter.Null;
            _traceWriter = traceWriter ?? TextWriter.Null;
            _host = new LispHost(_location, _input, _output);
            _host.RootFrame.TraceFunctionEntered += TraceFunctionEntered;
            _host.RootFrame.TraceFunctionReturned += TraceFunctionReturned;
            _parser = new LispParser(errorOnIncompleteExpressions: false);
        }

        private void TraceFunctionEntered(object sender, LispFunctionEnteredEventArgs e)
        {
            WriteTraceLine($"{CreateTracePrefix(e.Frame, isFunctionEnter: true)}: ({e.Frame.FunctionName}{(e.FunctionArguments.Length > 0 ? " " + string.Join<LispObject>(" ", e.FunctionArguments) : "")})");
        }

        private void TraceFunctionReturned(object sender, LispFunctionReturnedEventArgs e)
        {
            WriteTraceLine($"{CreateTracePrefix(e.Frame, isFunctionEnter: false)}: returned {e.ReturnValue}");
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
    }
}
