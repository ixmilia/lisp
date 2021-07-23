using System.IO;
using IxMilia.Lisp.Parser;
using IxMilia.Lisp.Tokens;

namespace IxMilia.Lisp
{
    public class LispRepl
    {
        private LispHost _host;
        private LispParser _parser;
        private TextWriter _output;
        private TextWriter _traceWriter;

        public LispRepl(TextWriter output = null, TextWriter traceWriter = null)
        {
            _output = output ?? TextWriter.Null;
            _traceWriter = traceWriter ?? TextWriter.Null;
            _host = new LispHost(_output);
            _host.RootFrame.TraceFunctionEntered += TraceFunctionEntered;
            _host.RootFrame.TraceFunctionReturned += TraceFunctionReturned;
            _parser = new LispParser(errorOnIncompleteExpressions: false);
        }

        private void TraceFunctionEntered(object sender, LispFunctionEnteredEventArgs e)
        {
            WriteTraceLine($"{CreateTracePrefix(e.Frame)}: ({e.Frame.FunctionName}{(e.FunctionArguments.Length > 0 ? " " + string.Join<LispObject>(" ", e.FunctionArguments) : "")})");
        }

        private void TraceFunctionReturned(object sender, LispFunctionReturnedEventArgs e)
        {
            WriteTraceLine($"{CreateTracePrefix(e.Frame)}: returned {e.ReturnValue}");
        }

        private string CreateTracePrefix(LispStackFrame frame)
        {
            var depth = frame.Depth;
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
            var tokenizer = new LispTokenizer(code);
            var tokens = tokenizer.GetTokens();
            _parser.AddTokens(tokens);
            var result = _parser.Parse();
            var lastValue = _host.Eval(result.Nodes);
            return new LispReplResult(lastValue, result.ParseDepth);
        }
    }
}
