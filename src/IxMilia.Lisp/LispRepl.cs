using IxMilia.Lisp.Parser;
using IxMilia.Lisp.Tokens;

namespace IxMilia.Lisp
{
    public class LispRepl
    {
        private LispHost _host;
        private LispParser _parser;

        public LispRepl()
        {
            _host = new LispHost();
            _parser = new LispParser(errorOnIncompleteExpressions: false);
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
