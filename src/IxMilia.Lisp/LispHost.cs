using System.Linq;
using IxMilia.Lisp.Parser;
using IxMilia.Lisp.Tokens;

namespace IxMilia.Lisp
{
    public class LispHost
    {
        public LispObject Eval(string code)
        {
            var tokenizer = new LispTokenizer(code);
            var tokens = tokenizer.GetTokens();
            var parser = new LispParser(tokens);
            var syntaxNodes = parser.Parse();
            LispObject lastValue = null;
            foreach (var syntaxNode in syntaxNodes)
            {
                lastValue = Eval(syntaxNode);
            }

            return lastValue;
        }

        private LispObject Eval(LispSyntax syntax)
        {
            switch (syntax)
            {
                case LispAtomSyntax atom:
                    return new LispAtom(atom.Atom.Value);
                case LispNumberSyntax num:
                    return new LispNumber(num.Number.Value);
                case LispStringSyntax str:
                    return new LispString(str.String.Text);
                case LispListSyntax list:
                    return null; // TODO: eval the list where the first item is an atom treated as a function
                case LispRawListSyntax list:
                    return new LispList(list.Elements.Select(e => Eval(e)));
                default:
                    return null;
            }
        }
    }
}
