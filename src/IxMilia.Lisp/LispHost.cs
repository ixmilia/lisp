using System;
using System.Collections.Generic;
using System.Linq;
using IxMilia.Lisp.Parser;
using IxMilia.Lisp.Tokens;

namespace IxMilia.Lisp
{
    public class LispHost
    {
        private readonly Dictionary<string, Func<LispObject[], LispObject>> _functionMap = new Dictionary<string, Func<LispObject[], LispObject>>();

        public void AddFunction(string name, Func<LispObject[], LispObject> function)
        {
            _functionMap[name] = function;
        }

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
                    return EvalList(list);
                case LispRawListSyntax list:
                    return new LispList(list.Elements.Select(e => Eval(e)));
                default:
                    return null;
            }
        }

        private LispObject EvalList(LispListSyntax list)
        {
            var functionName = ((LispAtomSyntax)list.Elements.First()).Atom.Value;
            var argsSyntax = list.Elements.Skip(1);
            var args = argsSyntax.Select(a => Eval(a)).ToArray();
            if (_functionMap.TryGetValue(functionName, out var function))
            {
                return function.Invoke(args);
            }
            else
            {
                return null; // TODO: error value
            }
        }
    }
}
