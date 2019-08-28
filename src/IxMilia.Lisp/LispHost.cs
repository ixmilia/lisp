using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using IxMilia.Lisp.Parser;
using IxMilia.Lisp.Tokens;

namespace IxMilia.Lisp
{
    public class LispHost
    {
        private readonly Dictionary<string, Func<LispHost, LispSyntax[], LispObject>> _functionMap = new Dictionary<string, Func<LispHost, LispSyntax[], LispObject>>();
        private LispScope _scope = new LispScope();

        public LispHost()
        {
            PrepareCommonFunctions();
        }

        public void AddFunction(string name, Func<LispHost, LispSyntax[], LispObject> function)
        {
            _functionMap[name] = function;
        }

        public void SetValue(string name, LispObject value)
        {
            _scope[name] = value;
        }

        public LispObject GetValue(string name)
        {
            return _scope[name];
        }

        private void IncreaseScope()
        {
            _scope = new LispScope(_scope);
        }

        private void DecreaseScope()
        {
            var parent = _scope.Parent;
            if (parent != null)
            {
                _scope = parent;
            }
        }

        public LispObject Eval(string code)
        {
            var tokenizer = new LispTokenizer(code);
            var tokens = tokenizer.GetTokens();
            var parser = new LispParser(tokens);
            var nodes = parser.Parse();
            return Eval(nodes);
        }

        public LispObject Eval(IEnumerable<LispSyntax> nodes)
        {
            LispObject lastValue = null;
            foreach (var node in nodes)
            {
                lastValue = Eval(node);
            }

            return lastValue;
        }

        private void PrepareCommonFunctions()
        {
            AddFunction("setq", (host, args) =>
            {
                // TODO: properly validate types
                for (int i = 0; i < args.Length - 1; i += 2)
                {
                    var name = ((LispAtomSyntax)args[i]).Atom.Value;
                    var value = host.Eval(args[i + 1]);
                    host.SetValue(name, value);
                }

                return null;
            });

            AddFunction("defun", (host, args) =>
            {
                // TODO: properly validate types and arg counts
                var name = ((LispAtomSyntax)args[0]).Atom.Value;
                var functionArgs = ((LispListSyntax)args[1]).Elements.Cast<LispAtomSyntax>().Select(a => a.Atom.Value);
                var commands = args.Skip(2);
                var function = new LispFunction(functionArgs, commands);
                host.SetValue(name, function);
                return null;
            });
        }

        public LispObject Eval(LispSyntax syntax)
        {
            switch (syntax)
            {
                case LispAtomSyntax atom:
                    return GetValue(atom.Atom.Value);
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
            var args = list.Elements.Skip(1).ToArray();
            var value = GetValue(functionName);
            if (value is LispFunction)
            {
                // TODO: what if it's a regular variable?
                var function = (LispFunction)value;
                IncreaseScope();
                // bind arguments
                // TODO: validate argument count
                for (int i = 0; i < function.Arguments.Length; i++)
                {
                    SetValue(function.Arguments[i], Eval(args[i]));
                }
                // eval values
                var result = Eval(function.Commands);
                DecreaseScope();
                return result;
            }
            else if (_functionMap.TryGetValue(functionName, out var function))
            {
                var result = function.Invoke(this, args);
                return result;
            }
            else
            {
                return null; // TODO: error value
            }
        }
    }
}
