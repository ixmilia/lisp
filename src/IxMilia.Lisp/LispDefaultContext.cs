using System.Linq;
using IxMilia.Lisp.Parser;

namespace IxMilia.Lisp
{
    public class LispDefaultContext
    {
        [LispValue("defun")]
        public LispObject DefineFunction(LispHost host, LispSyntax[] args)
        {
            // TODO: properly validate types and arg counts
            var name = ((LispAtomSyntax)args[0]).Atom.Value;
            var functionArgs = ((LispListSyntax)args[1]).Elements.Cast<LispAtomSyntax>().Select(a => a.Atom.Value);
            var commands = args.Skip(2);
            var function = new LispFunction(functionArgs, commands);
            host.SetValue(name, function);
            return null;
        }

        [LispValue("setq")]
        public LispObject SetValue(LispHost host, LispSyntax[] args)
        {
            // TODO: properly validate types
            for (int i = 0; i < args.Length - 1; i += 2)
            {
                var name = ((LispAtomSyntax)args[i]).Atom.Value;
                var value = host.Eval(args[i + 1]);
                host.SetValue(name, value);
            }

            return null;
        }
    }
}
