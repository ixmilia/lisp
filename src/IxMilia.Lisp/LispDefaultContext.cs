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
            return LispObject.Nil;
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

            return LispObject.Nil;
        }

        [LispValue("<")]
        public LispObject LessThan(LispHost host, LispSyntax[] args)
        {
            if (args.Length < 2)
            {
                return new LispError("Must supply 2 or more arguments");
            }

            var values = args.Select(a => host.Eval(a)).ToArray();
            var result = true;
            for (int i = 0; i < values.Length - 1 && result; i++)
            {
                var a = values[i];
                var b = values[i + 1];

                // TODO: handle non-numbers (aka, strings)
                if (a is LispNumber na && b is LispNumber nb)
                {
                    result &= na.Value < nb.Value;
                }
            }

            return result
                ? (LispObject)LispObject.T
                : LispObject.Nil;
        }

        [LispValue("if")]
        public LispObject If(LispHost host, LispSyntax[] args)
        {
            if (args.Length != 3)
            {
                return new LispError("Expected 3 arguments");
            }

            var condition = host.Eval(args[0]);
            var resultExpressions = condition is LispNil
                ? (LispListSyntax)args[2] // nil means follow the false path
                : (LispListSyntax)args[1]; // everything else is true
            // TODO: numerical 0 should probably follow the false path
            var result = host.Eval(resultExpressions.Elements);
            return result;
        }
    }
}
