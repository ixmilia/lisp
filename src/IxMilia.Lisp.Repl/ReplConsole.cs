using System;
using System.IO;

namespace IxMilia.Lisp.Repl
{
    public class ReplConsole
    {
        public TextReader Input { get; }
        public TextWriter Output { get; }
        public TextWriter Error { get; }

        public ReplConsole(TextReader input, TextWriter output, TextWriter error)
        {
            Input = input;
            Output = output;
            Error = error;
        }

        public void Run()
        {
            var repl = new LispRepl(input: Input, output: Output, traceWriter: Output);

            PrintPrompt(0);
            string line;
            while ((line = Input.ReadLine()) != "#quit")
            {
                var result = repl.Eval(line);
                if (result.LastValue != null)
                {
                    Output.WriteLine(result.LastValue.ToString());
                }

                PrintPrompt(result.ExpressionDepth);
            }
        }

        private void PrintPrompt(int depth)
        {
            for (int i = 0; i < depth; i++)
            {
                Output.Write("(");
            }

            Output.Write("_> ");
        }
    }
}
