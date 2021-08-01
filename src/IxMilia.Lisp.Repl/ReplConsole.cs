using System;
using System.IO;

namespace IxMilia.Lisp.Repl
{
    public class ReplConsole
    {
        public string Location { get; }
        public TextReader Input { get; }
        public TextWriter Output { get; }
        public TextWriter Error { get; }

        public ReplConsole(string location, TextReader input, TextWriter output, TextWriter error)
        {
            Location = location;
            Input = input;
            Output = output;
            Error = error;
        }

        public void Run()
        {
            var repl = new LispRepl(location: Location, input: Input, output: Output, traceWriter: Output);

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
