using System;

namespace IxMilia.Lisp.Repl
{
    class Program
    {
        static void Main(string[] args)
        {
            var r = new ReplConsole("*terminal-io*", Console.In, Console.Out, Console.Error);
            r.Run();
        }
    }
}
