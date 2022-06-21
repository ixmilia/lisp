using System;
using System.Threading.Tasks;

namespace IxMilia.Lisp.EditorServer
{
    public class Program
    {
        public static void Main(string[] args)
        {
            AppDomain.CurrentDomain.FirstChanceException += (_sender, e) =>
            {
                Console.Error.WriteLine(e.Exception.ToString());
            };

            if (args.Length == 1)
            {
                switch (args[0])
                {
                    case "lsp":
                        RunLspAsync().GetAwaiter().GetResult();
                        return;
                }
            }

            Console.Error.WriteLine("Usage: lsp");
            Environment.Exit(1);
        }

        private static async Task RunLspAsync()
        {
            var server = new LanguageServer.LanguageServer(Console.OpenStandardOutput(), Console.OpenStandardInput());
            server.Start();
            while (true)
            {
                await Task.Delay(500);
            }
        }
    }
}
