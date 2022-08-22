using System;
using System.IO;
using System.Threading.Tasks;
using IxMilia.Lisp.DebugAdapter;

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

            // TODO: use proper command line parser for options
            if (args.Length == 1)
            {
                switch (args[0])
                {
                    case "debug":
                        RunDebugAsync().GetAwaiter().GetResult();
                        return;
                    case "lsp":
                        RunLspAsync().GetAwaiter().GetResult();
                        return;
                }
            }

            Console.Error.WriteLine("Usage: lsp");
            Environment.Exit(1);
        }

        private static async Task RunDebugAsync()
        {
            var options = new DebugAdapterOptions(
                resolveFileContents: path => File.ReadAllTextAsync(path),
                messageLogger: null);
            var server = DebugAdapter.DebugAdapter.CreateFromStreams(Console.OpenStandardInput(), Console.OpenStandardOutput(), options);
            server.Start();
            await server.ServerTask;
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
