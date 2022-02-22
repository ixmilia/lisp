using System;
using System.Threading;

namespace IxMilia.Lisp.LanguageServer.App
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var server = new LanguageServer(Console.OpenStandardOutput(), Console.OpenStandardInput());
            server.Start();
            while (true)
            {
                Thread.Sleep(500);
            }
        }
    }
}
