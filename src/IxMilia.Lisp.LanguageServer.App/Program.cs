using System;
using System.Threading;

namespace IxMilia.Lisp.LanguageServer.App
{
    public class Program
    {
        public static void Main(string[] args)
        {
            AppDomain.CurrentDomain.FirstChanceException += (_sender, e) =>
            {
                Console.Error.WriteLine(e.Exception.ToString());
            };
            var server = new LanguageServer(Console.OpenStandardOutput(), Console.OpenStandardInput());
            server.Start();
            while (true)
            {
                Thread.Sleep(500);
            }
        }
    }
}
