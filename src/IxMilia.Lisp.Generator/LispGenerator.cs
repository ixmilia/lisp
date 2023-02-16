using System;
using System.IO;
using System.Linq;
using System.Reflection;

namespace IxMilia.Lisp.Generator
{
    public class LispGenerator
    {
        public void Generate(string[] args)
        {
            var types = new[]
            {
                typeof(LispDefaultContext),
                typeof(LispSpecialOperatorsContext),
            };
            var names = types
                .SelectMany(t => t.GetTypeInfo().DeclaredMethods)
                .SelectMany(m => m.GetCustomAttributes<LispInvokableTargetAttribute>())
                .Select(a => a.Name)
                .OrderBy(n => n)
                .Select(n => $"\"{n}\"")
                .ToList();
            var declaration = $"export const builtInNames: string[] = [\n    {string.Join(",\n    ", names)}\n];\n";
            foreach (var outputFile in args)
            {
                Console.WriteLine($"Generating contracts to {outputFile}...");
                File.WriteAllText(outputFile, declaration);
            }
        }
    }
}
