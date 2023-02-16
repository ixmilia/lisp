namespace IxMilia.Lisp.Generator
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var generator = new LispGenerator();
            generator.Generate(args);
        }
    }
}
