namespace IxMilia.Lisp.Test
{
    public abstract class TestBase
    {
        protected static string NormalizeNewlines(string value)
        {
            return value.Replace("\r", "");
        }
    }
}
