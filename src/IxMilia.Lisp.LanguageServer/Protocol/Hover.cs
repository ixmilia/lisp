namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class Hover
    {
        public MarkupContent Contents { get; set; }

        public Hover(MarkupContent contents)
        {
            Contents = contents;
        }
    }
}
