namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class Breakpoint
    {
        public int Id { get; set; }
        public bool Verified { get; set; }
        public Source Source { get; set; }
        public int? Line { get; set; }
        
        public Breakpoint(int id, bool verified, Source source = null, int? line = null)
        {
            Id = id;
            Verified = verified;
            Source = source;
            Line = line;
        }
    }
}
