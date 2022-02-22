namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class Range
    {
        public Position Start { get; set; }
        public Position End { get; set; }

        public Range(Position start, Position end)
        {
            Start = start;
            End = end;
        }

        public override string ToString()
        {
            return $"{Start}-{End}";
        }
    }
}
