namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class TextDocumentContentChangeEvent
    {
        public Range Range { get; set; }
        public uint? RangeLength { get; set; }
        public string Text { get; set; }

        public TextDocumentContentChangeEvent(Range range, uint? rangeLength, string text)
        {
            Range = range;
            RangeLength = rangeLength;
            Text = text;
        }
    }
}
