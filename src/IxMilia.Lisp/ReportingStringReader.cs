using System.IO;

namespace IxMilia.Lisp
{
    internal class ReportingStringReader : TextReader
    {
        private StringReader _reader;

        public int Offset { get; private set; } = 0;
        public int Length { get; }

        public bool IsComplete => Offset >= Length;

        public ReportingStringReader(string s)
        {
            _reader = new StringReader(s);
            Length = s.Length;
        }

        public override int Peek()
        {
            return _reader.Peek();
        }

        public override int Read()
        {
            var result = _reader.Read();
            Offset++;
            return result;
        }
    }
}
