using System.IO;

namespace IxMilia.Lisp
{
    internal class ReportingTextReader : TextReader
    {
        private TextReader _reader;
        private int _line = 1;
        private int _column = 1;

        public LispSourcePosition CurrentPosition => new LispSourcePosition(_line, _column);

        public ReportingTextReader(TextReader reader)
        {
            _reader = reader;
        }

        public override int Peek() => _reader.Peek();

        public override int Read()
        {
            var result = _reader.Read();
            if (result != -1)
            {
                _column++;
                var c = (char)result;
                if (c == '\n')
                {
                    _line++;
                    _column = 1;
                }
            }

            return result;
        }
    }
}
