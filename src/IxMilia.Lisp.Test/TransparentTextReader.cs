using System.IO;

namespace IxMilia.Lisp.Test
{
    internal class TransparentTextReader : TextReader
    {
        private TextReader _reader;
        private int _line = 1;
        private int _column = 1;

        public string Content { get; }
        public LispSourcePosition CurrentPosition => new LispSourcePosition(_line, _column);

        public TransparentTextReader(string s)
        {
            Content = s;
            _reader = new StringReader(s);
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
