using System.IO;

namespace IxMilia.Lisp
{
    internal class ObservableStringReader : TextReader
    {
        private int _line = 1;
        private int _column = 1;
        private int _offset = 0;

        public string Content { get; }
        public string RemainingContent => Content.Substring(_offset);
        public LispSourcePosition CurrentPosition => new LispSourcePosition(_line, _column);

        public ObservableStringReader(string s)
        {
            Content = s;
        }

        public override int Peek()
        {
            if (_offset >= Content.Length)
            {
                return -1;
            }

            return Content[_offset];
        }

        public override int Read()
        {
            var result = Peek();
            if (result != -1)
            {
                _offset++;
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
