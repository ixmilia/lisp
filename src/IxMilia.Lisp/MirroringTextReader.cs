using System;
using System.IO;

namespace IxMilia.Lisp
{
    internal class MirroringTextReader : TextReader
    {
        private TextReader _reader;
        private Action<char> _characterRead;

        public MirroringTextReader(TextReader reader, Action<char> characterRead)
        {
            _reader = reader;
            _characterRead = characterRead;
        }

        public override int Peek()
        {
            return _reader.Peek();
        }

        public override int Read()
        {
            var result = _reader.Read();
            if (result != -1)
            {
                var c = (char)result;
                _characterRead(c);
            }

            return result;
        }
    }
}
