using System;
using System.IO;
using System.Text;

namespace IxMilia.Lisp
{
    internal class ListeningTextWriter : TextWriter
    {
        private StringBuilder _sb = new StringBuilder();
        private Action<string> _lineWritten;

        public override Encoding Encoding => Encoding.UTF8;

        public ListeningTextWriter(Action<string> lineWritten)
        {
            _lineWritten = lineWritten ?? throw new ArgumentNullException(nameof(lineWritten));
        }

        public override void Write(char value)
        {
            _sb.Append(value);
            if (value == '\n')
            {
                Flush();
            }
        }

        public override void Flush()
        {
            _lineWritten(_sb.ToString());
            _sb.Clear();
        }
    }
}
