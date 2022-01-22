using System.IO;
using System.Text;

namespace IxMilia.Lisp
{
    internal class ReportingTextWriter : TextWriter
    {
        private TextWriter _writer;
        private StringBuilder _sb = new StringBuilder();

        public override Encoding Encoding => _writer.Encoding;

        public ReportingTextWriter(TextWriter writer)
        {
            _writer = writer;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                _writer.Dispose();
            }
        }

        public override void Flush()
        {
            _writer.Flush();
        }

        public override void Write(char value)
        {
            _writer.Write(value);
            _sb.Append(value);
        }

        public string GetContents() => _sb.ToString();
    }
}
