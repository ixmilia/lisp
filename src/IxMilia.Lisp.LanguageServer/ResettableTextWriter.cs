using System.IO;
using System.Text;

namespace IxMilia.Lisp.LanguageServer
{
    internal class ResettableTextWriter : TextWriter
    {
        private StringBuilder _builder = new StringBuilder();

        public override Encoding Encoding => Encoding.UTF8;

        public override void Write(char value)
        {
            _builder.Append(value);
        }

        public void Reset() => _builder.Clear();

        public string GetText() => _builder.ToString();
    }
}
