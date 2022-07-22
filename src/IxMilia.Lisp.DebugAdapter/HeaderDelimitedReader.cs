using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace IxMilia.Lisp.DebugAdapter
{
    public class HeaderDelimitedReader
    {
        private const int BufferSize = 256;
        private static byte[] HeaderDelimiter = new[] { (byte)'\r', (byte)'\n', (byte)'\r', (byte)'\n' };

        private Stream _receivingStream;
        private List<byte> _incompleteBuffer = new List<byte>();
        private byte[] _buffer = new byte[BufferSize];

        public HeaderDelimitedReader(Stream receivingStream)
        {
            _receivingStream = receivingStream;
        }

        public async Task<ReaderResult> ReadAsync()
        {
            Dictionary<string, string> headerDictionary = null;
            byte[] body = null;

            // get header values
            while (true)
            {
                // check for complete header
                var headerDelimiter = Find(HeaderDelimiter);
                if (headerDelimiter >= 0)
                {
                    headerDelimiter += HeaderDelimiter.Length;
                    var headerText = Encoding.ASCII.GetString(_incompleteBuffer.Take(headerDelimiter).ToArray()).Trim();
                    _incompleteBuffer.RemoveRange(0, headerDelimiter);
                    var headerLines = headerText.Split('\n').Select(line => line.Trim());
                    var headerTuples = headerLines.Select(line => line.Split(new[] { ':' }, 2));
                    headerDictionary = headerTuples.ToDictionary(tuple => tuple[0].Trim(), tuple => tuple[1].Trim(), StringComparer.OrdinalIgnoreCase);
                    break;
                }

                // read more data
                var readCount = await _receivingStream.ReadAsync(_buffer, 0, _buffer.Length);
                for (int i = 0; i < readCount; i++)
                {
                    _incompleteBuffer.Add(_buffer[i]);
                }
            }

            // get body
            var contentLength = int.Parse(headerDictionary["Content-Length"]);
            while (true)
            {
                // check for complete body
                if (_incompleteBuffer.Count >= contentLength)
                {
                    body = _incompleteBuffer.Take(contentLength).ToArray();
                    _incompleteBuffer.RemoveRange(0, contentLength);
                    break;
                }

                // read more data
                var readCount = await _receivingStream.ReadAsync(_buffer, 0, _buffer.Length);
                for (int i = 0; i < readCount; i++)
                {
                    _incompleteBuffer.Add(_buffer[i]);
                }
            }

            return new ReaderResult(headerDictionary, body);
        }

        private int Find(byte[] delimiter)
        {
            for (int i = 0; i < _incompleteBuffer.Count - delimiter.Length; i++)
            {
                var matched = true;
                for (int j = 0; j < delimiter.Length; j++)
                {
                    if (delimiter[j] != _incompleteBuffer[i + j])
                    {
                        matched = false;
                        break;
                    }
                }

                if (matched)
                {
                    return i;
                }
            }

            return -1;
        }
    }
}
