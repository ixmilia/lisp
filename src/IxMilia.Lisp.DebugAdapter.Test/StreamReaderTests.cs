using System.IO;
using System.Text;
using System.Threading.Tasks;
using Xunit;

namespace IxMilia.Lisp.DebugAdapter.Test
{
    public class StreamReaderTests
    {
        [Fact]
        public async Task ReadOnlyTheSpecifiedAmount()
        {
            using var ms = new MemoryStream();
            ms.Write(Encoding.ASCII.GetBytes("Content-Length: 4\r\n\r\nabcdefg"));
            ms.Seek(0, SeekOrigin.Begin);
            var reader = new HeaderDelimitedReader(ms);
            var result = await reader.ReadAsync();
            var expected = new[]
            {
                (byte)'a',
                (byte)'b',
                (byte)'c',
                (byte)'d',
            };
            Assert.Equal(expected, result.Body);
        }
    }
}
