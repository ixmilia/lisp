using System;
using System.IO;

namespace IxMilia.Lisp.Test
{
    public class TemporaryFile : IDisposable
    {
        public string FilePath { get; }

        public TemporaryFile(bool createFile)
        {
            var tempDir = Path.GetTempPath();
            var fileName = "IxMilia.Lisp.Test-" + Guid.NewGuid().ToString() + ".tmp";
            FilePath = Path.Combine(tempDir, fileName);
            if (createFile)
            {
                using (var _ = File.Create(FilePath))
                {
                }
            }
        }

        public void Dispose()
        {
            try
            {
                File.Delete(FilePath);
            }
            catch
            {
            }
        }
    }
}
