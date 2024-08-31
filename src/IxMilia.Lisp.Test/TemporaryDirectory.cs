using System;
using System.IO;

namespace IxMilia.Lisp.Test
{
    public class TemporaryDirectory : IDisposable
    {
        public string DirectoryPath { get; }

        public TemporaryDirectory()
        {
            var parentDir = Path.GetDirectoryName(GetType().Assembly.Location)!;
            var tempDirName = $"ixmilia-lisp-{Guid.NewGuid():d}";
            DirectoryPath = Path.Combine(parentDir, "test-data", tempDirName);
            Directory.CreateDirectory(DirectoryPath);
        }

        public void Dispose()
        {
            try
            {
                Directory.Delete(DirectoryPath, true);
            }
            catch
            {
            }
        }
    }
}
