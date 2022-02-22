using System;
using IxMilia.Lisp.LanguageServer.Protocol;

namespace IxMilia.Lisp.LanguageServer
{
    public class Converters
    {
        public static string PathFromUri(string uriString)
        {
            var uri = new Uri(uriString);
            var localPath = uri.LocalPath;
            if (localPath.Length >= 3 &&
                localPath[0] == '/' &&
                char.IsLetter(localPath[1]) &&
                localPath[2] == ':')
            {
                // starts with something like:
                //   /c:/Users...
                localPath = localPath.Substring(1);
            }

            return localPath;
        }

        public static LispSourcePosition SourcePositionFromPosition(Position position)
        {
            return new LispSourcePosition((int)position.Line + 1, (int)position.Character + 1);
        }
    }
}
