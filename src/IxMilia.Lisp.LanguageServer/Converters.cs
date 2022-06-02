using System;
using IxMilia.Lisp.LanguageServer.Protocol;

namespace IxMilia.Lisp.LanguageServer
{
    public class Converters
    {
        public static string PathFromUri(string uriString)
        {
            var result = uriString;
            var uri = new Uri(uriString);
            var localPath = uri.LocalPath;
            if (uri.Scheme == "file" &&
                localPath.Length >= 1 &&
                localPath[0] == '/')
            {
                if (localPath.Length >= 3 &&
                    char.IsLetter(localPath[1]) &&
                    localPath[2] == ':')
                {
                    // looks like:
                    //   file:///c:/Users...
                    result = localPath.Substring(1);
                }
                else
                {
                    // looks like:
                    //   file:///usr/test...
                    result = localPath;
                }
            }

            return result;
        }

        public static Position PositionFromSourcePosition(LispSourcePosition position)
        {
            return new Position((uint)(position.Line - 1), (uint)(position.Column - 1));
        }

        public static LispSourcePosition SourcePositionFromPosition(Position position)
        {
            return new LispSourcePosition((int)position.Line + 1, (int)position.Character + 1);
        }
    }
}
