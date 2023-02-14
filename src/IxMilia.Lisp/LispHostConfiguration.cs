using System;
using System.IO;

namespace IxMilia.Lisp
{
    public enum LispReaderType
    {
        Compiled,
        Interpreted,
    }

    public class LispHostConfiguration
    {
        public bool UseInitScript { get; }
        public bool UseTailCalls { get; }
        public bool UseJustMyCode { get; }
        public LispReaderType ReaderType { get; }
        public TextReader Input { get; }
        public TextWriter Output { get; }
        public Func<LispResolvedSymbol, LispObject> GetUntrackedValue { get; }
        public Func<LispResolvedSymbol, LispObject, bool> TrySetUntrackedValue { get; }

        public LispHostConfiguration()
        {
            UseInitScript = true;
            UseTailCalls = true;
            UseJustMyCode = true;
            ReaderType = LispReaderType.Compiled;
            Input = TextReader.Null;
            Output = TextWriter.Null;
            GetUntrackedValue = null;
            TrySetUntrackedValue = null;
        }

        public LispHostConfiguration(
            bool useInitScript = true,
            bool useTailCalls = true,
            bool useJustMyCode = true,
            LispReaderType readerType = LispReaderType.Compiled,
            TextReader input = null,
            TextWriter output = null,
            Func<LispResolvedSymbol, LispObject> getUntrackedValue = null,
            Func<LispResolvedSymbol, LispObject, bool> trySetUntrackedValue = null)
        {
            UseInitScript = useInitScript;
            UseTailCalls = useTailCalls;
            UseJustMyCode = useJustMyCode;
            ReaderType = readerType;
            Input = input ?? TextReader.Null;
            Output = output ?? TextWriter.Null;
            GetUntrackedValue = getUntrackedValue;
            TrySetUntrackedValue = trySetUntrackedValue;
        }
    }
}
