namespace IxMilia.Lisp.Test
{
    public class ObjectReadTests_NoReaderMacros: ObjectReadTestsBase
    {
        public override LispReaderType ReaderType => LispReaderType.NoReaderMacros;
    }
}
