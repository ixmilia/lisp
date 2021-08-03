namespace IxMilia.Lisp
{
    public struct LispSourceLocation
    {
        public string FilePath { get; }
        public int Line { get; }
        public int Column { get; }

        public LispSourceLocation(string filePath, int line, int column)
        {
            FilePath = filePath;
            Line = line;
            Column = column;
        }
    }
}
