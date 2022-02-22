namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class Position
    {
        public uint Line { get; set; }
        public uint Character { get; set; }

        public Position(uint line, uint character)
        {
            Line = line;
            Character = character;
        }

        public override string ToString()
        {
            return $"({Line}, {Character})";
        }

        internal int GetIndex(string value)
        {
            var index = 0;
            var line = 0;
            var character = 0;
            foreach (var c in value)
            {
                if (Line == line && Character == character)
                {
                    break;
                }

                character++;
                if (c == '\n')
                {
                    line++;
                    character = 0;
                }

                index++;
            }

            return index;
        }
    }
}
