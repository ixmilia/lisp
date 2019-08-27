using System.Collections.Generic;
using System.Text;

namespace IxMilia.Lisp.Tokens
{
    public class LispTriviaCollection
    {
        private List<LispTrivia> _trivia;

        public LispTriviaCollection()
        {
            _trivia = new List<LispTrivia>();
        }

        public LispTriviaCollection(IEnumerable<LispTrivia> trivia)
            : this()
        {
            _trivia.AddRange(trivia);
        }

        public override string ToString()
        {
            var builder = new StringBuilder();
            foreach (var t in _trivia)
            {
                builder.Append(t.ToString());
            }

            return builder.ToString();
        }
    }
}
