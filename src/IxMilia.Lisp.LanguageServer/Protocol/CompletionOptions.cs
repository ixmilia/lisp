using System.Collections.Generic;

namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class CompletionOptions
    {
        public List<char> TriggerCharacters { get; set; }

        public CompletionOptions()
        {
            TriggerCharacters = new List<char>()
            {
                ' ',
                '(',
            };
        }
    }
}
