using System.Collections.Generic;
using System.Linq;

namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class CompletionList
    {
        public bool IsIncomplete { get; set; }
        public List<CompletionItem> Items { get; set; }

        public CompletionList(bool isIncomplete, IEnumerable<CompletionItem> items)
        {
            IsIncomplete = isIncomplete;
            Items = items.ToList();
        }
    }
}
