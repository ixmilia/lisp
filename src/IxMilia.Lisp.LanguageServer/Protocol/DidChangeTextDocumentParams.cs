using System.Collections.Generic;

namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class DidChangeTextDocumentParams
    {
        public VersionedTextDocumentIdentifier TextDocument { get; set; }
        public List<TextDocumentContentChangeEvent> ContentChanges { get; } = new List<TextDocumentContentChangeEvent>();

        public DidChangeTextDocumentParams(VersionedTextDocumentIdentifier textDocument, IEnumerable<TextDocumentContentChangeEvent> contentChanges)
        {
            TextDocument = textDocument;
            ContentChanges.AddRange(contentChanges);
        }
    }
}
