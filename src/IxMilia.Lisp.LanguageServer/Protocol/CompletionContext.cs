namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class CompletionContext
    {
        public CompletionTriggerKind TriggerKind { get; set; }
        public char TriggerCharacter { get; set; }

        public CompletionContext(CompletionTriggerKind triggerKind, char triggerCharacter)
        {
            TriggerKind = triggerKind;
            TriggerCharacter = triggerCharacter;
        }
    }
}
