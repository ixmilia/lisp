namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class SemanticTokensOptions
    {
        public SemanticTokensLegend Legend { get; set; } = new SemanticTokensLegend();
        public bool Full { get; set; } = true;
    }
}
