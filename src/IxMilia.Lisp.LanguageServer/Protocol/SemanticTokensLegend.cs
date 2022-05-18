namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class SemanticTokensLegend
    {
        public string[] TokenTypes { get; set; } = SemanticTokenTypes.All;

        public string[] TokenModifiers { get; set; } = new[]
        {
            SemanticTokenModifiers.Declaration,
        };
    }
}
