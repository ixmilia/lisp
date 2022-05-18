using System;
using IxMilia.Lisp.LanguageServer.Protocol;

namespace IxMilia.Lisp.LanguageServer
{
    public static class Extensions
    {
        public static string AsTokenTypeString(this LispTokenType tokenType)
        {
            return tokenType switch
            {
                LispTokenType.Comment => SemanticTokenTypes.Comment,
                LispTokenType.Function => SemanticTokenTypes.Function,
                LispTokenType.Macro => SemanticTokenTypes.Macro,
                LispTokenType.Number => SemanticTokenTypes.Number,
                LispTokenType.Package => SemanticTokenTypes.Type,
                LispTokenType.Parameter => SemanticTokenTypes.Parameter,
                LispTokenType.String => SemanticTokenTypes.String,
                LispTokenType.Variable => SemanticTokenTypes.Variable,
                _ => throw new ArgumentException(nameof(tokenType)),
            };
        }
    }
}
