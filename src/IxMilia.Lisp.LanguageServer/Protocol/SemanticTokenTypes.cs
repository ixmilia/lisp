namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public static class SemanticTokenTypes
    {
        // importing values from https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens
        public static readonly string Type = CamlCase(nameof(Type));
        public static readonly string Class = CamlCase(nameof(Class));
        public static readonly string Enum = CamlCase(nameof(Enum));
        public static readonly string Interface = CamlCase(nameof(Interface));
        public static readonly string Struct = CamlCase(nameof(Struct));
        public static readonly string TypeParameter = CamlCase(nameof(TypeParameter));
        public static readonly string Parameter = CamlCase(nameof(Parameter));
        public static readonly string Variable = CamlCase(nameof(Variable));
        public static readonly string Property = CamlCase(nameof(Property));
        public static readonly string EnumMember = CamlCase(nameof(EnumMember));
        public static readonly string Event = CamlCase(nameof(Event));
        public static readonly string Function = CamlCase(nameof(Function));
        public static readonly string Method = CamlCase(nameof(Method));
        public static readonly string Macro = CamlCase(nameof(Macro));
        public static readonly string Keyword = CamlCase(nameof(Keyword));
        public static readonly string Modifier = CamlCase(nameof(Modifier));
        public static readonly string Comment = CamlCase(nameof(Comment));
        public static readonly string String = CamlCase(nameof(String));
        public static readonly string Number = CamlCase(nameof(Number));
        public static readonly string Regexp = CamlCase(nameof(Regexp));
        public static readonly string Operator = CamlCase(nameof(Operator));

        public static string[] All => new[]
        {
            Type,
            Class,
            Enum,
            Interface,
            Struct,
            TypeParameter,
            Parameter,
            Variable,
            Property,
            EnumMember,
            Event,
            Function,
            Method,
            Macro,
            Keyword,
            Modifier,
            Comment,
            String,
            Number,
            Regexp,
            Operator,
        };
        private static string CamlCase(string value) => $"{char.ToLowerInvariant(value[0])}{value.Substring(1)}";
    }
}
