using StreamJsonRpc;

namespace IxMilia.Lisp.LanguageServer
{
    public class LspMethodAttribute : JsonRpcMethodAttribute
    {
        public LspMethodAttribute(string name)
            : base(name)
        {
            UseSingleObjectParameterDeserialization = true;
        }
    }
}
