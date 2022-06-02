namespace IxMilia.Lisp.LanguageServer.Protocol
{
    public class EvalResult
    {
        public bool IsError { get; set; }
        public string Content { get; set; }

        public EvalResult(bool isError, string content)
        {
            IsError = isError;
            Content = content;
        }
    }
}
