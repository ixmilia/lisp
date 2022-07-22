using Newtonsoft.Json;

namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public abstract class Response : ProtocolMessage
    {
        [JsonProperty("request_seq")]
        public int RequestSeq { get; set; }
        public bool Success { get; set; }
        public string Command { get; set; }
        [JsonProperty(NullValueHandling = NullValueHandling.Ignore)]
        public string Message { get; set; }

        public Response(int seq, int requestSeq, bool success, string command, string message)
            : base(seq, ProtocolMessageType.Response)
        {
            RequestSeq = requestSeq;
            Success = success;
            Command = command;
            Message = message;
        }
    }

    public class ErrorResponse : Response
    {
        public ErrorResponse(int seq, int requestSeq, string command, string message)
            : base(seq, requestSeq, false, command, message)
        {
        }
    }
}
