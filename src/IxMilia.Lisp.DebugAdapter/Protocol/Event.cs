using Newtonsoft.Json;

namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class Event : ProtocolMessage
    {
        [JsonProperty("event")]
        public string EventType { get; set; }

        public Event(int seq, string eventType)
            : base(seq, ProtocolMessageType.Event)
        {
            EventType = eventType;
        }
    }
}
