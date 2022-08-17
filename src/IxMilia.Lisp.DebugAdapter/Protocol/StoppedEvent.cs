using Newtonsoft.Json;

namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class StoppedEvent : Event
    {
        public const string EventName = "stopped";

        public StoppedEventBody Body { get; set; }

        public StoppedEvent(int seq, StoppedEventBody body)
            : base(seq, EventName)
        {
            Body = body;
        }
    }

    public class StoppedEventBody
    {
        public string Reason { get; set; }

        [JsonProperty(NullValueHandling = NullValueHandling.Ignore)]
        public string Description { get; set; }
        [JsonProperty(NullValueHandling = NullValueHandling.Ignore)]
        public int? ThreadId { get; set; }
        [JsonProperty(NullValueHandling = NullValueHandling.Ignore)]
        public string Text { get; set; }
        [JsonProperty(NullValueHandling = NullValueHandling.Ignore)]
        public int[] HitBreakpointIds { get; set; }

        public StoppedEventBody(string reason, string description = null, int? threadId = null, string text = null, int[] hitBreakpointIds = null)
        {
            Reason = reason;
            Description = description;
            ThreadId = threadId;
            Text = text;
            HitBreakpointIds = hitBreakpointIds;
        }
    }
}
