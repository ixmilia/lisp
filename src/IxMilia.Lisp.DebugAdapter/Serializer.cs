using System;
using System.IO;
using IxMilia.Lisp.DebugAdapter.Protocol;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using Newtonsoft.Json.Linq;
using Newtonsoft.Json.Serialization;

namespace IxMilia.Lisp.DebugAdapter
{
    public class Serializer
    {
        public static JsonSerializer Instance;

        static Serializer()
        {
            Instance = new JsonSerializer();
            ConfigureSerializer(Instance);
        }

        public static void ConfigureSerializer(JsonSerializer serializer)
        {
            serializer.ContractResolver = new CamelCasePropertyNamesContractResolver();
            serializer.Converters.Add(new StringEnumConverter(typeof(CamelCaseNamingStrategy)));
            serializer.Converters.Add(new ProtocolMessageConverter());
        }

        public static T Deserialize<T>(string json)
        {
            var reader = new JsonTextReader(new StringReader(json));
            var result = Instance.Deserialize<T>(reader);
            return result;
        }

        public static string Serialize(object value)
        {
            var stringWriter = new StringWriter();
            var writer = new JsonTextWriter(stringWriter);
            Instance.Serialize(writer, value);
            writer.Flush();
            return stringWriter.ToString();
        }
    }

    public class ProtocolMessageConverter : JsonConverter<ProtocolMessage>
    {
        public override bool CanWrite => false;

        public override ProtocolMessage ReadJson(JsonReader reader, Type objectType, ProtocolMessage existingValue, bool hasExistingValue, JsonSerializer serializer)
        {
            var rawObject = JObject.Load(reader);
            var typeString = rawObject["type"]?.ToString();
            switch (typeString)
            {
                case Request.RequestType:
                    return LoadRequest(rawObject);
            }

            throw new NotImplementedException();
        }

        public override void WriteJson(JsonWriter writer, ProtocolMessage value, JsonSerializer serializer)
        {
            throw new NotImplementedException();
        }

        private Request LoadRequest(JObject rawObject)
        {
            var commandString = rawObject["command"]?.ToString();
            switch (commandString)
            {
                case ConfigurationDoneRequest.CommandName:
                    return rawObject.ToObject<ConfigurationDoneRequest>();
                case ContinueRequest.CommandName:
                    return rawObject.ToObject<ContinueRequest>();
                case DisconnectRequest.CommandName:
                    return rawObject.ToObject<DisconnectRequest>();
                case EvaluateRequest.CommandName:
                    return rawObject.ToObject<EvaluateRequest>();
                case InitializeRequest.CommandName:
                    return rawObject.ToObject<InitializeRequest>();
                case LaunchRequest.CommandName:
                    return rawObject.ToObject<LaunchRequest>();
                case ScopesRequest.CommandName:
                    return rawObject.ToObject<ScopesRequest>();
                case SetBreakpointsRequest.CommandName:
                    return rawObject.ToObject<SetBreakpointsRequest>();
                case SetExceptionBreakpointsRequest.CommandName:
                    return rawObject.ToObject<SetExceptionBreakpointsRequest>();
                case SetFunctionBreakpointsRequest.CommandName:
                    return rawObject.ToObject<SetFunctionBreakpointsRequest>();
                case SourceRequest.CommandName:
                    return rawObject.ToObject<SourceRequest>();
                case StackTraceRequest.CommandName:
                    return rawObject.ToObject<StackTraceRequest>();
                case ThreadsRequest.CommandName:
                    return rawObject.ToObject<ThreadsRequest>();
                case VariablesRequest.CommandName:
                    return rawObject.ToObject<VariablesRequest>();
            }

            throw new NotImplementedException(rawObject.ToString());
        }
    }
}
