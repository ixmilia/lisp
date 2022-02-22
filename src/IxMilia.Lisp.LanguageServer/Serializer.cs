using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using Newtonsoft.Json.Serialization;

namespace IxMilia.Lisp.LanguageServer
{
    public class Serializer
    {
        private static JsonSerializer _serializer;

        static Serializer()
        {
            _serializer = new JsonSerializer();
            ConfigureSerializer(_serializer);
        }

        public static void ConfigureSerializer(JsonSerializer serializer)
        {
            serializer.ContractResolver = new CamelCasePropertyNamesContractResolver();
            serializer.Converters.Add(new StringEnumConverter(typeof(CamelCaseNamingStrategy)));
        }
    }
}
