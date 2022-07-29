using System;
using System.Linq;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace IxMilia.Lisp.Interactive
{
    public static class LispObjectExtensions
    {
        public static string ToJsonString(this LispObject o) => o.ToJson().ToString(Formatting.None);

        private static JToken ToJson(this LispObject o)
        {
            switch (o)
            {
                case LispFloat f:
                    return new JValue(f.Value);
                case LispInteger i:
                    return new JValue(i.Value);
                case LispRatio r:
                    return new JValue(r.AsFloat().Value);
                case LispComplexNumber q:
                    return new JArray(q.RealPart.ToJson(), q.ImaginaryPart.ToJson());
                case LispCharacter c:
                    return new JValue(c.Value.ToString());
                case LispString s:
                    return new JValue(s.Value);
                case LispList l:
                    return new JArray(l.ToList().Select(v => v.ToJson()));
                case LispVector v:
                    return new JArray(v.Items.Select(i => i.ToJson()));
                case LispSymbol s:
                    return new JValue(s.ToString());
                case LispLambdaListKeyword k:
                    return new JValue(k.Keyword);
                case LispError e:
                    return new JObject(new JProperty("MESSAGE", new JValue(e.Message)));
                default:
                    throw new NotSupportedException();
            }
        }
    }
}
