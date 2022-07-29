using System.Collections.Generic;
using System.Text;
using Microsoft.DotNet.Interactive.Commands;
using Microsoft.DotNet.Interactive.Events;
using Microsoft.DotNet.Interactive.ValueSharing;
using Newtonsoft.Json.Linq;

namespace IxMilia.Lisp.Interactive
{
    public class LispValueDeclarer : IKernelValueDeclarer
    {
        public bool TryGetValueDeclaration(ValueProduced valueProduced, out KernelCommand command)
        {
            command = null;
            switch (valueProduced.FormattedValue.MimeType)
            {
                case "application/json":
                    if (TryGetLispObjectFromJson(valueProduced.FormattedValue.Value, out var obj) &&
                        TryGetDeclarationStatementForObject(obj, valueProduced.Name, out var declarationStatement))
                    {
                        command = new SubmitCode(declarationStatement);
                        return true;
                    }

                    break;
            }

            return command is { };
        }

        public static bool TryGetLispObjectFromJson(string json, out LispObject result)
        {
            var token = JToken.Parse(json);
            return TryGetLispObjectFromJToken(token, out result);
        }

        private static bool TryGetLispObjectFromJToken(JToken token, out LispObject result)
        {
            result = null;
            switch (token.Type)
            {
                case JTokenType.Array:
                    {
                        var array = (JArray)token;
                        var arrayValues = new List<LispObject>();
                        foreach (var value in array.Values())
                        {
                            if (TryGetLispObjectFromJToken(value, out var item))
                            {
                                arrayValues.Add(item);
                            }
                        }

                        result = LispList.FromEnumerable(arrayValues);
                    }
                    break;
                case JTokenType.Boolean:
                    {
                        var value = (bool)((JValue)token).Value;
                        result = value ? (LispObject)new LispInteger(1) : LispList.FromItems();
                    }
                    break;
                case JTokenType.Integer:
                    {
                        var value = (int)(long)((JValue)token).Value;
                        result = new LispInteger(value);
                    }
                    break;
                case JTokenType.Float:
                    {
                        var value = (double)((JValue)token).Value;
                        result = new LispFloat(value);
                    }
                    break;
                case JTokenType.String:
                    {
                        var value = (string)((JValue)token).Value;
                        result = new LispString(value);
                    }
                    break;
                case JTokenType.Null:
                case JTokenType.Undefined:
                    result = LispList.FromItems();
                    break;
                case JTokenType.Object:
                    {
                        var obj = (JObject)token;
                        var values = new List<LispObject>();
                        foreach (var prop in obj.Properties())
                        {
                            if (TryGetLispObjectFromJToken(prop.Value, out var propertyValue))
                            {
                                values.Add(LispSymbol.CreateFromString(prop.Name, "KEYWORD"));
                                values.Add(propertyValue);
                            }
                        }

                        result = LispList.FromEnumerable(values);
                    }
                    break;
            }

            return result is { };
        }

        public static bool TryGetDeclarationStatementForObject(LispObject obj, string valueName, out string declarationStatement)
        {
            declarationStatement = null;
            string valueToSet = null;
            switch (obj)
            {
                case LispCharacter _:
                case LispNumber _:
                    valueToSet = obj.ToString();
                    break;
                case LispList l:
                    valueToSet = $"'{l}";
                    break;
                case LispString s:
                    var sb = new StringBuilder();
                    sb.Append('"');
                    foreach (var c in s.Value)
                    {
                        if (c == '\\' || c == '"')
                        {
                            sb.Append('\\');
                        }

                        sb.Append(c);
                    }

                    sb.Append('"');
                    valueToSet = sb.ToString();
                    break;
            }

            // add a trailing `()` so there is no `ReturnValueProduced` generated when executing this code
            declarationStatement = $"(SETF {valueName} {valueToSet}) ()";
            return valueToSet is { };
        }
    }
}
