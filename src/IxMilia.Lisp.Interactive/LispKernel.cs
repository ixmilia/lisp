using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.DotNet.Interactive;
using Microsoft.DotNet.Interactive.Commands;
using Microsoft.DotNet.Interactive.Events;
using Microsoft.DotNet.Interactive.ValueSharing;
using Newtonsoft.Json.Linq;

namespace IxMilia.Lisp.Interactive
{
    public class LispKernel :
        Kernel,
        IKernelCommandHandler<RequestCompletions>,
        IKernelCommandHandler<RequestHoverText>,
        IKernelCommandHandler<RequestValue>,
        IKernelCommandHandler<RequestValueInfos>,
        IKernelCommandHandler<SendValue>,
        IKernelCommandHandler<SubmitCode>
    {
        private Lazy<Task<LispRepl>> _repl;
        private HashSet<string> _suppressedValues = new HashSet<string>();

        public LispKernel()
            : base("lisp")
        {
            KernelInfo.LanguageName = "Lisp";
            KernelInfo.DisplayName = "Lisp";
            _repl = new Lazy<Task<LispRepl>>(async () =>
            {
                var repl = await LispRepl.CreateAsync(location: "*REPL*");
                _suppressedValues = new HashSet<string>(repl.Host.RootFrame.GetValues().Select(v => v.Item1.Value));
                return repl;
            });

            this.UseValueSharing();
        }

        public async Task HandleAsync(RequestCompletions command, KernelInvocationContext context)
        {
            var repl = await _repl.Value;
            var parseResult = await repl.ParseUntilSourceLocationAsync(command.Code, new LispSourcePosition(command.LinePosition.Line + 1, command.LinePosition.Character + 1));
            var completionItems = parseResult.GetReducedCompletionItems(repl.Host.CurrentPackage,
                (symbol, value) =>
                    new CompletionItem(
                        displayText: symbol.ToDisplayString(repl.Host.CurrentPackage),
                        kind: "",
                        documentation: value is LispFunction f ? f.Documentation : null),
                name =>
                    new CompletionItem(
                        displayText: name,
                        kind: "")).ToList();
            if (completionItems.Count > 0)
            {
                context.Publish(new CompletionsProduced(completionItems, command));
            }
        }

        public async Task HandleAsync(RequestHoverText command, KernelInvocationContext context)
        {
            var repl = await _repl.Value;
            var parseResult = await repl.ParseUntilSourceLocationAsync(command.Code, new LispSourcePosition(command.LinePosition.Line + 1, command.LinePosition.Character + 1));
            if (parseResult.Object != null)
            {
                var markdown = parseResult.GetMarkdownDisplay();
                if (markdown != null)
                {
                    LinePositionSpan span = null;
                    var formatted = new FormattedValue("text/markdown", markdown);
                    if (parseResult.Object.SourceLocation.HasValue)
                    {
                        span = new LinePositionSpan(
                            new LinePosition(parseResult.Object.SourceLocation.Value.Start.Line - 1, parseResult.Object.SourceLocation.Value.Start.Column - 1),
                            new LinePosition(parseResult.Object.SourceLocation.Value.End.Line - 1, parseResult.Object.SourceLocation.Value.End.Column - 1)
                        );
                    }

                    context.Publish(new HoverTextProduced(command, new[] { formatted }, span));
                }
            }
        }

        public async Task HandleAsync(RequestValue command, KernelInvocationContext context)
        {
            if (_repl.IsValueCreated)
            {
                var repl = await _repl.Value;
                var formattedValue = await GetFormattedValue(command.Name, command.MimeType);
                if (formattedValue is { })
                {
                    context.Publish(new ValueProduced(null, command.Name, formattedValue, command));
                }
            }
        }

        public async Task HandleAsync(RequestValueInfos command, KernelInvocationContext context)
        {
            var kernelValueInfos = new KernelValueInfo[0];
            if (_repl.IsValueCreated)
            {
                var repl = await _repl.Value;
                var valueInfoTasks = repl.Host.RootFrame.GetValues()
                    .Where(v => !(v.Item2 is LispInvocableObject))
                    .Select(v => v.Item1)
                    .Where(v => !_suppressedValues.Contains(v.Value))
                    .Select(async v =>
                    {
                        var displayName = v.ToDisplayString(repl.Host.CurrentPackage);
                        var formattedValue = await GetFormattedValue(displayName, command.MimeType);
                        return new KernelValueInfo(displayName, formattedValue);
                    });
                kernelValueInfos = await Task.WhenAll(valueInfoTasks);
            }

            context.Publish(new ValueInfosProduced(kernelValueInfos, command));
        }

        public async Task HandleAsync(SendValue command, KernelInvocationContext context)
        {
            switch (command.FormattedValue.MimeType)
            {
                case "application/json":
                    if (TryGetLispObjectFromJson(command.FormattedValue.Value, out var obj) &&
                        TryGetDeclarationStatementForObject(obj, command.Name, out var declarationStatement))
                    {
                        var repl = await _repl.Value;
                        await repl.EvalAsync(declarationStatement, consumeIncompleteInput: false);
                    }

                    break;
                default:
                    // TODO: handle other mime types
                    break;
            }
        }

        public async Task HandleAsync(SubmitCode command, KernelInvocationContext context)
        {
            var repl = await _repl.Value;
            var writer = new ListeningTextWriter(line =>
            {
                var formatted = new FormattedValue("text/plain", line);
                context.Publish(new StandardOutputValueProduced(command, new[] { formatted }));
            });
            var consoleStream = new LispTextStream("", TextReader.Null, writer);
            repl.Host.SetValue("*TERMINAL-IO*", consoleStream);

            var result = await repl.EvalAsync(command.Code, consumeIncompleteInput: false);
            switch (result.ExecutionState.LastResult)
            {
                case LispError err:
                    var errorLocation = err.SourceLocation;
                    Console.WriteLine($"stack frame: {err.StackFrame}");
                    var replFrame = err.StackFrame;
                    while (replFrame != null)
                    {
                        if (replFrame.SourceLocation?.FilePath == "*REPL*")
                        {
                            errorLocation = replFrame.SourceLocation;
                            break;
                        }

                        replFrame = replFrame.Parent;
                    }

                    if (errorLocation != null)
                    {
                        var diag = new Microsoft.DotNet.Interactive.Diagnostic(
                            new LinePositionSpan(
                                new LinePosition(errorLocation.Value.Start.Line - 1, errorLocation.Value.Start.Column - 1),
                                new LinePosition(errorLocation.Value.End.Line - 1, errorLocation.Value.End.Column - 1)),
                            DiagnosticSeverity.Error,
                            "LISP0001",
                            err.Message);
                        context.Publish(new DiagnosticsProduced(new[] { diag }, command));
                    }

                    context.Fail(command, null, err.Message);
                    break;
                case LispObject obj:
                    if (!obj.IsNil())
                    {
                        var formattedText = new FormattedValue("text/plain", obj.ToString());
                        var formattedJson = new FormattedValue("application/json", obj.ToJsonString());
                        context.Publish(new ReturnValueProduced(obj, command, new[] { formattedText, formattedJson }));
                    }

                    context.Publish(new DiagnosticsProduced(new Microsoft.DotNet.Interactive.Diagnostic[0], command));
                    break;
            }
        }

        public async Task<FormattedValue> GetFormattedValue(string valueName, string mimeType)
        {
            var repl = await _repl.Value;
            var foundValuePair = repl.Host.RootFrame.GetValues().FirstOrDefault(v => v.Item1.LocalName == valueName || v.Item1.Value == valueName);
            if (foundValuePair is { })
            {
                var formatted = mimeType switch
                {
                    "application/json" => foundValuePair.Item2.ToJsonString(),
                    _ => foundValuePair.Item2.ToDisplayString(repl.Host.CurrentPackage),
                };

                return new FormattedValue(mimeType, formatted); ;
            }

            return null;
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
            var valueToSet = obj.FormatAsSExpression();
            if (obj is LispList)
            {
                // lists need to be quoted
                valueToSet = "'" + valueToSet;
            }

            // add a trailing `()` so there is no `ReturnValueProduced` generated when executing this code
            declarationStatement = $"(SETF {valueName} {valueToSet}) ()";
            return valueToSet is { };
        }
    }
}
