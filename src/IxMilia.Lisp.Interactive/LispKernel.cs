using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reactive.Subjects;
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
        private Lazy<Task<LispHost>> _host;
        private HashSet<string> _suppressedValues = new HashSet<string>();
        private Subject<string> _stdoutSubject = new Subject<string>();

        public LispKernel()
            : base("lisp")
        {
            KernelInfo.LanguageName = "Lisp";
            KernelInfo.DisplayName = "Lisp";
            _host = new Lazy<Task<LispHost>>(async () =>
            {
                var writer = new ListeningTextWriter(line => _stdoutSubject.OnNext(line));
                var configuration = new LispHostConfiguration(output: writer, readerType: LispReaderType.NoReaderMacros);
                var host = await LispHost.CreateAsync(configuration);
                _suppressedValues = new HashSet<string>(host.RootFrame.GetValues().Select(v => v.Item1.Value));
                return host;
            });

            this.UseValueSharing();
        }

        public async Task HandleAsync(RequestCompletions command, KernelInvocationContext context)
        {
            var host = await _host.Value;
            var parseResult = await host.ParseUntilSourceLocationAsync("*REPL*", command.Code, new LispSourcePosition(command.LinePosition.Line + 1, command.LinePosition.Character + 1));
            var completionItems = parseResult.GetReducedCompletionItems(host.CurrentPackage,
                (symbol, value) =>
                    new CompletionItem(
                        displayText: symbol.ToDisplayString(host.CurrentPackage),
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
            var host = await _host.Value;
            var parseResult = await host.ParseUntilSourceLocationAsync("*REPL*", command.Code, new LispSourcePosition(command.LinePosition.Line + 1, command.LinePosition.Character + 1));
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
            if (_host.IsValueCreated)
            {
                var host = await _host.Value;
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
            if (_host.IsValueCreated)
            {
                var host = await _host.Value;
                var valueInfoTasks = host.RootFrame.GetValues()
                    .Where(v => !(v.Item2 is LispInvocableObject))
                    .Select(v => v.Item1)
                    .Where(v => !_suppressedValues.Contains(v.Value))
                    .Select(async v =>
                    {
                        var displayName = v.ToDisplayString(host.CurrentPackage);
                        var formattedValue = await GetFormattedValue(displayName, command.MimeType);
                        return new KernelValueInfo(displayName, formattedValue);
                    });
                kernelValueInfos = await Task.WhenAll(valueInfoTasks);
            }

            context.Publish(new ValueInfosProduced(kernelValueInfos, command));
        }

        public async Task HandleAsync(SendValue command, KernelInvocationContext context)
        {
            var host = await _host.Value;
            switch (command.FormattedValue.MimeType)
            {
                case "application/json":
                case "text/json":
                    if (TryGetLispObjectFromJson(host, command.FormattedValue.Value, out var obj))
                    {
                        var symbolStream = new LispTextStream("send-value", new StringReader(command.Name), TextWriter.Null);
                        var readSymbol = LispSymbol.ReadSymbolLike(symbolStream, host, allowUnresolvedSymbols: true);
                        if (readSymbol is LispSymbol symbol)
                        {
                            var valueSymbol = symbol.Resolve(host.CurrentPackage);
                            host.RootFrame.SetValue(valueSymbol, obj);
                            return;
                        }

                        context.Fail(command, message: $"Unable to read symbol from name: \"{command.Name}\"");
                    }

                    break;
                default:
                    // TODO: handle other mime types
                    context.Fail(command, message: $"Unable to set value from mime type {command.FormattedValue.MimeType}");
                    break;
            }
        }

        public async Task HandleAsync(SubmitCode command, KernelInvocationContext context)
        {
            var host = await _host.Value;
            using var _ = _stdoutSubject.Subscribe(line =>
            {
                var formatted = new FormattedValue("text/plain", line);
                context.Publish(new StandardOutputValueProduced(command, new[] { formatted }));
            });

            var executionState = host.CreateExecutionState();
            var result = await host.EvalAsync("*REPL*", command.Code, executionState);
            switch (result.Value)
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
            var host = await _host.Value;
            var foundValuePair = host.RootFrame.GetValues().FirstOrDefault(v => v.Item1.LocalName == valueName || v.Item1.Value == valueName);
            if (foundValuePair is { })
            {
                var formatted = mimeType switch
                {
                    "application/json" => foundValuePair.Item2.ToJsonString(),
                    _ => foundValuePair.Item2.ToDisplayString(host.CurrentPackage),
                };

                return new FormattedValue(mimeType, formatted); ;
            }

            return null;
        }

        public static bool TryGetLispObjectFromJson(LispHost host, string json, out LispObject result)
        {
            var token = JToken.Parse(json);
            return TryGetLispObjectFromJToken(host, token, out result);
        }

        private static bool TryGetLispObjectFromJToken(LispHost host, JToken token, out LispObject result)
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
                            if (TryGetLispObjectFromJToken(host, value, out var item))
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
                        result = value ? (LispObject)host.T : host.Nil;
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
                    result = host.Nil;
                    break;
                case JTokenType.Object:
                    {
                        var obj = (JObject)token;
                        var values = new List<LispObject>();
                        foreach (var prop in obj.Properties())
                        {
                            if (TryGetLispObjectFromJToken(host, prop.Value, out var propertyValue))
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
    }
}
