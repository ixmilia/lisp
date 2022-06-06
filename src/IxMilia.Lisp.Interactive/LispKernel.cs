using System;
using System.IO;
using System.Linq;
using System.Reactive.Subjects;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.DotNet.Interactive;
using Microsoft.DotNet.Interactive.Commands;
using Microsoft.DotNet.Interactive.Events;

namespace IxMilia.Lisp.Interactive
{
    public class LispKernel :
        Kernel,
        IKernelCommandHandler<RequestCompletions>,
        IKernelCommandHandler<RequestHoverText>,
        IKernelCommandHandler<SubmitCode>
    {
        private Lazy<Task<LispRepl>> _repl;

        public LispKernel()
            : base("lisp")
        {
            _repl = new Lazy<Task<LispRepl>>(async () =>
            {
                var repl = await LispRepl.CreateAsync(location: "*REPL*");
                return repl;
            });
        }

        public async Task HandleAsync(RequestCompletions command, KernelInvocationContext context)
        {
            var repl = await _repl.Value;
            var parseResult = await repl.ParseUntilSourceLocationAsync(command.Code, new LispSourcePosition(command.LinePosition.Line + 1, command.LinePosition.Character + 1));
            if (parseResult.Object != null &&
                !(parseResult.Object is LispString))
            {
                var completionItems = parseResult.VisibleValues.Values.Select(
                    v => new CompletionItem(
                        displayText: v.Symbol.ToDisplayString(repl.Host.CurrentPackage),
                        kind: "",
                        documentation: v.Value is LispFunction f ? f.Documentation : null));
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

        public async Task HandleAsync(SubmitCode command, KernelInvocationContext context)
        {
            var repl = await _repl.Value;
            var writer = new ListeningTextWriter();
            using var subscription = writer.LineWritten.Subscribe(line =>
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
                    var formatted = new FormattedValue("text/plain", obj.ToString()); // TODO: return strings, ints, etc.
                    context.Publish(new ReturnValueProduced(obj, command, new[] { formatted }));
                    context.Publish(new DiagnosticsProduced(new Microsoft.DotNet.Interactive.Diagnostic[0], command));
                    break;
            }
        }

        private class ListeningTextWriter : TextWriter
        {
            private StringBuilder _sb = new StringBuilder();

            public Subject<string> LineWritten { get; } = new Subject<string>();
            public override Encoding Encoding => Encoding.UTF8;

            public override void Write(char value)
            {
                _sb.Append(value);
                if (value == '\n')
                {
                    Flush();
                }
            }

            public override void Flush()
            {
                LineWritten.OnNext(_sb.ToString());
                _sb.Clear();
            }
        }
    }
}
