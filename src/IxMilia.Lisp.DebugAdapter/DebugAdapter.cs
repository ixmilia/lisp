﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reactive.Subjects;
using System.Text;
using System.Threading.Tasks;
using IxMilia.Lisp.DebugAdapter.Protocol;

namespace IxMilia.Lisp.DebugAdapter
{
    public class DebugAdapter : IDisposable
    {
        private const int LocalsScopeId = 1;
        private const int GlobalsScopeId = 2;
        private const int MainThreadId = 1;
        private const string MainThreadName = "main";

        private List<IDisposable> _disposables = new List<IDisposable>();

        public DebugAdapterOptions Options { get; }
        public Subject<ProtocolMessage> OutboundMessages { get; private set; }

        private TaskCompletionSource<bool> _serverTaskCompletion = new TaskCompletionSource<bool>();
        private Dictionary<string, Breakpoint> _breakpointsByName = new();
        private Dictionary<int, Breakpoint> _breakpointsById = new();
        private Dictionary<BreakpointLocation, Breakpoint> _breakpointsByLocation = new();
        private LispHost _host;
        private LispExecutionState _executionState;

        public Task ServerTask => _serverTaskCompletion.Task;

        public DebugAdapter(Subject<ProtocolMessage> messageSubject, DebugAdapterOptions options)
        {
            Options = options;
            OutboundMessages = new Subject<ProtocolMessage>();
            _disposables.Add(OutboundMessages);
            _disposables.Add(messageSubject.Subscribe(protocolMessage =>
            {
                LogMessage($"received: {Serializer.Serialize(protocolMessage)}");
                var _ = HandleMessageAsync(protocolMessage).ContinueWith(async task =>
                {
                    await ReportErrorAsync(task.Exception.ToString());
                    if (protocolMessage is Request request)
                    {
                        PushMessage(new ErrorResponse(GetNextSeq(), request.Seq, request.Command, task.Exception.ToString()));
                    }
                }, System.Threading.CancellationToken.None, TaskContinuationOptions.OnlyOnFaulted, TaskScheduler.Default);
            }));
        }

        public static DebugAdapter CreateFromStreams(Stream receivingStream, Stream sendingStream, DebugAdapterOptions options)
        {
            var messageSubject = ReadMessagesFromStream(receivingStream);
            var da = new DebugAdapter(messageSubject, options);
            da._disposables.Add(messageSubject);
            da.OutboundMessages.Subscribe(protocolMessage =>
            {
                var _ = PublishMessageToStreamAsync(protocolMessage, sendingStream);
            });
            return da;
        }

        public void Dispose()
        {
            foreach (var disposable in _disposables)
            {
                disposable.Dispose();
            }
        }

        public void Start()
        {
        }

        private static Subject<ProtocolMessage> ReadMessagesFromStream(Stream receivingStream)
        {
            var subject = new Subject<ProtocolMessage>();
            var _ = Task.Run(async () =>
            {
                var reader = new HeaderDelimitedReader(receivingStream);
                while (true)
                {
                    try
                    {
                        var readResult = await reader.ReadAsync();
                        var rawJson = Encoding.UTF8.GetString(readResult.Body);
                        var protocolMessage = Serializer.Deserialize<ProtocolMessage>(rawJson);
                        subject.OnNext(protocolMessage);
                    }
                    catch (Exception e)
                    {
                        await Console.Error.WriteLineAsync(e.ToString());
                    }
                }
            });

            return subject;
        }

        private static async Task PublishMessageToStreamAsync(ProtocolMessage message, Stream sendingStream)
        {
            var jsonResponse = Serializer.Serialize(message);
            var responseBytes = Encoding.UTF8.GetBytes(jsonResponse);
            var responseHeaderText = $"Content-Length: {responseBytes.Length}\r\n\r\n";
            var responseHeaderBytes = Encoding.ASCII.GetBytes(responseHeaderText);
            var allResponseBytes = responseHeaderBytes.Concat(responseBytes).ToArray();
            await sendingStream.WriteAsync(allResponseBytes, 0, allResponseBytes.Length);
        }

        private async Task ReportErrorAsync(string message)
        {
            await Console.Error.WriteLineAsync(message);
            LogMessage(message);
        }

        private void LogMessage(string message)
        {
            Options.MessageLogger?.Invoke(message);
        }

        private async Task HandleMessageAsync(ProtocolMessage message)
        {
            if (message is Request request)
            {
                await HandleRequestAsync(request);
            }
            else
            {
                throw new NotImplementedException();
            }
        }

        private async Task HandleRequestAsync(Request request)
        {
            Task _t = null;
            switch (request)
            {
                case ConfigurationDoneRequest configurationDone:
                    ConfigurationDone(configurationDone);
                    break;
                case ContinueRequest cont:
                    await ContinueAsync(cont);
                    break;
                case DisconnectRequest disconnect:
                    Disconnect(disconnect);
                    break;
                case EvaluateRequest evaluate:
                    _t = EvaluateAsync(evaluate);
                    break;
                case InitializeRequest initialize:
                    Initialize(initialize);
                    break;
                case LaunchRequest launch:
                    await LaunchAsync(launch);
                    break;
                case ScopesRequest scopes:
                    Scopes(scopes);
                    break;
                case SetBreakpointsRequest setBreakpoints:
                    SetBreakpoints(setBreakpoints);
                    break;
                case SetExceptionBreakpointsRequest setExceptionBreakpoints:
                    SetExceptionBreakpoints(setExceptionBreakpoints);
                    break;
                case SetFunctionBreakpointsRequest setFunctionBreakpoints:
                    SetFunctionBreakpoints(setFunctionBreakpoints);
                    break;
                case SourceRequest source:
                    _t = SourceAsync(source);
                    break;
                case StackTraceRequest stackTrace:
                    StackTrace(stackTrace);
                    break;
                case ThreadsRequest threads:
                    Threads(threads);
                    break;
                case VariablesRequest variables:
                    Variables(variables);
                    break;
                default:
                    throw new NotImplementedException();
            }
        }

        private Breakpoint _errorBreakpoint = new Breakpoint(0, true);

        private BreakReason _breakReason = null;
        private bool _canBreakOnLocation = true;

        private int _nextBreakpointId = 1;
        private int GetNextBreakpointId() => _nextBreakpointId++;

        private int _nextSeq = 1;
        private int GetNextSeq() => _nextSeq++;
        private TaskCompletionSource<bool> _configurationDone = new TaskCompletionSource<bool>();

        private void PushMessage(ProtocolMessage message)
        {
            LogMessage($"sending: {Serializer.Serialize(message)}");
            OutboundMessages.OnNext(message);
        }

        private async Task ContinueEvaluationAsync()
        {
            var result = await _host.EvalContinueAsync(_executionState);

            if (_executionState.IsExecutionComplete)
            {
                PushMessage(new OutputEvent(GetNextSeq(), new OutputEventBody(OutputEventCategory.Console, $"Evaluation finished with: {result.Value}\n")));
                PushMessage(new TerminatedEvent(GetNextSeq()));
            }
            else
            {
                var location = _executionState.StackFrame.SourceLocation;
                switch (_breakReason)
                {
                    case LineBreakReason lineBreakReason:
                        _canBreakOnLocation = false;
                        UpdateBreakpointSourceLocation(lineBreakReason.Breakpoint, lineBreakReason.Location);
                        PushMessage(new StoppedEvent(GetNextSeq(), new StoppedEventBody("breakpoint", threadId: MainThreadId, hitBreakpointIds: new[] { lineBreakReason.Breakpoint.Id })));
                        break;
                    case ErrorBreakReason errorBreakReason:
                        UpdateBreakpointSourceLocation(_errorBreakpoint, errorBreakReason.Error.SourceLocation);
                        PushMessage(new StoppedEvent(GetNextSeq(), new StoppedEventBody("exception", description: "Paused on error", text: errorBreakReason.Error.Message, threadId: MainThreadId)));
                        break;
                    case FunctionBreakReason functionBreakReason:
                        UpdateBreakpointSourceLocation(functionBreakReason.Breakpoint, location);
                        PushMessage(new StoppedEvent(GetNextSeq(), new StoppedEventBody("function breakpoint", threadId: MainThreadId, hitBreakpointIds: new[] { functionBreakReason.Breakpoint.Id })));
                        break;
                    case null:
                        // finished with some result, error or not
                        PushMessage(new OutputEvent(GetNextSeq(), new OutputEventBody(OutputEventCategory.Console, $"Evaluation finished with: {result.Value}\n")));
                        PushMessage(new TerminatedEvent(GetNextSeq()));
                        break;
                }

                _breakReason = null;
            }
        }

        private void UpdateBreakpointSourceLocation(Breakpoint breakpoint, LispSourceLocation? sourceLocation)
        {
            if (sourceLocation.HasValue &&
                (breakpoint.Line != sourceLocation.Value.Start.Line ||
                 breakpoint.Source?.Path != sourceLocation.Value.FilePath))
            {
                breakpoint.Line = sourceLocation.Value.Start.Line;
                breakpoint.Source = new Source(sourceLocation.Value.FilePath);
                PushMessage(new BreakpointEvent(GetNextSeq(), new BreakpointEventBody(BreakpointEventReason.Changed, breakpoint)));
            }
        }

        private void ConfigurationDone(ConfigurationDoneRequest configurationDone)
        {
            PushMessage(new ConfigurationDoneResponse(GetNextSeq(), configurationDone.Seq));
            _configurationDone.SetResult(true);
        }

        private async Task ContinueAsync(ContinueRequest cont)
        {
            PushMessage(new ContinueResponse(GetNextSeq(), cont.Seq));
            await ContinueEvaluationAsync();
        }

        private void Disconnect(DisconnectRequest request)
        {
            PushMessage(new DisconnectResponse(GetNextSeq(), request.Seq));
            _serverTaskCompletion.SetResult(true);
        }

        private async Task EvaluateAsync(EvaluateRequest evaluate)
        {
            LispStackFrame evaluationFrame = _host.RootFrame;
            if (evaluate.Arguments.FrameId.HasValue)
            {
                evaluationFrame = _executionState.StackFrame;
                for (var frameId = evaluate.Arguments.FrameId.Value; frameId > 0; frameId--)
                {
                    evaluationFrame = evaluationFrame.Parent;
                }
            }

            var executionState = LispExecutionState.CreateExecutionState(evaluationFrame, allowHalting: false);
            var evalResult = await _host.EvalAsync("<eval>", evaluate.Arguments.Expression, executionState);
            PushMessage(new EvaluateResponse(GetNextSeq(), evaluate.Seq, new EvaluateResponseBody(evalResult.Value?.ToDisplayString(_host.CurrentPackage) ?? "<unknown>", 0)));
        }

        private void Initialize(InitializeRequest initialize)
        {
            PushMessage(new OutputEvent(GetNextSeq(), new OutputEventBody(OutputEventCategory.Console, $"Initializing debugger in process {System.Diagnostics.Process.GetCurrentProcess().Id}\n")));
            PushMessage(new InitializeResponse(GetNextSeq(), initialize.Seq));
            PushMessage(new InitializedEvent(GetNextSeq()));
        }

        private async Task LaunchAsync(LaunchRequest launch)
        {
            var fileContent = await Options.ResolveFileContents(launch.Arguments.Program);
            var output = new ListeningTextWriter(line => PushMessage(new OutputEvent(GetNextSeq(), new OutputEventBody(OutputEventCategory.Stdout, line))));
            var configuration = new LispHostConfiguration(output: output);
            _host = await LispHost.CreateAsync(configuration);
            _executionState = _host.CreateExecutionState();
            _host.RootFrame.ErrorOccurred += RootFrame_ErrorOccurred;
            _host.RootFrame.EvaluatingExpression += RootFrame_EvaluatingExpression;
            _host.RootFrame.FunctionEntered += RootFrame_FunctionEntered;
#pragma warning disable VSTHRD003 // Avoid awaiting foreign Tasks
            await _configurationDone.Task;
#pragma warning restore VSTHRD003 // Avoid awaiting foreign Tasks
            _executionState.InsertCodeOperations(launch.Arguments.Program, fileContent);
            PushMessage(new LaunchResponse(GetNextSeq(), launch.Seq));
            await ContinueEvaluationAsync();
        }

        private void RootFrame_ErrorOccurred(object sender, LispErrorOccuredEventArgs e)
        {
            _breakReason = new ErrorBreakReason(e.Error);
        }

        private void RootFrame_EvaluatingExpression(object sender, LispEvaluatingExpressionEventArgs e)
        {
            if (!_canBreakOnLocation)
            {
                _canBreakOnLocation = true;
                return;
            }

            var breakpointLine = e.Expression.GetBreakpointLine();
            if (e.Expression.SourceLocation.HasValue &&
                breakpointLine.HasValue)
            {
                var breakpointLocation = new BreakpointLocation(e.Expression.SourceLocation.Value.FilePath, breakpointLine.Value);
                if (_breakpointsByLocation.TryGetValue(breakpointLocation, out var breakpoint))
                {
                    e.HaltExecution = true;
                    _breakReason = new LineBreakReason(breakpoint, e.Expression.SourceLocation.Value);
                }
            }
        }

        private void RootFrame_FunctionEntered(object sender, LispFunctionEnteredEventArgs e)
        {
            Breakpoint bp;
            if (_breakpointsByName.TryGetValue(e.Frame.FunctionSymbol.LocalName, out bp) ||
                _breakpointsByName.TryGetValue(e.Frame.FunctionSymbol.Value, out bp))
            {
                e.HaltExecution = true;
                _breakReason = new FunctionBreakReason(bp);
            }
        }

        private void Scopes(ScopesRequest scopes)
        {
            PushMessage(new ScopesResponse(GetNextSeq(), scopes.Seq, new ScopesResponseBody(new[]
            {
                new Scope("Locals", LocalsScopeId, false),
                new Scope("Globals", GlobalsScopeId, true)
            })));
        }

        private void SetBreakpoints(SetBreakpointsRequest setBreakpoints)
        {
            var breakpoints = new List<Breakpoint>();
            foreach (var breakpoint in setBreakpoints.Arguments.Breakpoints)
            {
                var breakpointLocation = new BreakpointLocation(setBreakpoints.Arguments.Source.Path, breakpoint.Line);
                var newBreakpoint = new Breakpoint(GetNextBreakpointId(), true, setBreakpoints.Arguments.Source, breakpoint.Line);
                _breakpointsById[newBreakpoint.Id] = newBreakpoint;
                _breakpointsByLocation[breakpointLocation] = newBreakpoint;
                breakpoints.Add(newBreakpoint);
            }

            PushMessage(new SetBreakpointsResponse(GetNextSeq(), setBreakpoints.Seq, breakpoints.ToArray()));
        }

        private void SetExceptionBreakpoints(SetExceptionBreakpointsRequest setExceptionBreakpoints)
        {
            var breakpoints = new Breakpoint[] { _errorBreakpoint };
            PushMessage(new SetExceptionBreakpointsResponse(GetNextSeq(), setExceptionBreakpoints.Seq, breakpoints));
        }

        private void SetFunctionBreakpoints(SetFunctionBreakpointsRequest setFunctionBreakpoints)
        {
            var resolvedBreakpoints = new List<Breakpoint>();
            foreach (var breakpoint in _breakpointsByName.Values)
            {
                _breakpointsById.Remove(breakpoint.Id);
            }

            _breakpointsByName.Clear();
            foreach (var fb in setFunctionBreakpoints.Arguments.Breakpoints)
            {
                var bp = new Breakpoint(GetNextBreakpointId(), true);
                resolvedBreakpoints.Add(bp);
                _breakpointsByName.Add(fb.Name, bp);
                _breakpointsById.Add(bp.Id, bp);
            }

            PushMessage(new SetFunctionBreakpointsResponse(GetNextSeq(), setFunctionBreakpoints.Seq, resolvedBreakpoints.ToArray()));
        }

        private async Task SourceAsync(SourceRequest source)
        {
            SourceResponseBody body = null;
            if (source.Arguments.Source.Path == "init.lisp")
            {
                var content = await LispHost.GetInitScriptContents();
                body = new SourceResponseBody(content);
            }

            PushMessage(new SourceResponse(GetNextSeq(), source.Seq, body));
        }

        private void StackTrace(StackTraceRequest stackTrace)
        {
            var stackFrames = new List<StackFrame>();
            var currentFrame = _executionState.StackFrame;
            while (currentFrame != null)
            {
                var location = currentFrame.SourceLocation ?? new LispSourceLocation();
                stackFrames.Add(new StackFrame(stackFrames.Count, currentFrame.FunctionSymbol.Value, new Source(location.FilePath), location.Start.Line, location.Start.Column));
                currentFrame = currentFrame.Parent;
            }
            PushMessage(new StackTraceResponse(GetNextSeq(), stackTrace.Seq, new StackTraceResponseBody(stackFrames.ToArray())));
        }

        private void Threads(ThreadsRequest threads)
        {
            PushMessage(new ThreadsResponse(GetNextSeq(), threads.Seq, new ThreadsResponseBody(new[] { new Thread(MainThreadId, MainThreadName) })));
        }

        private void Variables(VariablesRequest variables)
        {
            var variablesFrame = variables.Arguments.VariablesReference == LocalsScopeId
                ? _executionState.StackFrame
                : _executionState.StackFrame.Root;
            var variablesArray = variablesFrame.GetValues().Select(pair => new Variable(pair.Item1.ToDisplayString(_host.CurrentPackage), pair.Item2.ToDisplayString(_host.CurrentPackage), 0)).ToArray();
            PushMessage(new VariablesResponse(GetNextSeq(), variables.Seq, new VariablesResponseBody(variablesArray)));
        }

        private struct BreakpointLocation
        {
            public string FilePath { get; }
            public int Line { get; }

            public BreakpointLocation(string filePath, int line)
            {
                FilePath = NormalizePath(filePath);
                Line = line;
            }

            public override int GetHashCode()
            {
                int hashCode = -514213941;
                hashCode = hashCode * -1521134295 + EqualityComparer<string>.Default.GetHashCode(FilePath);
                hashCode = hashCode * -1521134295 + Line.GetHashCode();
                return hashCode;
            }

            public override string ToString() => $"{FilePath}:{Line}";

            private static string NormalizePath(string path)
            {
                var fullPath = Path.GetFullPath(path);
                if (Environment.OSVersion.Platform == PlatformID.Win32NT)
                {
                    fullPath = fullPath.ToLowerInvariant();
                }

                return fullPath;
            }
        }
    }
}
