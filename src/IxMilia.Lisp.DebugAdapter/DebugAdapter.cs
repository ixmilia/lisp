using System;
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
                var _ = Task.Run(async () =>
                {
                    try
                    {
                        LogMessage($"received: {Serializer.Serialize(protocolMessage)}");
                        await HandleMessageAsync(protocolMessage);
                    }
                    catch (Exception e)
                    {
                        await ReportErrorAsync(e.ToString());
                        if (protocolMessage is Request request)
                        {
                            PushMessage(new ErrorResponse(GetNextSeq(), request.Seq, request.Command, e.ToString()));
                        }
                    }
                });
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

        private Breakpoint _lastHitBreakpoint = null;
        private int _nextBreakpointId = 1;
        private int GetNextBreakpointId() => _nextBreakpointId++;

        private int _nextSeq = 1;
        private int GetNextSeq() => _nextSeq++;
        private TaskCompletionSource<bool> _configurationDone = new TaskCompletionSource<bool>();

        private void PushMessage(ProtocolMessage message)
        {
            OutboundMessages.OnNext(message);
            LogMessage($"sending: {Serializer.Serialize(message)}");
        }

        private async Task ContinueEvaluationAsync()
        {
            var result = await _host.EvalContinueAsync(_executionState);
            _executionState = result.ExecutionState;

            if (_executionState.IsExecutionComplete)
            {
                PushMessage(new TerminatedEvent(GetNextSeq()));
            }
            else
            {
                var location = _executionState.StackFrame.SourceLocation;
                if (_lastHitBreakpoint is { })
                {
                    if (location.HasValue)
                    {
                        _lastHitBreakpoint.Line = location.Value.Start.Line;
                        _lastHitBreakpoint.Source = new Source(location.Value.FilePath);
                        PushMessage(new BreakpointEvent(GetNextSeq(), new BreakpointEventBody(BreakpointEventReason.Changed, _lastHitBreakpoint)));
                    }

                    PushMessage(new StoppedEvent(GetNextSeq(), new StoppedEventBody("function breakpoint", MainThreadId, new[] { _lastHitBreakpoint.Id })));
                }
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

        private void Initialize(InitializeRequest initialize)
        {
            PushMessage(new InitializeResponse(GetNextSeq(), initialize.Seq));
            PushMessage(new InitializedEvent(GetNextSeq()));
        }

        private async Task LaunchAsync(LaunchRequest launch)
        {
            var fileContent = await Options.ResolveFileContents(launch.Arguments.Program);
            _host = await LispHost.CreateAsync(filePath: launch.Arguments.Program);
            _executionState = _host.CreateExecutionState(fileContent);
            _host.RootFrame.FunctionEntered += RootFrame_FunctionEntered;
            await _configurationDone.Task;
            PushMessage(new LaunchResponse(GetNextSeq(), launch.Seq));
            await ContinueEvaluationAsync();
        }

        private void RootFrame_FunctionEntered(object sender, LispFunctionEnteredEventArgs e)
        {
            Breakpoint bp;
            if (_breakpointsByName.TryGetValue(e.Frame.FunctionSymbol.LocalName, out bp) ||
                _breakpointsByName.TryGetValue(e.Frame.FunctionSymbol.Value, out bp))
            {
                e.HaltExecution = true;
                _lastHitBreakpoint = bp;
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
            var breakpoints = new Breakpoint[] { };
            PushMessage(new SetBreakpointsResponse(GetNextSeq(), setBreakpoints.Seq, breakpoints));
        }

        private void SetExceptionBreakpoints(SetExceptionBreakpointsRequest setExceptionBreakpoints)
        {
            var breakpoints = new Breakpoint[] { };
            PushMessage(new SetExceptionBreakpointsResponse(GetNextSeq(), setExceptionBreakpoints.Seq, breakpoints));
        }

        private void SetFunctionBreakpoints(SetFunctionBreakpointsRequest setFunctionBreakpoints)
        {
            var resolvedBreakpoints = new List<Breakpoint>();
            _breakpointsByName.Clear();
            _breakpointsById.Clear();
            foreach (var fb in setFunctionBreakpoints.Arguments.Breakpoints)
            {
                var bp = new Breakpoint(GetNextBreakpointId(), true);
                resolvedBreakpoints.Add(bp);
                _breakpointsByName.Add(fb.Name, bp);
                _breakpointsById.Add(bp.Id, bp);
            }

            PushMessage(new SetFunctionBreakpointsResponse(GetNextSeq(), setFunctionBreakpoints.Seq, resolvedBreakpoints.ToArray()));
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
            PushMessage(new ThreadsResponse(GetNextSeq(), threads.Seq, new ThreadsResponseBody(new[] { new Thread(MainThreadId, MainThreadName) } )));
        }

        private void Variables(VariablesRequest variables)
        {
            var variablesFrame = variables.Arguments.VariablesReference == LocalsScopeId
                ? _executionState.StackFrame
                : _executionState.StackFrame.Root;
            var variablesArray = variablesFrame.GetValues().Select(pair => new Variable(pair.Item1.Value, pair.Item2.ToString(), 0)).ToArray();
            PushMessage(new VariablesResponse(GetNextSeq(), variables.Seq, new VariablesResponseBody(variablesArray)));
        }
    }
}
