using System;
using System.Diagnostics;
using System.Linq;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Threading.Tasks;
using IxMilia.Lisp.DebugAdapter.Protocol;
using Xunit;
using Xunit.Abstractions;

namespace IxMilia.Lisp.DebugAdapter.Test
{
    public class DebugAdapterTests
    {
        private readonly ITestOutputHelper output;

        public DebugAdapterTests(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Fact]
        public async Task FullTest()
        {
            var filePath = "script-file.lisp";
            var fileContent = @"
(defun add (a b)
    ""Adds two numbers.""
    (+ a b))

(add 2 3)".Trim();

            var nextSeq = 1;
            int Seq() => nextSeq++;
            var messageSender = new Subject<ProtocolMessage>();
            var options = new DebugAdapterOptions(
                path => path == filePath ? Task.FromResult(fileContent) : throw new Exception($"Expected file path of '{filePath}'"),
                message => output.WriteLine(message));
            var da = new DebugAdapter(
                messageSender,
                options);
            da.Start();

            var initializeResponseAwaiter = GetAwaiterForType<InitializeResponse>();
            var initializeEventAwaiter = GetAwaiterForType<InitializedEvent>();
            messageSender.OnNext(new InitializeRequest(Seq(), new InitializeRequestArguments("ixmilia-lisp")));
            await initializeResponseAwaiter;
            await initializeEventAwaiter;

            var launchResponseAwaiter = GetAwaiterForType<LaunchResponse>();
            var breakpointEventAwaiter = GetAwaiterForType<BreakpointEvent>();
            var stoppedEventAwaiter = GetAwaiterForType<StoppedEvent>();
            messageSender.OnNext(new LaunchRequest(Seq(), new LaunchRequestCommandArguments(filePath)));

            var setFunctionBreakpointsResponseAwaiter = GetAwaiterForType<SetFunctionBreakpointsResponse>();
            messageSender.OnNext(new SetFunctionBreakpointsRequest(Seq(), new SetFunctionBreakpointsRequestArguments(new[] { new FunctionBreakpoint("ADD") })));
            await setFunctionBreakpointsResponseAwaiter;

            var configurationDoneResponseAwaiter = GetAwaiterForType<ConfigurationDoneResponse>();
            messageSender.OnNext(new ConfigurationDoneRequest(Seq()));
            await configurationDoneResponseAwaiter;

            var threadsResponseAwaiter = GetAwaiterForType<ThreadsResponse>();
            messageSender.OnNext(new ThreadsRequest(Seq()));
            var threadsResponse = await threadsResponseAwaiter;
            var thread = threadsResponse.Body.Threads.Single();
            Assert.Equal(1, thread.Id);
            Assert.Equal("main", thread.Name);

            await launchResponseAwaiter;
            await breakpointEventAwaiter;
            await stoppedEventAwaiter;

            threadsResponseAwaiter = GetAwaiterForType<ThreadsResponse>();
            messageSender.OnNext(new ThreadsRequest(Seq()));
            await threadsResponseAwaiter;

            var stackTraceResponseAwaiter = GetAwaiterForType<StackTraceResponse>();
            messageSender.OnNext(new StackTraceRequest(Seq(), new StackTraceArguments(thread.Id)));
            await stackTraceResponseAwaiter;

            var scopesResponseAwaiter = GetAwaiterForType<ScopesResponse>();
            messageSender.OnNext(new ScopesRequest(Seq(), new ScopesArguments(1)));
            var scopesResponse = await scopesResponseAwaiter;
            var scope = scopesResponse.Body.Scopes.First();

            var variablesResponseAwaiter = GetAwaiterForType<VariablesResponse>();
            messageSender.OnNext(new VariablesRequest(Seq(), new VariablesRequestArguments(scope.VariablesReference)));
            var variablesResponse = await variablesResponseAwaiter;
            Assert.Equal(2, variablesResponse.Body.Variables.Length);
            Assert.Equal("COMMON-LISP-USER:A", variablesResponse.Body.Variables[0].Name);
            Assert.Equal("2", variablesResponse.Body.Variables[0].Value);
            Assert.Equal("COMMON-LISP-USER:B", variablesResponse.Body.Variables[1].Name);
            Assert.Equal("3", variablesResponse.Body.Variables[1].Value);

            var continueResponseAwaiter = GetAwaiterForType<ContinueResponse>();
            var terminatedEventAwaiter = GetAwaiterForType<TerminatedEvent>();
            messageSender.OnNext(new ContinueRequest(Seq(), new ContinueRequestArguments(thread.Id)));
            await continueResponseAwaiter;
            await terminatedEventAwaiter;

            var disconnectResponseAwaiter = GetAwaiterForType<DisconnectResponse>();
            messageSender.OnNext(new DisconnectRequest(Seq(), new DisconnectRequestArguments(false)));
            await disconnectResponseAwaiter;

            await da.ServerTask;

            //
            Task<T> GetAwaiterForType<T>() where T : ProtocolMessage
            {
                output.WriteLine($"about to wait for {typeof(T).Name}");

                var typeCompletionSource = new TaskCompletionSource<T>();
                var messages = da.OutboundMessages.OfType<T>();
                if (!Debugger.IsAttached)
                {
                    messages = messages.Timeout(TimeSpan.FromSeconds(5));
                }

                IDisposable sub = null;
                sub = messages.Subscribe(t =>
                {
                    sub?.Dispose();
                    output.WriteLine($"returning {typeof(T).Name}");
                    typeCompletionSource.SetResult(t);
                });

                return typeCompletionSource.Task;
            }
        }
    }
}
