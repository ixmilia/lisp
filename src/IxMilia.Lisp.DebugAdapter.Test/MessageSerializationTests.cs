using IxMilia.Lisp.DebugAdapter.Protocol;
using Xunit;

namespace IxMilia.Lisp.DebugAdapter.Test
{
    public class MessageSerializationTests
    {
        [Fact]
        public void BreakpointEvent()
        {
            var obj = new BreakpointEvent(2, new BreakpointEventBody(BreakpointEventReason.New, new Breakpoint(3, true)));
            var json = Serializer.Serialize(obj);
            var expected = @"{""body"":{""reason"":""new"",""breakpoint"":{""id"":3,""verified"":true,""source"":null,""line"":null}},""event"":""breakpoint"",""seq"":2,""type"":""event""}";
            Assert.Equal(expected, json);
        }

        [Fact]
        public void ConfigurationDoneResponse()
        {
            var obj = new ConfigurationDoneResponse(2, 1);
            var json = Serializer.Serialize(obj);
            var expected = @"{""request_seq"":1,""success"":true,""command"":""configurationDone"",""seq"":2,""type"":""response""}";
            Assert.Equal(expected, json);
        }

        [Fact]
        public void ContinueResponse()
        {
            var obj = new ContinueResponse(2, 1);
            var json = Serializer.Serialize(obj);
            var expected = @"{""request_seq"":1,""success"":true,""command"":""continue"",""seq"":2,""type"":""response""}";
            Assert.Equal(expected, json);
        }

        [Fact]
        public void DisconnectResponse()
        {
            var obj = new DisconnectResponse(2, 1);
            var json = Serializer.Serialize(obj);
            var expected = @"{""request_seq"":1,""success"":true,""command"":""disconnect"",""seq"":2,""type"":""response""}";
            Assert.Equal(expected, json);
        }

        [Fact]
        public void EvaluateResponse()
        {
            var obj = new EvaluateResponse(2, 1, new EvaluateResponseBody("some-value", 0));
            var json = Serializer.Serialize(obj);
            var expected = @"{""body"":{""result"":""some-value"",""variablesReference"":0},""request_seq"":1,""success"":true,""command"":""evaluate"",""seq"":2,""type"":""response""}";
            Assert.Equal(expected, json);
        }

        [Fact]
        public void InitializedEvent()
        {
            var obj = new InitializedEvent(1);
            var json = Serializer.Serialize(obj);
            var expected = @"{""event"":""initialized"",""seq"":1,""type"":""event""}";
            Assert.Equal(expected, json);
        }

        [Fact]
        public void InitializeResponse()
        {
            var obj = new InitializeResponse(2, 1);
            var json = Serializer.Serialize(obj);
            var expected = @"{""body"":{""supportsConfigurationDoneRequest"":true,""supportsFunctionBreakpoints"":true,""supportsEvaluateForHovers"":true,""exceptionBreakpointFilters"":[{""filter"":""error"",""label"":""Error breakpoint""}]},""request_seq"":1,""success"":true,""command"":""initialize"",""seq"":2,""type"":""response""}";
            Assert.Equal(expected, json);
        }

        [Fact]
        public void LaunchResponse()
        {
            var obj = new LaunchResponse(2, 1);
            var json = Serializer.Serialize(obj);
            var expected = @"{""request_seq"":1,""success"":true,""command"":""launch"",""seq"":2,""type"":""response""}";
            Assert.Equal(expected, json);
        }

        [Fact]
        public void OutputEvent()
        {
            var obj = new OutputEvent(2, new OutputEventBody(OutputEventCategory.Console, "abcd"));
            var json = Serializer.Serialize(obj);
            var expected = @"{""body"":{""category"":""console"",""output"":""abcd""},""event"":""output"",""seq"":2,""type"":""event""}";
            Assert.Equal(expected, json);
        }

        [Fact]
        public void ProcessEvent()
        {
            var obj = new ProcessEvent(2, new ProcessEventBody("some-name"));
            var json = Serializer.Serialize(obj);
            var expected = @"{""body"":{""name"":""some-name""},""event"":""process"",""seq"":2,""type"":""event""}";
            Assert.Equal(expected, json);
        }

        [Fact]
        public void ScopesResponse()
        {
            var obj = new ScopesResponse(2, 1, new ScopesResponseBody(new[] { new Scope("some-scope", 3, true) }));
            var json = Serializer.Serialize(obj);
            var expected = @"{""body"":{""scopes"":[{""name"":""some-scope"",""variablesReference"":3,""expensive"":true}]},""request_seq"":1,""success"":true,""command"":""scopes"",""seq"":2,""type"":""response""}";
            Assert.Equal(expected, json);
        }

        [Fact]
        public void SetBreakpointsResponse()
        {
            var obj = new SetBreakpointsResponse(2, 1, new[] { new Breakpoint(3, true), new Breakpoint(4, true, source: new Source("some-file"), line: 5) });
            var json = Serializer.Serialize(obj);
            var expected = @"{""breakpoints"":[{""id"":3,""verified"":true,""source"":null,""line"":null},{""id"":4,""verified"":true,""source"":{""path"":""some-file""},""line"":5}],""request_seq"":1,""success"":true,""command"":""setBreakpoints"",""seq"":2,""type"":""response""}";
            Assert.Equal(expected, json);
        }

        [Fact]
        public void SetExceptionBreakpointsResponse()
        {
            var obj = new SetExceptionBreakpointsResponse(2, 1, new[] { new Breakpoint(3, true) });
            var json = Serializer.Serialize(obj);
            var expected = @"{""breakpoints"":[{""id"":3,""verified"":true,""source"":null,""line"":null}],""request_seq"":1,""success"":true,""command"":""setExceptionBreakpoints"",""seq"":2,""type"":""response""}";
            Assert.Equal(expected, json);
        }

        [Fact]
        public void SetFunctionBreakpointsResponse()
        {
            var obj = new SetFunctionBreakpointsResponse(2, 1, new[] { new Breakpoint(3, true) });
            var json = Serializer.Serialize(obj);
            var expected = @"{""breakpoints"":[{""id"":3,""verified"":true,""source"":null,""line"":null}],""request_seq"":1,""success"":true,""command"":""setFunctionBreakpoints"",""seq"":2,""type"":""response""}";
            Assert.Equal(expected, json);
        }

        [Fact]
        public void SourceResponse()
        {
            var obj = new SourceResponse(2, 1, new SourceResponseBody("source-content"));
            var json = Serializer.Serialize(obj);
            var expected = @"{""body"":{""content"":""source-content""},""request_seq"":1,""success"":true,""command"":""source"",""seq"":2,""type"":""response""}";
            Assert.Equal(expected, json);
        }

        [Fact]
        public void StackTraceResponse()
        {
            var obj = new StackTraceResponse(2, 1, new StackTraceResponseBody(new[] { new StackFrame(3, "some-frame", new Source("some-path"), 4, 5) }));
            var json = Serializer.Serialize(obj);
            var expected = @"{""body"":{""stackFrames"":[{""id"":3,""name"":""some-frame"",""source"":{""path"":""some-path""},""line"":4,""column"":5}]},""request_seq"":1,""success"":true,""command"":""stackTrace"",""seq"":2,""type"":""response""}";
            Assert.Equal(expected, json);
        }

        [Fact]
        public void StoppedEvent()
        {
            var obj = new StoppedEvent(2, new StoppedEventBody("some-reason", threadId: 3, hitBreakpointIds: new[] { 4 }));
            var json = Serializer.Serialize(obj);
            var expected = @"{""body"":{""reason"":""some-reason"",""threadId"":3,""hitBreakpointIds"":[4]},""event"":""stopped"",""seq"":2,""type"":""event""}";
            Assert.Equal(expected, json);
        }

        [Fact]
        public void TerminatedEvent()
        {
            var obj = new TerminatedEvent(2);
            var json = Serializer.Serialize(obj);
            var expected = @"{""event"":""terminated"",""seq"":2,""type"":""event""}";
            Assert.Equal(expected, json);
        }

        [Fact]
        public void ThreadsResponse()
        {
            var obj = new ThreadsResponse(2, 1, new ThreadsResponseBody(new[] { new Thread(3, "some-thread") }));
            var json = Serializer.Serialize(obj);
            var expected = @"{""body"":{""threads"":[{""id"":3,""name"":""some-thread""}]},""request_seq"":1,""success"":true,""command"":""threads"",""seq"":2,""type"":""response""}";
            Assert.Equal(expected, json);
        }

        [Fact]
        public void VariablesResponse()
        {
            var obj = new VariablesResponse(2, 1, new VariablesResponseBody(new[] { new Variable("some-name", "some-value", 3) }));
            var json = Serializer.Serialize(obj);
            var expected = @"{""body"":{""variables"":[{""name"":""some-name"",""value"":""some-value"",""variablesReference"":3}]},""request_seq"":1,""success"":true,""command"":""variables"",""seq"":2,""type"":""response""}";
            Assert.Equal(expected, json);
        }
    }
}
