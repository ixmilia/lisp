using System.Linq;
using IxMilia.Lisp.DebugAdapter.Protocol;
using Xunit;

namespace IxMilia.Lisp.DebugAdapter.Test
{
    public class MessageDeserializationTests
    {
        [Fact]
        public void ConfigurationDoneRequest()
        {
            var obj = Serializer.Deserialize<ConfigurationDoneRequest>(@"{""command"":""configurationDone"",""type"":""request"",""seq"":1}");
            Assert.Equal(1, obj.Seq);
        }

        [Fact]
        public void ContinueRequest()
        {
            var obj = Serializer.Deserialize<ContinueRequest>(@"{""command"":""continue"",""arguments"":{""threadId"":2},""type"":""request"",""seq"":1}");
            Assert.Equal(1, obj.Seq);
            Assert.Equal(2, obj.Arguments.ThreadId);
        }

        [Fact]
        public void DisconnectRequest()
        {
            var obj = Serializer.Deserialize<DisconnectRequest>(@"{""command"":""disconnect"",""arguments"":{""restart"":true},""type"":""request"",""seq"":1}");
            Assert.Equal(1, obj.Seq);
            Assert.True(obj.Arguments.Restart);
        }

        [Fact]
        public void EvaluateRequest()
        {
            var obj = Serializer.Deserialize<EvaluateRequest>(@"{""command"":""evaluate"",""arguments"":{""expression"":""x"",""frameId"":0,""context"":""hover""},""type"":""request"",""seq"":1}");
            Assert.Equal(1, obj.Seq);
            Assert.Equal("x", obj.Arguments.Expression);
        }

        [Fact]
        public void InitializeRequest()
        {
            var obj = Serializer.Deserialize<InitializeRequest>(@"{""command"":""initialize"",""arguments"":{""adapterId"":""some-adapter-id""},""type"":""request"",""seq"":1}");
            Assert.Equal(1, obj.Seq);
            Assert.Equal("some-adapter-id", obj.Arguments.AdapterId);
        }

        [Fact]
        public void LaunchRequest()
        {
            var obj = Serializer.Deserialize<LaunchRequest>(@"{""command"":""launch"",""arguments"":{""program"":""some-file""},""type"":""request"",""seq"":1}");
            Assert.Equal(1, obj.Seq);
            Assert.Equal("some-file", obj.Arguments.Program);
        }

        [Fact]
        public void ScopesRequest()
        {
            var obj = Serializer.Deserialize<ScopesRequest>(@"{""command"":""scopes"",""arguments"":{""frameId"":2},""type"":""request"",""seq"":1}");
            Assert.Equal(1, obj.Seq);
            Assert.Equal(2, obj.Arguments.FrameId);
        }

        [Fact]
        public void SetBreakpointsRequest()
        {
            var obj = Serializer.Deserialize<SetBreakpointsRequest>(@"{""command"":""setBreakpoints"",""arguments"":{""source"":{""path"":""some-file""},""breakpoints"":[{""line"":3}]},""type"":""request"",""seq"":1}");
            Assert.Equal(1, obj.Seq);
            Assert.Equal("some-file", obj.Arguments.Source.Path);
            Assert.Equal(3, obj.Arguments.Breakpoints.Single().Line);
        }

        [Fact]
        public void SetExceptionBreakpointsRequest()
        {
            var obj = Serializer.Deserialize<SetExceptionBreakpointsRequest>(@"{""command"":""setExceptionBreakpoints"",""arguments"":{""filters"":[""some-filter""]},""type"":""request"",""seq"":1}");
            Assert.Equal(1, obj.Seq);
            Assert.Equal("some-filter", obj.Arguments.Filters.Single());
        }

        [Fact]
        public void SetFunctionBreakpointsRequest()
        {
            var obj = Serializer.Deserialize<SetFunctionBreakpointsRequest>(@"{""command"":""setFunctionBreakpoints"",""arguments"":{""breakpoints"":[{""name"":""some-function-breakpoint""}]},""type"":""request"",""seq"":1}");
            Assert.Equal(1, obj.Seq);
            Assert.Equal("some-function-breakpoint", obj.Arguments.Breakpoints.Single().Name);
        }

        [Fact]
        public void SourceRequest()
        {
            var obj = Serializer.Deserialize<SourceRequest>(@"{""command"":""source"",""arguments"":{""source"":{""path"":""some-path""},""sourceReference"":2},""type"":""request"",""seq"":1}");
            Assert.Equal(1, obj.Seq);
            Assert.Equal("some-path", obj.Arguments.Source.Path);
            Assert.Equal(2, obj.Arguments.SourceReference);
        }

        [Fact]
        public void StackTraceRequest()
        {
            var obj = Serializer.Deserialize<StackTraceRequest>(@"{""command"":""stackTrace"",""arguments"":{""threadId"":2},""type"":""request"",""seq"":1}");
            Assert.Equal(1, obj.Seq);
            Assert.Equal(2, obj.Arguments.ThreadId);
        }

        [Fact]
        public void ThreadsRequest()
        {
            var obj = Serializer.Deserialize<ThreadsRequest>(@"{""command"":""threads"",""type"":""request"",""seq"":1}");
            Assert.Equal(1, obj.Seq);
        }

        [Fact]
        public void VariablesRequest()
        {
            var obj = Serializer.Deserialize<VariablesRequest>(@"{""command"":""variables"",""arguments"":{""variablesReference"":2},""type"":""request"",""seq"":1}");
            Assert.Equal(1, obj.Seq);
            Assert.Equal(2, obj.Arguments.VariablesReference);
        }
    }
}
