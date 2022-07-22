using System;
using System.Collections.Generic;
using System.Text;

namespace IxMilia.Lisp.DebugAdapter.Protocol
{
    public class InitializeRequest : Request
    {
        internal const string CommandName = "initialize";

        public InitializeRequestArguments Arguments { get; set; }

        public InitializeRequest(int seq, InitializeRequestArguments arguments)
            : base(seq, CommandName)
        {
            Arguments = arguments;
        }
    }

    public class InitializeRequestArguments
    {
        public string AdapterId { get; set; }

        public InitializeRequestArguments(string adapterId)
        {
            AdapterId = adapterId;
        }
    }
}
