using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace IxMilia.Lisp
{
    public class LispRepl
    {
        private string _unconsumedInput;
        private string _location;
        private TextReader _input;

        public TextWriter Output { get; }

        private TextWriter _traceWriter;

        public LispHost Host { get; private set; }

        public HashSet<string> TracedFunctions { get; } = new HashSet<string>();

        private LispRepl(string location = null, TextReader input = null, TextWriter output = null, TextWriter traceWriter = null, bool useTailCalls = false)
        {
            _location = location;
            _input = input ?? TextReader.Null;
            Output = output ?? TextWriter.Null;
            _traceWriter = traceWriter ?? TextWriter.Null;
        }

        public static async Task<LispRepl> CreateAsync(string location = null, TextReader input = null, TextWriter output = null, TextWriter traceWriter = null, bool useTailCalls = false, CancellationToken cancellationToken = default)
        {
            var repl = new LispRepl(location, input, output, traceWriter, useTailCalls);
            var configuration = new LispHostConfiguration(
                useTailCalls: useTailCalls,
                input: repl._input,
                output: repl.Output);
            repl.Host = await LispHost.CreateAsync(
                configuration: configuration,
                cancellationToken: cancellationToken);
            repl.Host.RootFrame.FunctionEntered += repl.FunctionEntered;
            repl.Host.RootFrame.FunctionReturned += repl.FunctionReturned;

            var replContext = new LispReplDefaultContext(repl);
            repl.Host.AddContextObject(replContext);
            repl.Host.SetReaderFunction(LispReaderType.Interpreted);
            return repl;
        }

        private void FunctionEntered(object sender, LispFunctionEnteredEventArgs e)
        {
            if (TracedFunctions.Contains(e.Frame.FunctionSymbol.Value))
            {
                WriteTraceLine($"{CreateTracePrefix(e.Frame, isFunctionEnter: true)}: ({e.Frame.FunctionSymbol.LocalName}{(e.FunctionArguments.Length > 0 ? " " + string.Join<LispObject>(" ", e.FunctionArguments) : "")})");
            }
        }

        private void FunctionReturned(object sender, LispFunctionReturnedEventArgs e)
        {
            if (TracedFunctions.Contains(e.Frame.FunctionSymbol.Value))
            {
                WriteTraceLine($"{CreateTracePrefix(e.Frame, isFunctionEnter: false)}: returned {e.ReturnValue}");
            }
        }

        private string CreateTracePrefix(LispStackFrame frame, bool isFunctionEnter)
        {
            var depth = Math.Max(0, frame.Depth - 1);
            var indent = new string(' ', depth);
            return $"{indent}{depth}";
        }

        private void WriteTraceLine(string value)
        {
            _traceWriter.WriteLine(value);
            _traceWriter.Flush();
        }

        public LispObject GetValue(string name) => Host.GetValue(name);

        public TObject GetValue<TObject>(string name) where TObject : LispObject => Host.GetValue<TObject>(name);

        public async Task<LispReplResult> EvalAsync(string code, CancellationToken cancellationToken = default)
        {
            var fullCode = string.Concat(string.IsNullOrEmpty(_unconsumedInput) ? null : string.Concat(_unconsumedInput, Environment.NewLine), code);
            var seenChars = 0;
            using var fullCodeReader = new StringReader(fullCode);
            using var mirroringReader = new MirroringTextReader(fullCodeReader, _c => seenChars++);
            var executionState = LispExecutionState.CreateExecutionState(Host.RootFrame, true);
            var stream = new LispTextStream(_location, mirroringReader, TextWriter.Null);
            var readObject = LispList.FromItems(
            new LispResolvedSymbol("COMMON-LISP", "READ", true),
                stream,
                Host.T,
                Host.Nil,
                Host.T);

            var expressionDepth = 0;
            bool IsListReader(LispResolvedSymbol symbol) => symbol.PackageName == "COMMON-LISP" && symbol.LocalName == "LIST-READER";
            void EnterList(object sender, LispFunctionEnteredEventArgs e)
            {
                if (IsListReader(e.Frame.FunctionSymbol))
                {
                    expressionDepth++;
                }
            }

            void ExitList(object sender, LispFunctionReturnedEventArgs e)
            {
                if (IsListReader(e.Frame.FunctionSymbol))
                {
                    expressionDepth--;
                }
            }

            LispObject lastResult = null;
            while (true)
            {
                Host.RootFrame.FunctionEntered += EnterList;
                Host.RootFrame.FunctionReturned += ExitList;

                var readEvalResult = await Host.EvalAsync(readObject, executionState, cancellationToken: cancellationToken);

                Host.RootFrame.FunctionReturned -= ExitList;
                Host.RootFrame.FunctionEntered -= EnterList;

                if (readEvalResult.State == LispEvaluationState.FatalHalt)
                {
                    _unconsumedInput = fullCode;
                    return new LispReplResult(lastResult, executionState, expressionDepth);
                }

                _unconsumedInput = fullCode.Substring(seenChars);
                var evalEvalResult = await Host.EvalAsync(readEvalResult.Value, executionState, createDribbleInstructions: true, cancellationToken: cancellationToken);
                lastResult = evalEvalResult.Value;
            }
        }

        internal Task EvalAsync(LispExecutionState executionState, CancellationToken cancellationToken)
        {
            return Host.EvalContinueAsync(executionState, cancellationToken);
        }

        internal LispObject Trace(LispObject[] args)
        {
            if (args.Length == 0)
            {
                return LispList.FromEnumerable(TracedFunctions.Select(f => LispSymbol.CreateFromString(f)));
            }
            else
            {
                var addedFunctions = new HashSet<string>();
                var hasInvalidArguments = false;
                foreach (var arg in args)
                {
                    if (arg is LispSymbol symbol)
                    {
                        var resolvedSymbol = symbol.Resolve(Host.CurrentPackage);
                        TracedFunctions.Add(resolvedSymbol.Value);
                        addedFunctions.Add(resolvedSymbol.Value);
                    }
                    else
                    {
                        hasInvalidArguments = true;
                    }
                }

                if (hasInvalidArguments)
                {
                    return new LispError("Expected only symbols");
                }
                else
                {
                    return LispList.FromEnumerable(addedFunctions.Select(f => LispSymbol.CreateFromString(f)));
                }
            }
        }

        internal LispObject Untrace(LispObject[] args)
        {
            if (args.Length == 0)
            {
                var result = LispList.FromEnumerable(TracedFunctions.Select(f => LispSymbol.CreateFromString(f)));
                TracedFunctions.Clear();
                return result;
            }
            else
            {
                var removedFunctions = new HashSet<string>();
                var hasInvalidArguments = false;
                foreach (var arg in args)
                {
                    if (arg is LispSymbol symbol)
                    {
                        var resolvedSymbol = symbol.Resolve(Host.CurrentPackage);
                        if (TracedFunctions.Remove(resolvedSymbol.Value))
                        {
                            removedFunctions.Add(resolvedSymbol.Value);
                        }
                    }
                    else
                    {
                        hasInvalidArguments = true;
                    }
                }

                if (hasInvalidArguments)
                {
                    return new LispError("Expected only symbols");
                }
                else
                {
                    return LispList.FromEnumerable(removedFunctions.Select(f => LispSymbol.CreateFromString(f)));
                }
            }
        }
    }
}
