using System.IO;
using System.Threading;
using System.Threading.Tasks;

namespace IxMilia.Lisp.Repl
{
    public class ReplConsole
    {
        private const string DebugContinueCommand = "continue";

        public string Location { get; }
        public TextReader Input { get; }
        public TextWriter Output { get; }
        public TextWriter Error { get; }

        private LispRepl _repl;

        public ReplConsole(string location, TextReader input, TextWriter output, TextWriter error)
        {
            Location = location;
            Input = input;
            Output = output;
            Error = error;
        }

        public async Task RunAsync(CancellationToken cancellationToken = default)
        {
            _repl = await LispRepl.CreateAsync(location: Location, input: Input, output: Output, traceWriter: Output);
            _repl.Host.RootFrame.EvaluationHalted += (s, e) =>
            {
                if (e.EvaluationState != LispEvaluationState.FatalHalt)
                {
                    Output.WriteLine($"Non-fatal break.  Type '{DebugContinueCommand}' to resume evaluation.");
                    RunInHaltedStateAsync(cancellationToken).GetAwaiter().GetResult();
                }
            };

            PrintPrompt(0);
            string line;
            while ((line = Input.ReadLine()) != "#quit")
            {
                var result = await EvalAndPrintAsync(line, cancellationToken: cancellationToken);
                while (!result.ExecutionState.IsExecutionComplete && !(result.LastResult is LispError))
                {
                    await _repl.EvalAsync(result.ExecutionState, cancellationToken);
                }

                PrintPrompt(result.ExpressionDepth);
            }
        }

        private async Task<LispReplResult> EvalAndPrintAsync(string line, CancellationToken cancellationToken = default)
        {
            var result = await _repl.EvalAsync(line, cancellationToken);
            if (result.LastResult != null)
            {
                Output.WriteLine(result.LastResult.ToDisplayString(_repl.Host.CurrentPackage));
            }

            return result;
        }

        private void PrintPrompt(int depth)
        {
            for (int i = 0; i < depth; i++)
            {
                Output.Write("(");
            }

            Output.Write("_> ");
        }

        private async Task RunInHaltedStateAsync(CancellationToken cancellationToken)
        {
            var inDebug = true;
            while (inDebug)
            {
                PrintDebugPrompt();
                var line = Input.ReadLine();
                switch (line)
                {
                    case DebugContinueCommand:
                        inDebug = false;
                        break;
                    default:
                        await EvalAndPrintAsync(line, cancellationToken: cancellationToken);
                        break;
                }
            }
        }

        private void PrintDebugPrompt()
        {
            Output.Write("DEBUG:> ");
        }
    }
}
