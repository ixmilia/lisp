using System.IO;

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

        public void Run()
        {
            _repl = new LispRepl(location: Location, input: Input, output: Output, traceWriter: Output);
            _repl.Host.RootFrame.EvaluationHalted += (s, e) =>
            {
                if (e.EvaluationState != LispEvaluationState.FatalHalt)
                {
                    Output.WriteLine($"Non-fatal break.  Type '{DebugContinueCommand}' to resume evaluation.");
                    RunInHaltedState();
                }
            };

            PrintPrompt(0);
            string line;
            while ((line = Input.ReadLine()) != "#quit")
            {
                var result = EvalAndPrint(line);
                while (!result.ExecutionState.IsExecutionComplete && !(result.LastResult is LispError))
                {
                    _repl.Eval(result.ExecutionState);
                }

                PrintPrompt(result.ExpressionDepth);
            }
        }

        private LispReplResult EvalAndPrint(string line, bool consumeIncompleteInput = true)
        {
            var result = _repl.Eval(line, consumeIncompleteInput);
            if (result.LastResult != null)
            {
                Output.WriteLine(result.LastResult.ToString());
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

        private void RunInHaltedState()
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
                        EvalAndPrint(line, consumeIncompleteInput: false);
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
