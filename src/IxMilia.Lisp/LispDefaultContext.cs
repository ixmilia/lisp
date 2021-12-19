using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace IxMilia.Lisp
{
    public class LispDefaultContext
    {
        [LispFunction("ERROR")]
        public LispObject Error(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length >= 1)
            {
                var formatArgs = new List<LispObject>();
                formatArgs.Add(host.Nil); // force raw string generation
                formatArgs.AddRange(args);
                var candidateErrorString = Format(host, executionState, formatArgs.ToArray());
                switch (candidateErrorString)
                {
                    case LispString errorString:
                        return new LispError(errorString.Value);
                    case LispError error:
                        return error;
                    default:
                        return new LispError($"Unable to format error string, got: {candidateErrorString}");
                }
            }

            return new LispError("Expected format string");
        }

        [LispFunction("BREAK")]
        public LispObject Break(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length >= 1)
            {
                var formatArgs = new List<LispObject>();
                formatArgs.Add(host.Nil); // force raw string generation
                formatArgs.AddRange(args);
                var candidateErrorString = Format(host, executionState, formatArgs.ToArray());
                if (candidateErrorString is LispError error)
                {
                    return error;
                }

                var displayString = candidateErrorString as LispString;
                executionState.StackFrame.Root.TerminalIO.Output.WriteLine(displayString?.Value);
                EventHandler<LispEvaluatingExpressionEventArgs> halter = null;
                halter = new EventHandler<LispEvaluatingExpressionEventArgs>((s, e) =>
                {
                    // halt on very next expresion and never again
                    e.HaltExecution = true;
                    executionState.StackFrame.Root.EvaluatingExpression -= halter;
                });
                executionState.StackFrame.Root.EvaluatingExpression += halter;
                return host.Nil;
            }

            return new LispError("Expected format string");
        }

        [LispMacro("DEFMACRO")]
        public LispObject DefineMacro(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate arg types and count
            var macroNameSymbol = (LispSymbol)args[0];
            var macroName = macroNameSymbol.Value;
            var macroArgs = ((LispList)args[1]).ToList();
            if (!LispArgumentCollection.TryBuildArgumentCollection(macroArgs.ToArray(), out var argumentCollection, out var error))
            {
                return error;
            }

            // TODO: allow docstring
            var macroBody = args.Skip(2);
            var macro = new LispCodeMacro(macroName, argumentCollection, macroBody)
            {
                SourceLocation = macroNameSymbol.SourceLocation,
            };
            executionState.StackFrame.SetValueInParentScope(macroName, macro);
            return host.Nil;
        }

        [LispMacro("DEFUN")]
        public LispObject DefineFunction(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (!TryGetCodeFunctionFromItems(args, out var codeFunction, out var error))
            {
                return error;
            }

            codeFunction.SourceLocation = executionState.StackFrame.SourceLocation;
            executionState.StackFrame.SetValueInParentScope(codeFunction.Name, codeFunction);
            return host.Nil;
        }

        internal static bool TryGetCodeFunctionFromItems(LispObject[] items, out LispCodeFunction codeFunction, out LispError error)
        {
            codeFunction = default;

            // TODO: properly validate types and arg counts
            var name = ((LispSymbol)items[0]).Value;
            var argumentList = ((LispList)items[1]).ToList();
            if (!LispArgumentCollection.TryBuildArgumentCollection(argumentList.ToArray(), out var argumentCollection, out error))
            {
                return false;
            }

            var commands = items.Skip(2).ToList();
            string documentation = null;
            if (commands[0] is LispString str)
            {
                documentation = str.Value;
                commands.RemoveAt(0);
            }

            codeFunction = new LispCodeFunction(name, documentation, argumentCollection, commands);
            return true;
        }

        [LispMacro("DEFVAR")]
        public LispObject DefineVariable(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: properly validage single symbol argument
            var symbol = (LispSymbol)args[0];
            var name = symbol.Value;
            executionState.StackFrame.SetValueInParentScope(name, symbol);
            return symbol;
        }

        [LispMacro("LET")]
        public LispObject Let(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            return Let(host, executionState.StackFrame, args, bindSequentially: false);
        }

        [LispMacro("LET*")]
        public LispObject LetStar(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            return Let(host, executionState.StackFrame, args, bindSequentially: true);
        }

        private LispObject Let(LispHost host, LispStackFrame frame, LispObject[] args, bool bindSequentially)
        {
            // TODO: validate arguments
            var values = ((LispList)args[0]).ToList();
            var replacements = new Dictionary<string, LispObject>();
            var body = args.Skip(1);
            foreach (var valuePair in values)
            {
                // TODO: validate shape
                var valuePairList = (LispList)valuePair;
                var varName = ((LispSymbol)valuePairList.Value).Value;
                var varRawValue = ((LispList)valuePairList.Next).Value;
                var replacedRawValue = bindSequentially
                    ? varRawValue.PerformMacroReplacements(replacements)
                    : varRawValue;
                var varValue = host.EvalAtStackFrame(frame, replacedRawValue);
                if (varValue is LispError error)
                {
                    return error;
                }

                // quoted because lists shouldn't get force-evaluated
                replacements[varName] = LispList.FromItems(new LispSymbol("QUOTE"), varValue);
            }

            var result = body.PerformMacroReplacements(replacements);
            return result;
        }

        [LispMacro("LABELS")]
        public LispObject Labels(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate arguments
            var functionDefinitions = ((LispList)args[0]).ToList();
            var replacements = new Dictionary<string, LispObject>();
            var body = args.Skip(1);
            foreach (var functionDefinitionSet in functionDefinitions)
            {
                // TODO: validate shape
                var functionDefinition = ((LispList)functionDefinitionSet).ToList();
                if (!TryGetCodeFunctionFromItems(functionDefinition.ToArray(), out var codeFunction, out var error))
                {
                    return error;
                }

                var replacedCodeFunction = (LispCodeFunction)codeFunction.PerformMacroReplacements(replacements);
                replacedCodeFunction.SourceLocation = functionDefinitionSet.SourceLocation;
                replacements[codeFunction.Name] = replacedCodeFunction;

                // ensure recursive function calls get updated
                var selfReplacement = new Dictionary<string, LispObject>()
                {
                    { replacedCodeFunction.Name, replacedCodeFunction }
                };
                replacedCodeFunction.Commands = replacedCodeFunction.Commands.PerformMacroReplacements(selfReplacement).ToList().ToArray();
            }

            var result = body.PerformMacroReplacements(replacements);
            return result;
        }

        [LispMacro("EVAL")]
        public LispObject Eval(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            LispObject result;
            if (args.Length == 1)
            {
                result = host.EvalAtStackFrame(executionState.StackFrame, args[0]);
            }
            else
            {
                result = new LispError("Expected exactly one argument");
            }

            return result;
        }

        [LispFunction("APPLY")]
        public LispObject Apply(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length == 2 &&
                args[0] is LispFunctionReference functionRef &&
                args[1] is LispList functionArguments)
            {
                return FunCall(host, executionState.StackFrame, functionRef, functionArguments.ToList());
            }
            else
            {
                return new LispError("Expected function reference and list of arguments");
            }
        }

        internal static LispObject FunCall(LispHost host, LispStackFrame evaluatingFrame, LispFunctionReference functionReference, IEnumerable<LispObject> functionArguments)
        {
            string synthesizedFunctionName = null;
            Action preExecute = null;
            Action postExecute = null;
            if (functionReference is LispQuotedNamedFunctionReference namedFunction)
            {
                synthesizedFunctionName = namedFunction.Name;
            }
            else if (functionReference is LispQuotedLambdaFunctionReference lambdaFunction)
            {
                synthesizedFunctionName = lambdaFunction.Definition.Name;
                evaluatingFrame = lambdaFunction.StackFrame;
                preExecute = () => evaluatingFrame.SetValue(lambdaFunction.Definition.Name, lambdaFunction.Definition);
                postExecute = () => evaluatingFrame.DeleteValue(lambdaFunction.Definition.Name);
            }

            if (synthesizedFunctionName != null)
            {
                var synthesizedSymbol = new LispSymbol(synthesizedFunctionName);
                synthesizedSymbol.SourceLocation = functionReference.SourceLocation;
                var synthesizedFunctionItems = new List<LispObject>();
                synthesizedFunctionItems.Add(synthesizedSymbol);
                synthesizedFunctionItems.AddRange(functionArguments);
                var synthesizedFunctionCall = LispList.FromEnumerable(synthesizedFunctionItems);
                synthesizedFunctionCall.SourceLocation = functionReference.SourceLocation;

                preExecute?.Invoke();
                var result = host.EvalAtStackFrame(evaluatingFrame, synthesizedFunctionCall);
                postExecute?.Invoke();

                if (!result.SourceLocation.HasValue)
                {
                    result.SourceLocation = functionReference.SourceLocation;
                }

                return result;
            }

            return new LispError("Expected function reference");
        }

        [LispMacro("FUNCALL")]
        public LispObject FunCall(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            LispObject result = new LispError("Expected function reference");
            if (args.Length >= 1)
            {
                var candidateFunctionReference = host.EvalAtStackFrame(executionState.StackFrame, args[0]);
                if (candidateFunctionReference is LispFunctionReference functionReference)
                {
                    result = FunCall(host, executionState.StackFrame, functionReference, args.Skip(1));
                }
            }

            // the evalutated result is the result of the `funcall` macro, so it has to be quoted to allow it to pass through
            var quotedResult = LispList.FromItems(new LispSymbol("QUOTE"), result);
            return quotedResult;
        }

        [LispFunction("MAKE-STRING-INPUT-STREAM")]
        public LispObject MakeStringInputStream(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length >= 1 &&
                args[0] is LispString inputString)
            {
                var startIndex = 0;
                var endIndex = inputString.Value.Length - 1;
                if (args.Length >= 2 &&
                    args[1] is LispInteger startIndexBound)
                {
                    startIndex = startIndexBound.Value;
                    if (args.Length >= 3 &&
                        args[2] is LispInteger endIndexBound)
                    {
                        endIndex = endIndexBound.Value;
                        // TODO: allow fourth argument of an input string stream
                    }
                }

                var fullString = inputString.Value.Substring(startIndex, endIndex - startIndex + 1);
                var input = new StringReader(fullString);
                var inputStream = new LispStream("", input, TextWriter.Null);
                return inputStream;
            }
            else
            {
                return new LispError("Expected an input string");
            }
        }

        [LispFunction("FORMAT")]
        public LispObject Format(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length >= 2 &&
                args[1] is LispString s)
            {
                var formatArgs = args.Skip(2);
                if (LispFormatter.TryFormatString(s.Value, formatArgs, out var result))
                {
                    LispStream stream;
                    if (args[0] == host.T)
                    {
                        // write to terminal
                        stream = host.TerminalIO;
                    }
                    else if (args[0].IsNil())
                    {
                        // return formatted string
                        return new LispString(result);
                    }
                    else if (args[0] is LispStream suppliedStream)
                    {
                        stream = suppliedStream;
                    }
                    else
                    {
                        return new LispError("Unsupported output stream");
                    }

                    stream.Output.Write(result);
                    stream.Output.Flush();
                    return host.Nil;
                }
                else
                {
                    return new LispError(result);
                }
            }

            return new LispError("Expected output type and string");
        }

        [LispFunction("CODE-CHAR")]
        public LispObject CodeChar(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length == 1 &&
                args[0] is LispInteger i)
            {
                var c = (char)i.Value;
                return new LispCharacter(c);
            }

            return new LispError("Expected an integer");
        }

        [LispFunction("CHAR=")]
        public LispObject CharEquals(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length == 2 &&
                args[0] is LispCharacter c1 &&
                args[1] is LispCharacter c2)
            {
                return c1.Value == c2.Value
                    ? host.T
                    : host.Nil;
            }

            return new LispError("Expected 2 characters");
        }

        [LispFunction("PEEK-CHAR")]
        public LispObject PeekChar(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            var peekType = host.Nil;
            var inputStream = host.TerminalIO;
            var errorOnEof = true;
            var eofValue = host.Nil;
            var _isRecursive = false;
            if (args.Length >= 1)
            {
                var argumentOffset = 0;
                if (args[0].IsNil() ||
                    args[0] == host.T ||
                    args[0] is LispCharacter)
                {
                    peekType = args[0];
                    argumentOffset = 1;
                }

                if (args[0 + argumentOffset] is LispStream stream)
                {
                    inputStream = stream;
                }
                else
                {
                    return new LispError("Expected an input stream");
                }

                if (args.Length >= 2 + argumentOffset)
                {
                    errorOnEof = args[1 + argumentOffset].IsTLike();

                    if (args.Length >= 3 + argumentOffset)
                    {
                        eofValue = args[2 + argumentOffset];

                        if (args.Length >= 4 + argumentOffset)
                        {
                            _isRecursive = args[3 + argumentOffset].IsTLike();

                            if (args.Length >= 5 + argumentOffset)
                            {
                                return new LispError("Too many arguments");
                            }
                        }
                    }
                }
            }

            return PeekChar(peekType, inputStream, errorOnEof, eofValue, _isRecursive);
        }

        [LispFunction("READ-CHAR")]
        public LispObject ReadChar(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            var inputStream = host.TerminalIO;
            var errorOnEof = true;
            var eofValue = host.Nil;
            var _isRecursive = false;
            if (args.Length >= 1)
            {
                if (args[0] is LispStream stream)
                {
                    inputStream = stream;
                }
                else
                {
                    return new LispError("Expected an input stream");
                }

                if (args.Length >= 2)
                {
                    errorOnEof = args[1].IsTLike();

                    if (args.Length >= 3)
                    {
                        eofValue = args[2];

                        if (args.Length >= 4)
                        {
                            _isRecursive = args[3].IsTLike();

                            if (args.Length >= 5)
                            {
                                return new LispError("Too many arguments");
                            }
                        }
                    }
                }
            }

            return ReadChar(inputStream, errorOnEof, eofValue, _isRecursive);
        }

        internal static LispObject PeekChar(LispObject peekType, LispStream inputStream, bool errorOnEof, LispObject eofValue, bool isRecursive)
        {
            Func<char, bool> shouldConsumeAndSkip;
            if (peekType == null || peekType.IsNil())
            {
                // simple peek, don't consume
                shouldConsumeAndSkip = (_) => false;
            }
            else if (peekType is LispCharacter lc)
            {
                // consume until `lc`, then return lc
                shouldConsumeAndSkip = (c) => lc.Value != c;
            }
            else if (peekType.IsTLike())
            {
                // consume whitespace, return next
                shouldConsumeAndSkip = (c) => LispObjectReader.IsSkippableWhitespace(c);
            }
            else
            {
                return new LispError("Expected `nil`, `t` or character");
            }

            int peeked;
            char peekedChar;
            while (true)
            {
                peeked = inputStream.Input.Peek();
                peekedChar = (char)peeked;
                if (peeked == -1)
                {
                    // eof
                    if (errorOnEof)
                    {
                        return new LispError("EOF");
                    }
                    else
                    {
                        return eofValue;
                    }
                }

                if (!shouldConsumeAndSkip(peekedChar))
                {
                    break;
                }

                // swallow it
                inputStream.Input.Read();
            }

            return new LispCharacter(peekedChar);
        }

        internal static LispObject ReadChar(LispStream inputStream, bool errorOnEof, LispObject eofValue, bool isRecursive)
        {
            var peeked = inputStream.Input.Peek();
            if (peeked == -1)
            {
                // eof
                if (errorOnEof)
                {
                    return new LispError("EOF");
                }
                else
                {
                    return eofValue;
                }
            }

            var c = (char)inputStream.Input.Read();
            return new LispCharacter(c);
        }

        [LispFunction("READ")]
        public LispObject Read(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            var inputStream = host.TerminalIO;
            var errorOnEof = true;
            var eofValue = host.Nil;
            var isRecursive = false;
            if (args.Length >= 1)
            {
                if (args[0] is LispStream stream)
                {
                    inputStream = stream;
                }
                else
                {
                    return new LispError("Expected an input stream");
                }

                if (args.Length >= 2)
                {
                    errorOnEof = args[1].IsTLike();

                    if (args.Length >= 3)
                    {
                        eofValue = args[2];

                        if (args.Length >= 4)
                        {
                            isRecursive = args[3].IsTLike();

                            if (args.Length >= 5)
                            {
                                return new LispError("Too many arguments");
                            }
                        }
                    }
                }
            }

            LispObject result = null;
            var previousReaderStream = host.ObjectReader.InputStream;
            try
            {
                host.ObjectReader.SetReaderStream(inputStream);
                var readerResult = host.ObjectReader.Read(errorOnEof, eofValue, isRecursive);
                result = readerResult.LastResult;
            }
            finally
            {
                host.ObjectReader.SetReaderStream(previousReaderStream);
            }

            return result;
        }

        [LispMacro("WITH-OPEN-FILE")]
        public LispObject WithOpenFile(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length >= 2 &&
                args[0] is LispList openArguments &&
                openArguments.Length >= 2 &&
                openArguments.Value is LispSymbol streamName &&
                openArguments.Next is LispList filePathList &&
                filePathList.Length >= 1)
            {
                var openArgumentsList = openArguments.ToList();
                var directionArgument = GetKeywordArgument(openArgumentsList.Skip(2), ":DIRECTION");
                var fileMode = directionArgument is LispKeyword directionKeyword && directionKeyword.Keyword == ":OUTPUT"
                    ? FileMode.OpenOrCreate
                    : FileMode.Open;

                var candidateFilePath = host.EvalAtStackFrame(executionState.StackFrame, filePathList.Value);
                if (candidateFilePath is LispString filePath)
                {
                    var fileStream = new FileStream(filePath.Value, fileMode);
                    var streamObject = new LispFileStream(filePath.Value, fileStream);
                    var body = args.Skip(1);
                    var result = body.PerformMacroReplacements(new Dictionary<string, LispObject>() { { streamName.Value, streamObject } });
                    var closeExpression = LispList.FromEnumerable(new LispObject[] { new LispSymbol("CLOSE"), streamObject }); // (close fileStream)
                    var finalResult = new LispList(new LispSymbol("PROGN"), new LispList(result, new LispList(closeExpression)));
                    return finalResult;
                }
                else
                {
                    return new LispError("Expected a string file path");
                }
            }

            return new LispError("Expected `<(streamName filePath)> <body>`");
        }

        [LispFunction("CLOSE")]
        public LispObject Close(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length == 1 &&
                args[0] is LispFileStream fileStream)
            {
                fileStream.FileStream.Flush();
                fileStream.FileStream.Dispose();
                return host.Nil;
            }

            return new LispError("Expected a file stream");
        }

        [LispFunction("DRIBBLE")]
        public LispObject Dribble(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length == 0)
            {
                // finish recording
                var dribbleStream = executionState.StackFrame.Root.DribbleStream;
                if (dribbleStream is null)
                {
                    return new LispError("Dribble stream object not found");
                }

                host.TerminalIO.Output.WriteLine($"Finished recording in file {dribbleStream.FileStream.Name}");
                dribbleStream.Output.Flush();
                dribbleStream.Output.Dispose();
                executionState.StackFrame.Root.DribbleStream = null;
                return host.Nil;
            }
            else if (args.Length == 1)
            {
                var filePath = host.EvalAtStackFrame(executionState.StackFrame, args[0]);
                if (filePath is LispError error)
                {
                    return error;
                }
                else if (filePath is LispString dribblePath)
                {
                    // start new recording
                    if (executionState.StackFrame.Root.DribbleStream is LispFileStream)
                    {
                        return new LispError("Dribble recording already started");
                    }

                    var dribbleStream = new LispFileStream(dribblePath.Value, new FileStream(dribblePath.Value, FileMode.Create));
                    dribbleStream.Output.WriteLine($";Recording in {dribbleStream.FileStream.Name}");
                    dribbleStream.Output.WriteLine($";Recording started at {DateTime.Now:h:mmtt d-MMM-yy}:");
                    dribbleStream.Output.WriteLine();
                    host.TerminalIO.Output.WriteLine($"Now recording in file {dribbleStream.FileStream.Name}");

                    executionState.StackFrame.Root.DribbleStream = dribbleStream;
                    return host.Nil;
                }
                else
                {
                    return new LispError("Expected a string path to start dribble recording.");
                }
            }

            return new LispError("Expected single file path arugment to start recording, or no arguments to stop");
        }

        [LispMacro("SETF")]
        [LispMacro("SETQ")]
        public LispObject SetValue(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: properly validate types
            LispObject last = host.Nil;
            for (int i = 0; i < args.Length - 1; i += 2)
            {
                var destination = args[i];
                var rawValue = args[i + 1];
                var value = host.EvalAtStackFrame(executionState.StackFrame, rawValue);
                if (destination is LispSymbol symbol)
                {
                    var name = symbol.Value;
                    executionState.StackFrame.SetValueInParentScope(name, value);
                }
                else
                {
                    destination = host.EvalAtStackFrame(executionState.StackFrame, destination);
                    if (destination.SetPointerValue != null)
                    {
                        destination.SetPointerValue(value);
                    }
                    else
                    {
                        return new LispError("Expected symbol or pointer location");
                    }
                }

                destination.SetPointerValue = null;
                last = value;
            }

            return LispList.FromItems(new LispSymbol("QUOTE"), last);
        }

        [LispFunction("NUMBERP")]
        public LispObject NumberP(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispInteger _:
                case LispFloat _:
                case LispRatio _:
                    return host.T;
                default:
                    return host.Nil;
            }
        }

        [LispFunction("STRINGP")]
        public LispObject StringP(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispString _:
                    return host.T;
                default:
                    return host.Nil;
            }
        }

        [LispFunction("KEYWORDP")]
        public LispObject KeywordP(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispKeyword _:
                    return host.T;
                default:
                    return host.Nil;
            }
        }

        [LispFunction("SYMBOLP")]
        public LispObject SymbolP(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispKeyword _:
                case LispSymbol _:
                    return host.T;
                default:
                    return host.Nil;
            }
        }

        [LispFunction("ZEROP")]
        public LispObject ZeroP(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispInteger i when i.IsZero:
                    return host.T;
                case LispFloat f when f.IsZero:
                    return host.T;
                case LispRatio r when r.IsZero:
                    return host.T;
                default:
                    return host.Nil;
            }
        }

        [LispFunction("PLUSP")]
        public LispObject PlusP(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate argument count
            if (args[0] is LispNumber num)
            {
                switch (num)
                {
                    case LispInteger i when i.Value > 0:
                        return host.T;
                    case LispFloat f when f.Value > 0.0:
                        return host.T;
                    case LispRatio r when !r.IsZero && Math.Sign(r.Numerator) == Math.Sign(r.Denominator):
                        return host.T;
                    default:
                        return host.Nil;
                }
            }

            return new LispError("wrong type input");
        }

        [LispFunction("EVENP")]
        public LispObject EvenP(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispInteger num when num.IsEven:
                    return host.T;
                case LispFloat num when num.IsEven:
                    return host.T;
                default:
                    return host.Nil;
            }
        }

        [LispFunction("ODDP")]
        public LispObject OddP(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispInteger num when num.IsOdd:
                    return host.T;
                case LispFloat num when num.IsOdd:
                    return host.T;
                default:
                    return host.Nil;
            }
        }

        [LispFunction("LISTP")]
        public LispObject ListP(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate single argument
            return args[0] is LispList
                ? host.T
                : host.Nil;
        }

        [LispFunction("CONSP")]
        public LispObject ConsP(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            return args[0] is LispList list && !list.IsNil()
                ? host.T
                : host.Nil;
        }

        [LispFunction("STREAMP")]
        public LispObject StreamP(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            return args.Length == 1 && args[0] is LispStream
                ? host.T
                : host.Nil;
        }

        [LispMacro("FUNCTION")]
        public LispObject Function(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate argument count
            if (args[0] is LispSymbol symbol)
            {
                return new LispQuotedNamedFunctionReference(symbol.Value);
            }
            else
            {
                return new LispError("Expected a function symbol");
            }
        }

        [LispFunction("CONS")]
        public LispObject Cons(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate arguments
            return new LispList(args[0], args[1]);
        }

        [LispFunction("LIST")]
        public LispObject List(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            return LispList.FromEnumerable(args);
        }

        [LispFunction("LENGTH")]
        public LispObject Length(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate single argument
            if (args[0] is LispList list)
            {
                return new LispInteger(list.Length);
            }
            else
            {
                return new LispError("Expected a list");
            }
        }

        [LispFunction("CAR")]
        [LispFunction("FIRST")]
        public LispObject First(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate single argument
            if (args[0] is LispList list)
            {
                var result = list.Value;
                result.SetPointerValue = (value) => list.Value = value;
                return result;
            }
            else
            {
                return new LispError($"Expected a list, found {args[0]}");
            }
        }

        [LispFunction("CDR")]
        [LispFunction("REST")]
        public LispObject Rest(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate single argument
            if (args[0] is LispList list)
            {
                var result = list.Next;
                result.SetPointerValue = (value) => list.Next = value;
                return result;
            }
            else
            {
                return new LispError($"Expected a list, found {args[0]}");
            }
        }

        [LispFunction("KERNEL:APPEND/2")]
        public LispObject TwoArgumentAppend(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length == 2 &&
                args[0] is LispList l1)
            {
                var result = args[1];
                var headItems = l1.ToList();
                for (int i = headItems.Count - 1; i >= 0; i--)
                {
                    var newResult = new LispList(headItems[i], result);
                    result = newResult;
                }

                return result;
            }

            return new LispError("Expected a list and a tail");
        }

        [LispMacro("NCONC")]
        public LispObject Nconc(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            LispObject lastResult = host.Nil;
            for (int i = args.Length - 2; i >= 0; i--)
            {
                var a1Raw = args[i];
                var a2 = args[i + 1];

                var evalResult = host.Eval(a1Raw);
                var a1 = evalResult.LastResult;

                // lastResult = (append a1 a2)
                var simulatedFunctionCall1 = LispList.FromItems(new LispSymbol("APPEND"), LispList.FromItems(new LispSymbol("QUOTE"), a1), a2);
                lastResult = host.EvalAtStackFrame(executionState.StackFrame, simulatedFunctionCall1);
                if (lastResult is LispError error)
                {
                    return error;
                }

                lastResult = LispList.FromItems(new LispSymbol("QUOTE"), lastResult);

                // a1 = lastResult
                if (!a1.IsNil())
                {
                    var simulatedFunctionCall2 = LispList.FromItems(new LispSymbol("SETF"), a1Raw, lastResult);
                    host.EvalAtStackFrame(executionState.StackFrame, simulatedFunctionCall2);
                }
            }

            return lastResult;
        }

        [LispFunction("REVERSE")]
        public LispObject Reverse(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length == 1 && args[0] is LispList list)
            {
                var values = list.ToList().Reverse();
                var result = LispList.FromEnumerable(values);
                return result;
            }
            else
            {
                return new LispError("Expected a single list");
            }
        }

        [LispFunction("INTERSECTION")]
        public LispObject Intersection(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length == 2 &&
                args[0] is LispList a &&
                args[1] is LispList b)
            {
                var valuesInOne = new HashSet<LispObject>(a.ToList(), LispObject.Comparer);
                var finalSet = b.ToList().Where(i => valuesInOne.Contains(i, LispObject.Comparer));
                return LispList.FromEnumerable(finalSet);
            }
            else
            {
                return new LispError("Expected 2 lists");
            }
        }

        [LispFunction("UNION")]
        public LispObject Union(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length == 2 &&
                args[0] is LispList a &&
                args[1] is LispList b)
            {
                var finalSet = new HashSet<LispObject>(a.ToList(), LispObject.Comparer);
                foreach (var item in b.ToList())
                {
                    finalSet.Add(item);
                }

                return LispList.FromEnumerable(finalSet);
            }
            else
            {
                return new LispError("Expected 2 lists");
            }
        }

        [LispFunction("SET-DIFFERENCE")]
        public LispObject SetDifference(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length == 2 &&
                args[0] is LispList a &&
                args[1] is LispList b)
            {
                var finalSet = new HashSet<LispObject>(a.ToList(), LispObject.Comparer);
                foreach (var item in b.ToList())
                {
                    finalSet.Remove(item);
                }

                return LispList.FromEnumerable(finalSet);
            }
            else
            {
                return new LispError("Expected 2 lists");
            }
        }

        [LispFunction("SET-EXCLUSIVE-OR")]
        public LispObject SetExclusiveOr(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length == 2 &&
               args[0] is LispList a &&
               args[1] is LispList b)
            {
                var inA = new HashSet<LispObject>(a.ToList(), LispObject.Comparer);
                var inB = new HashSet<LispObject>(b.ToList(), LispObject.Comparer);
                var inBoth = inA.Intersect(inB, LispObject.Comparer);
                var finalSet = inA.Union(inB, LispObject.Comparer).Except(inBoth, LispObject.Comparer);
                return LispList.FromEnumerable(finalSet);
            }
            else
            {
                return new LispError("Expected 2 lists");
            }
        }

        [LispFunction("REMOVE")]
        public LispObject Remove(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TOOD: validate arguments
            var key = args[0];
            var list = ((LispList)args[1]).ToList();
            var result = new List<LispObject>();
            var count = GetKeywordArgument(args.Skip(2), ":COUNT");
            var limit = count is LispInteger number
                ? number.Value
                : list.Count;
            var fromEndArg = GetKeywordArgument(args.Skip(2), ":FROM-END");
            var fromEnd = fromEndArg.IsTLike();
            if (fromEnd)
            {
                list = list.Reverse().ToList();
            }

            var removeCount = 0;
            foreach (var item in list)
            {
                if (removeCount < limit && key.Equals(item))
                {
                    removeCount++;
                }
                else
                {
                    result.Add(item);
                }
            }

            if (fromEnd)
            {
                result.Reverse();
            }

            return LispList.FromEnumerable(result);
        }

        [LispFunction("REMOVE-DUPLICATES")]
        public LispObject RemoveDuplicates(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length == 1 && args[0] is LispList list)
            {
                var finalItems = new List<LispObject>();
                var seenItems = new HashSet<LispObject>();
                foreach (var item in list.ToList())
                {
                    if (seenItems.Add(item))
                    {
                        finalItems.Add(item);
                    }
                }

                return LispList.FromEnumerable(finalItems);
            }
            else
            {
                return new LispError("Expected a list");
            }
        }

        [LispFunction("FIND-IF")]
        public LispObject FindIf(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate argument counts
            if (args.Length >= 2 &&
                args[0] is LispFunctionReference functionReference &&
                args[1] is LispList inputList)
            {
                var fromEndKeyword = GetKeywordArgument(args.Skip(2), ":FROM-END");
                var fromEnd = fromEndKeyword.IsTLike();
                var items = inputList.ToList();
                if (fromEnd)
                {
                    items = items.Reverse().ToList();
                }

                foreach (var item in items)
                {
                    // quote the arguments so they can be safely evaluated
                    var functionArguments = new LispObject[]
                    {
                        LispList.FromItems(new LispSymbol("QUOTE"), item)
                    };
                    var result = FunCall(host, executionState.StackFrame, functionReference, functionArguments);
                    if (result is LispError)
                    {
                        return result;
                    }

                    if (result.IsTLike())
                    {
                        return item;
                    }
                }

                return host.Nil;
            }
            else
            {
                return new LispError("Expected a function reference and list");
            }
        }

        [LispFunction("REMOVE-IF")]
        public LispObject RemoveIf(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate argument counts
            if (args.Length >= 2 &&
                args[0] is LispFunctionReference functionReference &&
                args[1] is LispList inputList)
            {
                var countArgument = GetKeywordArgument(args.Skip(2), ":COUNT");
                var count = countArgument is LispInteger i
                    ? i.Value
                    : int.MaxValue;
                var items = inputList.ToList();
                var resultItems = new List<LispObject>();
                var removed = 0;
                foreach (var item in items)
                {
                    var result = FunCall(host, executionState.StackFrame, functionReference, new LispObject[] { item });
                    if (result is LispError)
                    {
                        return result;
                    }

                    if (result.IsTLike() && removed < count)
                    {
                        // remove it
                        removed++;
                    }
                    else
                    {
                        // keep it
                        resultItems.Add(item);
                    }
                }

                return LispList.FromEnumerable(resultItems);
            }
            else
            {
                return new LispError("Expected a function reference and list");
            }
        }

        [LispFunction("REMOVE-IF-NOT")]
        public LispObject RemoveIfNot(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate argument counts
            if (args.Length >= 2 &&
                args[0] is LispFunctionReference functionReference &&
                args[1] is LispList inputList)
            {
                var countArgument = GetKeywordArgument(args.Skip(2), ":COUNT");
                var count = countArgument is LispInteger i
                    ? i.Value
                    : int.MaxValue;
                var items = inputList.ToList();
                var resultItems = new List<LispObject>();
                var removed = 0;
                foreach (var item in items)
                {
                    var result = FunCall(host, executionState.StackFrame, functionReference, new LispObject[] { item });
                    if (result is LispError)
                    {
                        return result;
                    }

                    if (result.IsNil() && removed < count)
                    {
                        // remove it
                        removed++;
                    }
                    else
                    {
                        // keep it
                        resultItems.Add(item);
                    }
                }

                return LispList.FromEnumerable(resultItems);
            }
            else
            {
                return new LispError("Expected a function reference and list");
            }
        }

        [LispFunction("REDUCE")]
        public LispObject Reduce(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate argument counts
            if (args.Length >= 2 &&
                args[0] is LispFunctionReference functionReference &&
                args[1] is LispList inputList)
            {
                var fromEndKeyword = GetKeywordArgument(args.Skip(2), ":FROM-END");
                var fromEnd = fromEndKeyword.IsTLike();
                var items = inputList.ToList();
                if (fromEnd)
                {
                    items = items.Reverse().ToList();
                }

                while (items.Count > 1)
                {
                    var arg1 = items[fromEnd ? 1 : 0];
                    var arg2 = items[fromEnd ? 0 : 1];
                    items.RemoveAt(0);
                    items.RemoveAt(0);
                    var result = FunCall(host, executionState.StackFrame, functionReference, new LispObject[] { LispList.FromItems(new LispSymbol("QUOTE"), arg1), LispList.FromItems(new LispSymbol("QUOTE"), arg2) });
                    if (result is LispError)
                    {
                        return result;
                    }

                    items.Insert(0, result);
                }

                if (items.Count == 0)
                {
                    return host.Nil;
                }
                else
                {
                    return items[0];
                }
            }
            else
            {
                return new LispError("Expected a function reference and list");
            }
        }

        [LispFunction("EVERY")]
        public LispObject Every(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            var result = MapCar(host, executionState, args);
            switch (result)
            {
                case LispList list when list.ToList().Any(o => o.IsNil()):
                    return host.Nil;
                default:
                    return host.T;
            }
        }

        [LispFunction("MAPCAR")]
        public LispObject MapCar(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length >= 2)
            {
                var candidateLists = args.Skip(1).Select(a => a as LispList).ToArray();
                if (candidateLists.Any(a => a == null))
                {
                    return new LispError("Expected function reference and only lists");
                }

                Func<IEnumerable<LispObject>, LispObject> evaluator;
                switch (args[0])
                {
                    case LispInvocableObject directlyInvocable:
                        evaluator = (functionArguments) =>
                        {
                            var manualInvokeItems = new List<LispObject>() { directlyInvocable };
                            manualInvokeItems.AddRange(functionArguments);
                            var manualInvokeList = LispList.FromEnumerable(manualInvokeItems);
                            return host.EvalAtStackFrame(executionState.StackFrame, manualInvokeList);
                        };
                        break;
                    case LispFunctionReference functionRef:
                        evaluator = (functionArguments) => FunCall(host, executionState.StackFrame, functionRef, functionArguments);
                        break;
                    default:
                        return new LispError($"Unsupported `mapcar` execution target: {args[0].GetType().Name}");
                }

                var resultItems = new List<LispObject>();
                var lists = candidateLists.Select(l => l.ToList());
                var maxLength = lists.Select(l => l.Count).Aggregate(int.MaxValue, (aLength, bLength) => Math.Min(aLength, bLength));
                for (int i = 0; i < maxLength; i++)
                {
                    var functionArguments = lists.Select(l => l[i]);
                    var result = evaluator(functionArguments);
                    if (result is LispError)
                    {
                        return result;
                    }

                    resultItems.Add(result);
                }

                return LispList.FromEnumerable(resultItems);
            }

            return new LispError("Expected function reference and list");
        }

        [LispFunction("<")]
        public LispObject LessThan(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            return FoldComparison(executionState.StackFrame, args, (a, b) => LispNumber.LessThan(a, b));
        }

        [LispFunction("<=")]
        public LispObject LessThanOrEqual(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            return FoldComparison(executionState.StackFrame, args, (a, b) => LispNumber.LessThanOrEqual(a, b));
        }

        [LispFunction(">")]
        public LispObject GreaterThan(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            return FoldComparison(executionState.StackFrame, args, (a, b) => LispNumber.GreaterThan(a, b));
        }

        [LispFunction(">=")]
        public LispObject GreaterThanOrEqual(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            return FoldComparison(executionState.StackFrame, args, (a, b) => LispNumber.GreaterThanOrEqual(a, b));
        }

        [LispFunction("=")]
        public LispObject NumberEqual(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            return FoldComparison(executionState.StackFrame, args, (a, b) => LispNumber.Equal(a, b));
        }

        [LispFunction("EQUAL")]
        public LispObject Equal(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            return FoldObj(executionState.StackFrame, args, (a, b) => a.Equals(b));
        }

        [LispFunction("EQUALP")]
        public LispObject EqualP(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            return FoldObj(executionState.StackFrame, args, (a, b) =>
            {
                if (a is LispString sa && b is LispString sb)
                {
                    return string.Compare(sa.Value, sb.Value, StringComparison.OrdinalIgnoreCase) == 0;
                }

                return a.Equals(b);
            });
        }

        [LispFunction("EQ")]
        public LispObject Eq(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            return FoldObj(executionState.StackFrame, args, (a, b) => ReferenceEquals(a, b));
        }

        [LispFunction("EQL")]
        public LispObject Eql(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            return FoldObj(executionState.StackFrame, args, (a, b) =>
            {
                if (a is LispInteger ia && b is LispInteger ib && ia.Value == ib.Value)
                {
                    return true;
                }

                if (a is LispFloat fa && b is LispFloat fb && fa.Value == fb.Value)
                {
                    return true;
                }

                if (a is LispRatio ra && b is LispRatio rb && ra == rb)
                {
                    return true;
                }

                if (a is LispString sa && b is LispString sb && sa == sb)
                {
                    return true;
                }

                if (a is LispKeyword ka && b is LispKeyword kb && ka.Keyword == kb.Keyword)
                {
                    return true;
                }

                if (a is LispSymbol ssa && b is LispSymbol ssb && ssa.Value == ssb.Value)
                {
                    return true;
                }

                if (a is LispNilList && b is LispNilList)
                {
                    return true;
                }

                return ReferenceEquals(a, b);
            });
        }

        [LispFunction("!=")]
        [LispFunction("<>")]
        public LispObject NotEqual(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            return FoldObj(executionState.StackFrame, args, (a, b) => !a.Equals(b));
        }

        [LispMacro("AND")]
        public LispObject And(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            return FoldBoolean(host, executionState.StackFrame, args, true, false, (a, b) => a && b);
        }

        [LispMacro("OR")]
        public LispObject Or(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            return FoldBoolean(host, executionState.StackFrame, args, true, true, (a, b) => a || b);
        }

        [LispMacro("COND")]
        public LispObject Cond(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            foreach (var arg in args)
            {
                if (arg is LispList list && list.Length == 2)
                {
                    var values = list.ToList();
                    var predicate = host.EvalAtStackFrame(executionState.StackFrame, values[0]);
                    switch (predicate)
                    {
                        case LispError error:
                            return error;
                        case LispNilList _:
                            break;
                        default:
                            return values[1];
                    }
                }
                else
                {
                    return new LispError("Expected list of length 2")
                    {
                        SourceLocation = arg.SourceLocation,
                    };
                }
            }

            return host.Nil;
        }

        [LispFunction("ABS")]
        public LispObject Abs(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate arguments
            switch (args[0])
            {
                case LispInteger i:
                    return new LispInteger(Math.Abs(i.Value));
                case LispFloat f:
                    return new LispFloat(Math.Abs(f.Value));
                case LispRatio r:
                    return new LispRatio(Math.Abs(r.Numerator), Math.Abs(r.Denominator));
                default:
                    return new LispError($"Expected {nameof(LispInteger)} or {nameof(LispFloat)} but found {args[0].GetType().Name} with value {args[0]}");
            }
        }

        [LispFunction("SQRT")]
        public LispObject Sqrt(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            // TODO: validate arguments
            switch (args[0])
            {
                case LispInteger i:
                    return new LispFloat(Math.Sqrt(i.Value));
                case LispFloat f:
                    return new LispFloat(Math.Sqrt(f.Value));
                case LispRatio r:
                    return new LispFloat(Math.Sqrt(((LispFloat)r).Value));
                default:
                    return new LispError($"Expected {nameof(LispInteger)} or {nameof(LispFloat)} but found {args[0].GetType().Name} with value {args[0]}");
            }
        }

        [LispFunction("KERNEL:+/2")]
        public LispObject TwoAgumentPlus(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length == 2 &&
                args[0] is LispNumber n1 &&
                args[1] is LispNumber n2)
            {
                var result = LispNumber.Add(n1, n2);
                return result;
            }

            return new LispError("Expected exactly two numbers");
        }

        [LispFunction("KERNEL:-/1")]
        public LispObject OneArgumentMinus(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length == 1)
            {
                // simple negation
                var value = args[0];
                switch (value)
                {
                    case LispInteger num:
                        return new LispInteger(num.Value * -1);
                    case LispFloat num:
                        return new LispFloat(num.Value * -1.0);
                    case LispRatio num:
                        return new LispRatio(num.Numerator * -1, num.Denominator).Reduce();
                    default:
                        return new LispError($"Expected type number but found {value.GetType()}");
                }
            }

            return new LispError("Expected exactly two numbers");
        }

        [LispFunction("KERNEL:-/2")]
        public LispObject TwoArgumentMinus(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length == 2 &&
                args[0] is LispNumber n1 &&
                args[1] is LispNumber n2)
            {
                var result = LispNumber.Sub(n1, n2);
                return result;
            }

            return new LispError("Expected exactly two numbers");
        }

        [LispFunction("KERNEL:*/2")]
        public LispObject TwoArgumentAsterisk(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length == 2 &&
                args[0] is LispNumber n1 &&
                args[1] is LispNumber n2)
            {
                var result = LispNumber.Mul(n1, n2);
                return result;
            }

            return new LispError("Expected exactly two numbers");
        }

        [LispFunction("KERNEL://2")]
        public LispObject TwoArgumentSlash(LispHost host, LispExecutionState executionState, LispObject[] args)
        {
            if (args.Length == 2 &&
                args[0] is LispNumber n1 &&
                args[1] is LispNumber n2)
            {
                var result = LispNumber.Div(n1, n2);
                return result;
            }

            return new LispError("Expected exactly two numbers");
        }

        private static LispObject FoldBoolean(LispHost host, LispStackFrame frame, LispObject[] args, bool init, bool shortCircuitValue, Func<bool, bool, bool> operation)
        {
            LispObject result;
            if (args.Length == 0)
            {
                result = new LispError("Missing arguments");
            }
            else
            {
                var collected = init;
                foreach (var value in args)
                {
                    var evaluated = host.EvalAtStackFrame(frame, value);
                    if (evaluated is LispError error)
                    {
                        return error;
                    }
                    // TODO: non zero
                    var next = evaluated.IsTLike();
                    if (next == shortCircuitValue)
                    {
                        collected = shortCircuitValue;
                        goto done;
                    }
                    collected = operation(collected, next);
                }
            done:
                result = collected ? host.T : host.Nil;
            }

            return result;
        }

        private static LispObject FoldObj(LispStackFrame frame, LispObject[] args, Func<LispObject, LispObject, bool> operation)
        {
            if (args.Length < 2)
            {
                return new LispError("At least 2 arguments needed");
            }

            var result = true;
            for (int i = 0; i < args.Length - 1; i++)
            {
                result &= operation(args[i], args[i + 1]);
                if (!result)
                {
                    return frame.Nil;
                }
            }

            return frame.T;
        }

        private static LispObject FoldComparison(LispStackFrame frame, LispObject[] args, Func<LispNumber, LispNumber, bool> operation)
        {
            if (args.Length < 2)
            {
                return new LispError("At least 2 arguments needed");
            }

            LispNumber lastValue;
            switch (args[0])
            {
                case LispInteger i:
                    lastValue = i;
                    break;
                case LispFloat f:
                    lastValue = f;
                    break;
                case LispRatio r:
                    lastValue = r;
                    break;
                default:
                    return new LispError($"Expected number, got {args[0].GetType().Name} with value {args[0]}");
            }

            foreach (var value in args.Skip(1))
            {
                LispNumber nextValue;
                switch (value)
                {
                    case LispInteger i:
                        nextValue = i;
                        break;
                    case LispFloat f:
                        nextValue = f;
                        break;
                    case LispRatio r:
                        nextValue = r;
                        break;
                    default:
                        return new LispError($"Expected number, got {value.GetType().Name} with value {value}");
                }

                var result = operation(lastValue, nextValue);
                if (!result)
                {
                    return frame.Nil;
                }

                lastValue = nextValue;
            }

            return frame.T;
        }

        private static LispObject GetKeywordArgument(IEnumerable<LispObject> args, string keyword)
        {
            LispObject result = LispNilList.Instance;
            var enumerator = args.GetEnumerator();
            while (enumerator.MoveNext())
            {
                if (enumerator.Current is LispKeyword keywordSpecifier && keywordSpecifier.Keyword == keyword)
                {
                    if (enumerator.MoveNext())
                    {
                        result = enumerator.Current;
                    }

                    break;
                }
            }

            return result;
        }
    }
}
