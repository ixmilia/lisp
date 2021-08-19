using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace IxMilia.Lisp
{
    public class LispDefaultContext
    {
        [LispFunction("error")]
        public LispObject Error(LispStackFrame frame, LispObject[] args)
        {
            if (args.Length >= 1)
            {
                var formatArgs = new List<LispObject>();
                formatArgs.Add(frame.Nil); // force raw string generation
                formatArgs.AddRange(args);
                var candidateErrorString = Format(frame, formatArgs.ToArray());
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

        [LispFunction("break")]
        public LispObject Break(LispStackFrame frame, LispObject[] args)
        {
            if (args.Length >= 1)
            {
                var formatArgs = new List<LispObject>();
                formatArgs.Add(frame.Nil); // force raw string generation
                formatArgs.AddRange(args);
                var candidateErrorString = Format(frame, formatArgs.ToArray());
                if (candidateErrorString is LispError error)
                {
                    return error;
                }

                var displayString = candidateErrorString as LispString;
                frame.Root.TerminalIO.Output.WriteLine(displayString?.Value);
                EventHandler<LispEvaluatingExpressionEventArgs> halter = null;
                halter = new EventHandler<LispEvaluatingExpressionEventArgs>((s, e) =>
                {
                    // halt on very next expresion and never again
                    e.HaltExecution = true;
                    frame.Root.EvaluatingExpression -= halter;
                });
                frame.Root.EvaluatingExpression += halter;
                return frame.Nil;
            }

            return new LispError("Expected format string");
        }

        [LispMacro("defmacro")]
        public IEnumerable<LispObject> DefineMacro(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate arg types and count
            var macroNameSymbol = (LispSymbol)args[0];
            var macroName = macroNameSymbol.Value;
            var macroArgs = ((LispList)args[1]).ToList();
            if (!LispArgumentCollection.TryBuildArgumentCollection(macroArgs.ToArray(), out var argumentCollection, out var error))
            {
                return new LispObject[] { error };
            }

            // TODO: allow docstring
            var macroBody = args.Skip(2);
            var macro = new LispCodeMacro(macroName, argumentCollection, macroBody)
            {
                SourceLocation = macroNameSymbol.SourceLocation,
            };
            frame.SetValueInParentScope(macroName, macro);
            return new LispObject[] { frame.Nil };
        }

        [LispMacro("defun")]
        public IEnumerable<LispObject> DefineFunction(LispStackFrame frame, LispObject[] args)
        {
            if (!TryGetCodeFunctionFromItems(args, out var codeFunction, out var error))
            {
                return new LispObject[] { error };
            }

            codeFunction.SourceLocation = frame.SourceLocation;
            frame.SetValueInParentScope(codeFunction.Name, codeFunction);
            return new LispObject[] { frame.Nil };
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

        [LispMacro("defvar")]
        public IEnumerable<LispObject> DefineVariable(LispStackFrame frame, LispObject[] args)
        {
            // TODO: properly validage single symbol argument
            var symbol = (LispSymbol)args[0];
            var name = symbol.Value;
            frame.SetValueInParentScope(name, symbol);
            return new LispObject[] { symbol };
        }

        [LispMacro("let")]
        public IEnumerable<LispObject> Let(LispStackFrame frame, LispObject[] args)
        {
            return Let(frame, args, bindSequentially: false);
        }

        [LispMacro("let*")]
        public IEnumerable<LispObject> LetStar(LispStackFrame frame, LispObject[] args)
        {
            return Let(frame, args, bindSequentially: true);
        }

        private IEnumerable<LispObject> Let(LispStackFrame frame, LispObject[] args, bool bindSequentially)
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
                var varValue = frame.Eval(replacedRawValue);
                if (varValue is LispError error)
                {
                    return new LispObject[] { error };
                }

                // quoted because lists shouldn't get force-evaluated
                replacements[varName] = new LispQuotedObject(varValue);
            }

            var result = body.PerformMacroReplacements(replacements);
            return result;
        }

        [LispMacro("labels")]
        public IEnumerable<LispObject> Labels(LispStackFrame frame, LispObject[] args)
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
                    return new LispObject[] { error };
                }

                var replacedCodeFunction = (LispCodeFunction)codeFunction.PerformMacroReplacements(replacements);
                replacedCodeFunction.SourceLocation = functionDefinitionSet.SourceLocation;
                replacements[codeFunction.Name] = replacedCodeFunction;

                // ensure recursive function calls get updated
                var selfReplacement = new Dictionary<string, LispObject>()
                {
                    { replacedCodeFunction.Name, replacedCodeFunction }
                };
                replacedCodeFunction.Commands = replacedCodeFunction.Commands.PerformMacroReplacements(selfReplacement).ToArray();
            }

            var result = body.PerformMacroReplacements(replacements).ToArray();
            return result;
        }

        [LispMacro("eval")]
        public IEnumerable<LispObject> Eval(LispStackFrame frame, LispObject[] args)
        {
            LispObject result;
            if (args.Length == 1)
            {
                result = frame.Eval(args[0]);
            }
            else
            {
                result = new LispError("Expected exactly one argument");
            }

            return new LispObject[] { result };
        }

        [LispMacro("progn")]
        public IEnumerable<LispObject> ProgN(LispStackFrame frame, LispObject[] args)
        {
            if (args.Length == 0)
            {
                return new LispObject[] { new LispError("Expected at least one expression") };
            }

            return args;
        }

        [LispFunction("apply")]
        public LispObject Apply(LispStackFrame frame, LispObject[] args)
        {
            if (args.Length == 2 &&
                args[0] is LispFunctionReference functionRef &&
                args[1] is LispList functionArguments)
            {
                return FunCall(frame, functionRef, functionArguments.ToList());
            }
            else
            {
                return new LispError("Expected function reference and list of arguments");
            }
        }

        private LispObject FunCall(LispStackFrame frame, LispFunctionReference functionReference, IEnumerable<LispObject> functionArguments)
        {
            string synthesizedFunctionName = null;
            var evaluatingFrame = frame;
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
                var result = evaluatingFrame.Eval(synthesizedFunctionCall);
                postExecute?.Invoke();

                if (!result.SourceLocation.HasValue)
                {
                    result.SourceLocation = functionReference.SourceLocation;
                }

                return result;
            }

            return new LispError("Expected function reference");
        }

        [LispMacro("funcall")]
        public IEnumerable<LispObject> FunCall(LispStackFrame frame, LispObject[] args)
        {
            LispObject result = new LispError("Expected function reference");
            if (args.Length >= 1)
            {
                var candidateFunctionReference = frame.Eval(args[0]);
                if (candidateFunctionReference is LispFunctionReference functionReference)
                {
                    result = FunCall(frame, functionReference, args.Skip(1));
                }
            }

            // the evalutated result is the result of the `funcall` macro, so it has to be quoted to allow it to pass through
            var quotedResult = new LispQuotedObject(result);
            return new LispObject[] { quotedResult };
        }

        [LispFunction("format")]
        public LispObject Format(LispStackFrame frame, LispObject[] args)
        {
            if (args.Length >= 2 &&
                args[1] is LispString s)
            {
                var formatArgs = args.Skip(2);
                if (LispFormatter.TryFormatString(s.Value, formatArgs, out var result))
                {
                    LispStream stream;
                    if (args[0] == frame.T)
                    {
                        // write to terminal
                        stream = frame.TerminalIO;
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
                    return frame.Nil;
                }
                else
                {
                    return new LispError(result);
                }
            }

            return new LispError("Expected output type and string");
        }

        [LispFunction("read")]
        public LispObject Read(LispStackFrame frame, LispObject[] args)
        {
            LispStream readStream;
            LispObject eofMarker = null;
            if (args.Length == 0)
            {
                readStream = frame.TerminalIO;
            }
            else if (args.Length >= 1 && args[0] is LispStream stream)
            {
                readStream = stream;
                if (args.Length == 3 &&
                    args[1].IsNil())
                {
                    eofMarker = args[2];
                }
            }
            else
            {
                return new LispError("Expected a stream");
            }

            var result = readStream.ReadObject(eofMarker);
            return result;
        }

        [LispMacro("with-open-file")]
        public IEnumerable<LispObject> WithOpenFile(LispStackFrame frame, LispObject[] args)
        {
            if (args.Length >= 2 &&
                args[0] is LispList openArguments &&
                openArguments.Length >= 2 &&
                openArguments.Value is LispSymbol streamName &&
                openArguments.Next is LispList filePathList &&
                filePathList.Length >= 1)
            {
                var openArgumentsList = openArguments.ToList();
                var directionArgument = GetKeywordArgument(openArgumentsList.Skip(2), ":direction");
                var fileMode = directionArgument is LispKeyword directionKeyword && directionKeyword.Keyword == ":output"
                    ? FileMode.OpenOrCreate
                    : FileMode.Open;

                var candidateFilePath = frame.Eval(filePathList.Value);
                if (candidateFilePath is LispString filePath)
                {
                    var fileStream = new FileStream(filePath.Value, fileMode);
                    var streamObject = new LispFileStream(filePath.Value, fileStream);
                    var body = args.Skip(1);
                    var result = body.PerformMacroReplacements(new Dictionary<string, LispObject>() { { streamName.Value, streamObject } });
                    var closeExpression = LispList.FromEnumerable(new LispObject[] { new LispSymbol("close"), streamObject }); // (close fileStream)
                    var finalResult = result.Concat(new[] { closeExpression });
                    return finalResult;
                }
                else
                {
                    return new LispObject[] { new LispError("Expected a string file path") };
                }
            }

            return new LispObject[] { new LispError("Expected `<(streamName filePath)> <body>`") };
        }

        [LispFunction("close")]
        public LispObject Close(LispStackFrame frame, LispObject[] args)
        {
            if (args.Length == 1 &&
                args[0] is LispFileStream fileStream)
            {
                fileStream.FileStream.Flush();
                fileStream.FileStream.Dispose();
                return frame.Nil;
            }

            return new LispError("Expected a file stream");
        }

        [LispFunction("dribble")]
        public LispObject Dribble(LispStackFrame frame, LispObject[] args)
        {
            if (args.Length == 0)
            {
                // finish recording
                var dribbleStream = frame.Root.DribbleStream;
                if (dribbleStream is null)
                {
                    return new LispError("Dribble stream object not found");
                }

                frame.TerminalIO.Output.WriteLine($"Finished recording in file {dribbleStream.FileStream.Name}");
                dribbleStream.Output.Flush();
                dribbleStream.Output.Dispose();
                frame.Root.DribbleStream = null;
                return frame.Nil;
            }
            else if (args.Length == 1)
            {
                var filePath = frame.Eval(args[0]);
                if (filePath is LispError error)
                {
                    return error;
                }
                else if (filePath is LispString dribblePath)
                {
                    // start new recording
                    if (frame.Root.DribbleStream is LispFileStream)
                    {
                        return new LispError("Dribble recording already started");
                    }

                    var dribbleStream = new LispFileStream(dribblePath.Value, new FileStream(dribblePath.Value, FileMode.Create));
                    dribbleStream.Output.WriteLine($";Recording in {dribbleStream.FileStream.Name}");
                    dribbleStream.Output.WriteLine($";Recording started at {DateTime.Now:h:mmtt d-MMM-yy}:");
                    dribbleStream.Output.WriteLine();
                    frame.TerminalIO.Output.WriteLine($"Now recording in file {dribbleStream.FileStream.Name}");

                    frame.Root.DribbleStream = dribbleStream;
                    return frame.Nil;
                }
                else
                {
                    return new LispError("Expected a string path to start dribble recording.");
                }
            }

            return new LispError("Expected single file path arugment to start recording, or no arguments to stop");
        }

        [LispMacro("setf")]
        [LispMacro("setq")]
        public IEnumerable<LispObject> SetValue(LispStackFrame frame, LispObject[] args)
        {
            // TODO: properly validate types
            LispObject last = frame.Nil;
            for (int i = 0; i < args.Length - 1; i += 2)
            {
                var destination = args[i];
                var rawValue = args[i + 1];
                var value = frame.Eval(rawValue);
                if (destination is LispSymbol symbol)
                {
                    var name = symbol.Value;
                    frame.SetValueInParentScope(name, value);
                }
                else
                {
                    destination = frame.Eval(destination);
                    if (destination.SetPointerValue != null)
                    {
                        destination.SetPointerValue(value);
                    }
                    else
                    {
                        return new LispObject[] { new LispError("Expected symbol or pointer location") };
                    }
                }

                destination.SetPointerValue = null;
                last = value;
            }

            return new LispObject[] { new LispQuotedObject(last) };
        }

        [LispFunction("numberp")]
        public LispObject NumberP(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispInteger _:
                case LispFloat _:
                case LispRatio _:
                    return frame.T;
                default:
                    return frame.Nil;
            }
        }

        [LispFunction("stringp")]
        public LispObject StringP(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispString _:
                    return frame.T;
                default:
                    return frame.Nil;
            }
        }

        [LispFunction("keywordp")]
        public LispObject KeywordP(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispKeyword _:
                    return frame.T;
                default:
                    return frame.Nil;
            }
        }

        [LispFunction("symbolp")]
        public LispObject SymbolP(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispKeyword _:
                case LispSymbol _:
                    return frame.T;
                default:
                    return frame.Nil;
            }
        }

        [LispFunction("zerop")]
        public LispObject ZeroP(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispInteger i when i.IsZero:
                    return frame.T;
                case LispFloat f when f.IsZero:
                    return frame.T;
                case LispRatio r when r.IsZero:
                    return frame.T;
                default:
                    return frame.Nil;
            }
        }

        [LispFunction("plusp")]
        public LispObject PlusP(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument count
            if (args[0] is LispNumber num)
            {
                switch (num)
                {
                    case LispInteger i when i.Value > 0:
                        return frame.T;
                    case LispFloat f when f.Value > 0.0:
                        return frame.T;
                    case LispRatio r when !r.IsZero && Math.Sign(r.Numerator) == Math.Sign(r.Denominator):
                        return frame.T;
                    default:
                        return frame.Nil;
                }
            }

            return new LispError("wrong type input");
        }

        [LispFunction("evenp")]
        public LispObject EvenP(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispInteger num when num.IsEven:
                    return frame.T;
                case LispFloat num when num.IsEven:
                    return frame.T;
                default:
                    return frame.Nil;
            }
        }

        [LispFunction("oddp")]
        public LispObject OddP(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispInteger num when num.IsOdd:
                    return frame.T;
                case LispFloat num when num.IsOdd:
                    return frame.T;
                default:
                    return frame.Nil;
            }
        }

        [LispFunction("listp")]
        public LispObject ListP(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate single argument
            return args[0] is LispList
                ? frame.T
                : frame.Nil;
        }

        [LispFunction("consp")]
        public LispObject ConsP(LispStackFrame frame, LispObject[] args)
        {
            return args[0] is LispList list && !list.IsNil()
                ? frame.T
                : frame.Nil;
        }

        [LispFunction("streamp")]
        public LispObject StreamP(LispStackFrame frame, LispObject[] args)
        {
            return args.Length == 1 && args[0] is LispStream
                ? frame.T
                : frame.Nil;
        }

        [LispMacro("quote")]
        public IEnumerable<LispObject> Quote(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument count
            return new LispObject[] { new LispQuotedObject(args[0]) };
        }

        [LispMacro("function")]
        public IEnumerable<LispObject> Function(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument count
            if (args[0] is LispSymbol symbol)
            {
                return new LispObject[] { new LispQuotedNamedFunctionReference(symbol.Value) };
            }
            else
            {
                return new LispObject[] { new LispError("Expected a function symbol") };
            }
        }

        [LispFunction("cons")]
        public LispObject Cons(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate arguments
            return new LispList(args[0], args[1]);
        }

        [LispFunction("list")]
        public LispObject List(LispStackFrame frame, LispObject[] args)
        {
            return LispList.FromEnumerable(args);
        }

        [LispFunction("length")]
        public LispObject Length(LispStackFrame frame, LispObject[] args)
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

        [LispFunction("car")]
        [LispFunction("first")]
        public LispObject First(LispStackFrame frame, LispObject[] args)
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

        [LispFunction("cdr")]
        [LispFunction("rest")]
        public LispObject Rest(LispStackFrame frame, LispObject[] args)
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

        [LispFunction("append")]
        public LispObject Append(LispStackFrame frame, LispObject[] args)
        {
            if (args.Length != 2)
            {
                return new LispError("Expected exactly 2 arguments");
            }

            if (args[0] is LispList list)
            {
                var items = list.ToList();
                var last = args[1];
                foreach (var item in items.Reverse())
                {
                    var next = new LispList(item, last);
                    last = next;
                }

                return last;
            }
            else
            {
                return new LispError("First argument must be a list");
            }
        }

        [LispFunction("reverse")]
        public LispObject Reverse(LispStackFrame frame, LispObject[] args)
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

        [LispFunction("intersection")]
        public LispObject Intersection(LispStackFrame frame, LispObject[] args)
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

        [LispFunction("union")]
        public LispObject Union(LispStackFrame frame, LispObject[] args)
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

        [LispFunction("set-difference")]
        public LispObject SetDifference(LispStackFrame frame, LispObject[] args)
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

        [LispFunction("set-exclusive-or")]
        public LispObject SetExclusiveOr(LispStackFrame frame, LispObject[] args)
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

        [LispFunction("remove")]
        public LispObject Remove(LispStackFrame frame, LispObject[] args)
        {
            // TOOD: validate arguments
            var key = args[0];
            var list = ((LispList)args[1]).ToList();
            var result = new List<LispObject>();
            var count = GetKeywordArgument(args.Skip(2), ":count");
            var limit = count is LispInteger number
                ? number.Value
                : list.Count;
            var fromEndArg = GetKeywordArgument(args.Skip(2), ":from-end");
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

        [LispFunction("remove-duplicates")]
        public LispObject RemoveDuplicates(LispStackFrame frame, LispObject[] args)
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

        [LispFunction("find-if")]
        public LispObject FindIf(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument counts
            if (args.Length >= 2 &&
                args[0] is LispFunctionReference functionReference &&
                args[1] is LispList inputList)
            {
                var fromEndKeyword = GetKeywordArgument(args.Skip(2), ":from-end");
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
                        new LispQuotedObject(item)
                    };
                    var result = FunCall(frame, functionReference, functionArguments);
                    if (result is LispError)
                    {
                        return result;
                    }

                    if (result.IsTLike())
                    {
                        return item;
                    }
                }

                return frame.Nil;
            }
            else
            {
                return new LispError("Expected a function reference and list");
            }
        }

        [LispFunction("remove-if")]
        public LispObject RemoveIf(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument counts
            if (args.Length >= 2 &&
                args[0] is LispFunctionReference functionReference &&
                args[1] is LispList inputList)
            {
                var countArgument = GetKeywordArgument(args.Skip(2), ":count");
                var count = countArgument is LispInteger i
                    ? i.Value
                    : int.MaxValue;
                var items = inputList.ToList();
                var resultItems = new List<LispObject>();
                var removed = 0;
                foreach (var item in items)
                {
                    var result = FunCall(frame, functionReference, new LispObject[] { item });
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

        [LispFunction("remove-if-not")]
        public LispObject RemoveIfNot(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument counts
            if (args.Length >= 2 &&
                args[0] is LispFunctionReference functionReference &&
                args[1] is LispList inputList)
            {
                var countArgument = GetKeywordArgument(args.Skip(2), ":count");
                var count = countArgument is LispInteger i
                    ? i.Value
                    : int.MaxValue;
                var items = inputList.ToList();
                var resultItems = new List<LispObject>();
                var removed = 0;
                foreach (var item in items)
                {
                    var result = FunCall(frame, functionReference, new LispObject[] { item });
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

        [LispFunction("reduce")]
        public LispObject Reduce(LispStackFrame frame, LispObject[] args)
        {
            // TODO: validate argument counts
            if (args.Length >= 2 &&
                args[0] is LispFunctionReference functionReference &&
                args[1] is LispList inputList)
            {
                var fromEndKeyword = GetKeywordArgument(args.Skip(2), ":from-end");
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
                    var result = FunCall(frame, functionReference, new LispObject[] { new LispQuotedObject(arg1), new LispQuotedObject(arg2) });
                    if (result is LispError)
                    {
                        return result;
                    }

                    items.Insert(0, result);
                }

                if (items.Count == 0)
                {
                    return frame.Nil;
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

        [LispFunction("every")]
        public LispObject Every(LispStackFrame frame, LispObject[] args)
        {
            var result = MapCar(frame, args);
            switch (result)
            {
                case LispList list when list.ToList().Any(o => o.IsNil()):
                    return frame.Nil;
                default:
                    return frame.T;
            }
        }

        [LispFunction("mapcar")]
        public LispObject MapCar(LispStackFrame frame, LispObject[] args)
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
                    case LispMacroOrFunction directlyInvocable:
                        evaluator = (functionArguments) =>
                        {
                            var manualInvokeItems = new List<LispObject>() { directlyInvocable };
                            manualInvokeItems.AddRange(functionArguments);
                            var manualInvokeList = LispList.FromEnumerable(manualInvokeItems);
                            return frame.Eval(manualInvokeList);
                        };
                        break;
                    case LispFunctionReference functionRef:
                        evaluator = (functionArguments) => FunCall(frame, functionRef, functionArguments);
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
        public LispObject LessThan(LispStackFrame frame, LispObject[] args)
        {
            return FoldComparison(frame, args, (a, b) => LispNumber.LessThan(a, b));
        }

        [LispFunction("<=")]
        public LispObject LessThanOrEqual(LispStackFrame frame, LispObject[] args)
        {
            return FoldComparison(frame, args, (a, b) => LispNumber.LessThanOrEqual(a, b));
        }

        [LispFunction(">")]
        public LispObject GreaterThan(LispStackFrame frame, LispObject[] args)
        {
            return FoldComparison(frame, args, (a, b) => LispNumber.GreaterThan(a, b));
        }

        [LispFunction(">=")]
        public LispObject GreaterThanOrEqual(LispStackFrame frame, LispObject[] args)
        {
            return FoldComparison(frame, args, (a, b) => LispNumber.GreaterThanOrEqual(a, b));
        }

        [LispFunction("=")]
        public LispObject NumberEqual(LispStackFrame frame, LispObject[] args)
        {
            return FoldComparison(frame, args, (a, b) => LispNumber.Equal(a, b));
        }

        [LispFunction("equal")]
        public LispObject Equal(LispStackFrame frame, LispObject[] args)
        {
            return FoldObj(frame, args, (a, b) => a.Equals(b));
        }

        [LispFunction("equalp")]
        public LispObject EqualP(LispStackFrame frame, LispObject[] args)
        {
            return FoldObj(frame, args, (a, b) =>
            {
                if (a is LispString sa && b is LispString sb)
                {
                    return string.Compare(sa.Value, sb.Value, StringComparison.OrdinalIgnoreCase) == 0;
                }

                return a.Equals(b);
            });
        }

        [LispFunction("eq")]
        public LispObject Eq(LispStackFrame frame, LispObject[] args)
        {
            return FoldObj(frame, args, (a, b) => ReferenceEquals(a, b));
        }

        [LispFunction("eql")]
        public LispObject Eql(LispStackFrame frame, LispObject[] args)
        {
            return FoldObj(frame, args, (a, b) =>
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

                return ReferenceEquals(a, b);
            });
        }

        [LispFunction("!=")]
        [LispFunction("<>")]
        public LispObject NotEqual(LispStackFrame frame, LispObject[] args)
        {
            return FoldObj(frame, args, (a, b) => !a.Equals(b));
        }

        [LispMacro("and")]
        public IEnumerable<LispObject> And(LispStackFrame frame, LispObject[] args)
        {
            return FoldBoolean(frame, args, true, false, (a, b) => a && b);
        }

        [LispMacro("or")]
        public IEnumerable<LispObject> Or(LispStackFrame frame, LispObject[] args)
        {
            return FoldBoolean(frame, args, true, true, (a, b) => a || b);
        }

        [LispMacro("cond")]
        public IEnumerable<LispObject> Cond(LispStackFrame frame, LispObject[] args)
        {
            foreach (var arg in args)
            {
                if (arg is LispList list && list.Length == 2)
                {
                    var values = list.ToList();
                    var predicate = frame.Eval(values[0]);
                    switch (predicate)
                    {
                        case LispError error:
                            return new LispObject[] { error };
                        case LispNilList _:
                            break;
                        default:
                            return new LispObject[] { values[1] };
                    }
                }
                else
                {
                    return new LispObject[]
                    {
                        new LispError("Expected list of length 2")
                        {
                            SourceLocation = arg.SourceLocation,
                        }
                    };
                }
            }

            return new LispObject[] { frame.Nil };
        }

        [LispFunction("abs")]
        public LispObject Abs(LispStackFrame frame, LispObject[] args)
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

        [LispFunction("sqrt")]
        public LispObject Sqrt(LispStackFrame frame, LispObject[] args)
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

        [LispFunction("kernel:two-arg-+")]
        public LispObject TwoAgumentPlus(LispStackFrame frame, LispObject[] args)
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

        [LispFunction("kernel:one-arg--")]
        public LispObject OneArgumentMinus(LispStackFrame frame, LispObject[] args)
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

        [LispFunction("kernel:two-arg--")]
        public LispObject TwoArgumentMinus(LispStackFrame frame, LispObject[] args)
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

        [LispFunction("kernel:two-arg-*")]
        public LispObject TwoArgumentAsterisk(LispStackFrame frame, LispObject[] args)
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

        [LispFunction("kernel:two-arg-/")]
        public LispObject TwoArgumentSlash(LispStackFrame frame, LispObject[] args)
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

        private static IEnumerable<LispObject> FoldBoolean(LispStackFrame frame, LispObject[] args, bool init, bool shortCircuitValue, Func<bool, bool, bool> operation)
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
                    var evaluated = frame.Eval(value);
                    if (evaluated is LispError error)
                    {
                        return new LispObject[] { error };
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
                result = collected ? frame.T : frame.Nil;
            }

            return new LispObject[] { result };
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
