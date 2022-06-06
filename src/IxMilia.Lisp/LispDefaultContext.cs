using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace IxMilia.Lisp
{
    public class LispDefaultContext
    {
        [LispFunction("ERROR", Documentation = "Raise an error.")]
        public async Task<LispObject> Error(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length >= 1)
            {
                var formatArgs = new List<LispObject>();
                formatArgs.Add(host.Nil); // force raw string generation
                formatArgs.AddRange(args);
                var candidateErrorString = await Format(host, executionState, formatArgs.ToArray(), cancellationToken);
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
        public async Task<LispObject> Break(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length >= 1)
            {
                var formatArgs = new List<LispObject>();
                formatArgs.Add(host.Nil); // force raw string generation
                formatArgs.AddRange(args);
                var candidateErrorString = await Format(host, executionState, formatArgs.ToArray(), cancellationToken);
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

        [LispMacro("DEFMACRO", Documentation = "Defines a macro.")]
        public Task<LispObject> DefineMacro(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (!TryGetCodeMacroFromItems(args, host.CurrentPackage, out var codeMacro, out var error))
            {
                return Task.FromResult<LispObject>(error);
            }

            executionState.StackFrame.SetValueInParentScope(codeMacro.NameSymbol, codeMacro);
            return Task.FromResult(host.Nil);
        }

        internal static bool TryGetCodeMacroFromItems(LispObject[] items, LispPackage currentPackage, out LispCodeMacro codeMacro, out LispError error)
        {
            codeMacro = default;

            if (items.Length >= 2 &&
                items[0] is LispSymbol symbol &&
                items[1] is LispList arguments)
            {
                var macroNameSymbol = symbol.Resolve(currentPackage);
                var macroArgs = arguments.ToList();
                if (!LispArgumentCollection.TryBuildArgumentCollection(macroArgs.ToArray(), out var argumentCollection, out error))
                {
                    return false;
                }

                ExtractDocumentationString(items.Skip(2), out var macroBody, out var documentation);
                codeMacro = new LispCodeMacro(macroNameSymbol, documentation, argumentCollection, macroBody)
                {
                    SourceLocation = macroNameSymbol.SourceLocation,
                };

                return true;
            }

            codeMacro = default;
            error = default;
            return false;
        }

        [LispMacro("DEFUN", Documentation = "Defines a function.")]
        public Task<LispObject> DefineFunction(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (!TryGetCodeFunctionFromItems(args, host.CurrentPackage, out var codeFunction, out var error))
            {
                return Task.FromResult<LispObject>(error);
            }

            codeFunction.SourceLocation = executionState.StackFrame.SourceLocation;
            executionState.StackFrame.SetValueInParentScope(codeFunction.NameSymbol, codeFunction);
            return Task.FromResult(host.Nil);
        }

        [LispMacro("LAMBDA", Signature = "ARGUMENT-LIST &REST BODY", Documentation = "Defines a lambda function.")]
        public Task<LispObject> Lambda(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: validate shape
            var lambdaName = $"(LAMBDA-{args[0].SourceLocation?.Start.Line}-{args[0].SourceLocation?.Start.Column})"; // surrounded by parens to make it un-utterable
            var lambdaItems = new List<LispObject>();
            lambdaItems.Add(new LispResolvedSymbol(host.CurrentPackage.Name, lambdaName, isPublic: true));
            lambdaItems.AddRange(args);

            if (!TryGetCodeFunctionFromItems(lambdaItems.ToArray(), host.CurrentPackage, out var codeFunction, out var error))
            {
                return Task.FromResult<LispObject>(error);
            }

            codeFunction.SourceLocation = executionState.StackFrame.SourceLocation;
            var result = new LispQuotedLambdaFunctionReference(codeFunction);
            result.SourceLocation = codeFunction.SourceLocation;
            return Task.FromResult<LispObject>(result);
        }

        internal static bool TryGetCodeFunctionFromItems(LispObject[] items, LispPackage currentPackage, out LispCodeFunction codeFunction, out LispError error)
        {
            codeFunction = default;

            if (items.Length >= 2 &&
                items[0] is LispSymbol symbol &&
                items[1] is LispList arguments)
            {
                var name = symbol.Resolve(currentPackage).Value;
                var argumentList = arguments.ToList();
                if (!LispArgumentCollection.TryBuildArgumentCollection(argumentList.ToArray(), out var argumentCollection, out error))
                {
                    return false;
                }

                ExtractDocumentationString(items.Skip(2), out var commands, out var documentation);
                var nameSymbol = LispSymbol.CreateFromString(name).Resolve(currentPackage);
                codeFunction = new LispCodeFunction(nameSymbol, documentation, argumentCollection, commands);
                return true;
            }

            codeFunction = default;
            error = default;
            return false;
        }

        internal static void ExtractDocumentationString(IEnumerable<LispObject> bodyObjects, out IEnumerable<LispObject> resultBody, out string documentation)
        {
            documentation = null;
            resultBody = bodyObjects;
            if (bodyObjects.Count() > 0 && bodyObjects.First() is LispString str)
            {
                documentation = NormalizeDocumentationString(str);
                resultBody = bodyObjects.Skip(1);
            }
        }

        internal static string NormalizeDocumentationString(LispString s)
        {
            var documentation = s.Value;
            if (s.SourceLocation.HasValue)
            {
                var documentationIndent = s.SourceLocation.Value.Start.Column;
                var indentValue = new string(' ', documentationIndent);
                var indentValueOneLess = new string(' ', Math.Max(documentationIndent - 1, 0));
                var documentationLines = documentation.Split('\n');
                if (documentationLines.Length > 1)
                {
                    var normalizedLines = new List<string>() { documentationLines[0] };
                    var canBeNormalized = true;
                    foreach (var line in documentationLines.Skip(1))
                    {
                        if (string.IsNullOrWhiteSpace(line))
                        {
                            normalizedLines.Add(string.Empty);
                        }
                        else if (line.StartsWith(indentValue))
                        {
                            normalizedLines.Add(line.Substring(indentValue.Length));
                        }
                        else if (line.StartsWith(indentValueOneLess))
                        {
                            normalizedLines.Add(line.Substring(indentValueOneLess.Length));
                        }
                        else
                        {
                            canBeNormalized = false;
                            break;
                        }
                    }

                    if (canBeNormalized)
                    {
                        documentation = string.Join("\n", normalizedLines);
                    }
                }
            }

            return documentation;
        }

        [LispMacro("DEFVAR")]
        public Task<LispObject> DefineVariable(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: properly validage single symbol argument
            var symbol = ((LispSymbol)args[0]).Resolve(host.CurrentPackage);
            executionState.StackFrame.SetValueInParentScope(symbol, symbol);
            return Task.FromResult<LispObject>(symbol);
        }

        [LispMacro("LET")]
        public Task<LispObject> Let(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            return Task.FromResult(Let(args, bindSequentially: false));
        }

        [LispMacro("LET*")]
        public Task<LispObject> LetStar(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            return Task.FromResult(Let(args, bindSequentially: true));
        }

        internal static LispObject Let(LispObject[] args, bool bindSequentially)
        {
            var values = ((LispList)args[0]).ToList();
            var body = args.Skip(1);

            // possibly convert values into `(setf value-name (computed-value))`
            var valueSetters = new List<LispObject>();
            var functionParameters = new List<LispObject>();
            var functionArguments = new List<LispObject>()
            {
                new LispUnresolvedSymbol("LIST"),
            };
            foreach (var valuePair in values)
            {
                // TODO: validate shape
                var valuePairList = (LispList)valuePair;
                var varName = (LispSymbol)valuePairList.Value;
                var varRawValue = ((LispList)valuePairList.Next).Value;
                if (bindSequentially)
                {
                    var setter = LispList.FromItems(
                        new LispUnresolvedSymbol("SETF"),
                        varName,
                        varRawValue);
                    valueSetters.Add(setter);
                }
                else
                {
                    functionParameters.Add(varName);
                    functionArguments.Add(LispList.FromItems(new LispUnresolvedSymbol("QUOTE"), varRawValue));
                }
            }

            var fullBody = valueSetters.Concat(body).ToList();
            var lambdaFullForm = new List<LispObject>();
            lambdaFullForm.Add(new LispUnresolvedSymbol("LAMBDA"));
            lambdaFullForm.Add(LispList.FromEnumerable(functionParameters));
            lambdaFullForm.AddRange(fullBody);

            var applyFullForm = new List<LispObject>();
            applyFullForm.Add(new LispUnresolvedSymbol("APPLY"));
            applyFullForm.Add(LispList.FromEnumerable(lambdaFullForm));
            applyFullForm.Add(LispList.FromEnumerable(functionArguments));

            var result = LispList.FromEnumerable(applyFullForm);
            return result;
        }

        [LispMacro("LABELS")]
        public Task<LispObject> Labels(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: validate arguments
            var functionDefinitions = ((LispList)args[0]).ToList();
            var replacements = new Dictionary<string, LispObject>();
            var body = args.Skip(1);
            foreach (var functionDefinitionSet in functionDefinitions)
            {
                // TODO: validate shape
                var functionDefinition = ((LispList)functionDefinitionSet).ToList();
                if (!TryGetCodeFunctionFromItems(functionDefinition.ToArray(), host.CurrentPackage, out var codeFunction, out var error))
                {
                    return Task.FromResult<LispObject>(error);
                }

                var replacedCodeFunction = (LispCodeFunction)codeFunction.PerformMacroReplacements(host.CurrentPackage, replacements);
                replacedCodeFunction.SourceLocation = functionDefinitionSet.SourceLocation;
                replacements[codeFunction.NameSymbol.Value] = replacedCodeFunction;

                // ensure recursive function calls get updated
                var selfReplacement = new Dictionary<string, LispObject>()
                {
                    { replacedCodeFunction.NameSymbol.Value, replacedCodeFunction }
                };
                replacedCodeFunction.Commands = replacedCodeFunction.Commands.PerformMacroReplacements(host.CurrentPackage, selfReplacement).ToList().ToArray();
            }

            var result = body.PerformMacroReplacements(host.CurrentPackage, replacements);
            return Task.FromResult<LispObject>(result);
        }

        [LispMacro("DEFPACKAGE")]
        public Task<LispObject> DefPackage(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length >= 1)
            {
                var packageName = PackageNameFromValue(args[0]);
                if (packageName != null)
                {
                    var inheritedPackageNames = new List<string>()
                    {
                        "COMMON-LISP"
                    };
                    foreach (var arg in args.Skip(1))
                    {
                        if (arg is LispList list)
                        {
                            switch (list.Value)
                            {
                                case LispResolvedSymbol s when s.IsKeyword:
                                    switch (s.LocalName)
                                    {
                                        case "USE":
                                            foreach (var candidatePackageName in list.ToList().Skip(1))
                                            {
                                                var inheritedPackageName = PackageNameFromValue(candidatePackageName);
                                                if (inheritedPackageName == null)
                                                {
                                                    return Task.FromResult<LispObject>(new LispError("Expected either a string or a keyword symbol")
                                                    {
                                                        SourceLocation = candidatePackageName.SourceLocation
                                                    });
                                                }

                                                inheritedPackageNames.Add(inheritedPackageName);
                                            }
                                            break;
                                        default:
                                            return Task.FromResult<LispObject>(new LispError($"Unexpected keyword directive {s.LocalName}")
                                            {
                                                SourceLocation = s.SourceLocation
                                            });
                                    }
                                    break;
                                default:
                                    return Task.FromResult<LispObject>(new LispError("Expected a keyword")
                                    {
                                        SourceLocation = list.Value.SourceLocation
                                    });
                            }
                        }
                        else
                        {
                            return Task.FromResult<LispObject>(new LispError("Expected a keyword directive list")
                            {
                                SourceLocation = arg.SourceLocation
                            });
                        }
                    }

                    var inheritedPackages = inheritedPackageNames.Select(p => host.RootFrame.GetPackage(p)).ToList();
                    var package = host.RootFrame.AddPackage(packageName, inheritedPackages);
                    return Task.FromResult<LispObject>(package);
                }
            }

            return Task.FromResult<LispObject>(new LispError("Expected either a string or a keyword symbol"));
        }

        [LispFunction("IN-PACKAGE")]
        public Task<LispObject> InPackage(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 1)
            {
                var packageName = PackageNameFromValue(args[0]);
                if (packageName != null)
                {
                    var package = host.RootFrame.GetPackage(packageName);
                    host.CurrentPackage = package;
                    return Task.FromResult<LispObject>(package);
                }
            }

            return Task.FromResult<LispObject>(new LispError("Expected either a string or a keyword symbol."));
        }

        private static string PackageNameFromValue(LispObject obj)
        {
            return obj switch
            {
                LispString s => s.Value,
                LispResolvedSymbol s when s.IsKeyword => s.LocalName,
                _ => null
            };
        }

        [LispMacro("EVAL")]
        public async Task<LispObject> Eval(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            LispObject result;
            if (args.Length == 1)
            {
                result = await host.EvalAtStackFrameAsync(executionState.StackFrame, args[0], cancellationToken);
            }
            else
            {
                result = new LispError("Expected exactly one argument");
            }

            return result;
        }

        [LispFunction("APPLY", Signature = "FUNCTION-REFERENCE FUNCTION-ARGUMENTS", Documentation = "Applies a function to a list of arguments.")]
        public async Task<LispObject> Apply(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 2 &&
                args[0] is LispFunctionReference functionRef &&
                args[1] is LispList functionArguments)
            {
                return await FunCallAsync(host, executionState.StackFrame, functionRef, functionArguments.ToList(), cancellationToken);
            }
            else
            {
                return new LispError("Expected function reference and list of arguments");
            }
        }

        internal static async Task<LispObject> FunCallAsync(LispHost host, LispStackFrame evaluatingFrame, LispFunctionReference functionReference, IEnumerable<LispObject> functionArguments, CancellationToken cancellationToken)
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
                synthesizedFunctionName = lambdaFunction.Definition.NameSymbol.Value;
                evaluatingFrame = lambdaFunction.StackFrame;
                preExecute = () => evaluatingFrame.SetValue(lambdaFunction.Definition.NameSymbol, lambdaFunction.Definition);
                postExecute = () => evaluatingFrame.DeleteValue(lambdaFunction.Definition.NameSymbol);
            }

            if (synthesizedFunctionName != null)
            {
                var synthesizedSymbol = LispSymbol.CreateFromString(synthesizedFunctionName);
                synthesizedSymbol.SourceLocation = functionReference.SourceLocation;
                var synthesizedFunctionItems = new List<LispObject>();
                synthesizedFunctionItems.Add(synthesizedSymbol);
                synthesizedFunctionItems.AddRange(functionArguments);
                var synthesizedFunctionCall = LispList.FromEnumerable(synthesizedFunctionItems);
                synthesizedFunctionCall.SourceLocation = functionReference.SourceLocation;

                preExecute?.Invoke();
                var result = await host.EvalAtStackFrameAsync(evaluatingFrame, synthesizedFunctionCall, cancellationToken);
                postExecute?.Invoke();

                return result;
            }

            return new LispError("Expected function reference");
        }

        [LispMacro("FUNCALL")]
        public async Task<LispObject> FunCallAsync(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            LispObject result = new LispError("Expected function reference");
            if (args.Length >= 1)
            {
                var candidateFunctionReference = await host.EvalAtStackFrameAsync(executionState.StackFrame, args[0], cancellationToken);
                if (candidateFunctionReference is LispFunctionReference functionReference)
                {
                    result = await FunCallAsync(host, executionState.StackFrame, functionReference, args.Skip(1), cancellationToken);
                }
            }

            // the evalutated result is the result of the `funcall` macro, so it has to be quoted to allow it to pass through
            var quotedResult = LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:QUOTE"), result);
            return quotedResult;
        }

        [LispFunction("MAKE-STRING-INPUT-STREAM")]
        public Task<LispObject> MakeStringInputStream(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
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
                var inputTextStream = new LispTextStream("", input, TextWriter.Null);
                return Task.FromResult<LispObject>(inputTextStream);
            }
            else
            {
                return Task.FromResult<LispObject>(new LispError("Expected an input string"));
            }
        }

        [LispFunction("MAKE-STRING-OUTPUT-STREAM")]
        public Task<LispObject> MakeStringOutputStream(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: actually honor this
            var _elementType = GetKeywordArgument(args, ":ELEMENT-TYPE");
            var outputTextStream = new LispTextStream("", TextReader.Null, TextWriter.Null);
            return Task.FromResult<LispObject>(outputTextStream);
        }

        [LispFunction("GET-OUTPUT-STREAM-STRING")]
        public Task<LispObject> GetOutputStreamString(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 1 &&
                args[0] is LispTextStream outputStream)
            {
                var result = outputStream.GetOutputContents();
                return Task.FromResult<LispObject>(new LispString(result));
            }

            return Task.FromResult<LispObject>(new LispError("Expected exactly one text stream"));
        }

        [LispFunction("FORMAT")]
        public Task<LispObject> Format(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length >= 2 &&
                args[1] is LispString s)
            {
                var formatArgs = args.Skip(2);
                if (LispFormatter.TryFormatString(s.Value, formatArgs, out var result))
                {
                    LispTextStream stream;
                    if (args[0] == host.T)
                    {
                        // write to terminal
                        stream = host.TerminalIO;
                    }
                    else if (args[0].IsNil())
                    {
                        // return formatted string
                        return Task.FromResult<LispObject>(new LispString(result));
                    }
                    else if (args[0] is LispTextStream suppliedStream)
                    {
                        stream = suppliedStream;
                    }
                    else
                    {
                        return Task.FromResult<LispObject>(new LispError("Unsupported output stream"));
                    }

                    stream.Output.Write(result);
                    stream.Output.Flush();
                    return Task.FromResult(host.Nil);
                }
                else
                {
                    return Task.FromResult<LispObject>(new LispError(result));
                }
            }

            return Task.FromResult<LispObject>(new LispError("Expected output type and string"));
        }

        [LispFunction("CODE-CHAR")]
        public Task<LispObject> CodeChar(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 1 &&
                args[0] is LispInteger i)
            {
                var c = (char)i.Value;
                return Task.FromResult<LispObject>(new LispCharacter(c));
            }

            return Task.FromResult<LispObject>(new LispError("Expected an integer"));
        }

        [LispFunction("CHAR-CODE")]
        public Task<LispObject> CharCode(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 1 &&
                args[0] is LispCharacter c)
            {
                var i = (int)c.Value;
                return Task.FromResult<LispObject>(new LispInteger(i));
            }

            return Task.FromResult<LispObject>(new LispError("Expected a character"));
        }

        [LispFunction("CHAR=")]
        public Task<LispObject> CharEquals(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 2 &&
                args[0] is LispCharacter c1 &&
                args[1] is LispCharacter c2)
            {
                return Task.FromResult(c1.Value == c2.Value
                    ? host.T
                    : host.Nil);
            }

            return Task.FromResult<LispObject>(new LispError("Expected 2 characters"));
        }

        [LispFunction("PEEK-CHAR")]
        public Task<LispObject> PeekChar(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            var peekType = host.Nil;
            var inputTextStream = host.TerminalIO;
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

                if (args[0 + argumentOffset] is LispTextStream stream)
                {
                    inputTextStream = stream;
                }
                else
                {
                    return Task.FromResult<LispObject>(new LispError("Expected an input stream"));
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
                                return Task.FromResult<LispObject>(new LispError("Too many arguments"));
                            }
                        }
                    }
                }
            }

            return Task.FromResult(PeekChar(peekType, inputTextStream, errorOnEof, eofValue, _isRecursive));
        }

        [LispFunction("READ-CHAR")]
        public Task<LispObject> ReadChar(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            var inputTextStream = host.TerminalIO;
            var errorOnEof = true;
            var eofValue = host.Nil;
            var _isRecursive = false;
            if (args.Length >= 1)
            {
                if (args[0] is LispTextStream stream)
                {
                    inputTextStream = stream;
                }
                else
                {
                    return Task.FromResult<LispObject>(new LispError("Expected an input stream"));
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
                                return Task.FromResult<LispObject>(new LispError("Too many arguments"));
                            }
                        }
                    }
                }
            }

            return Task.FromResult(ReadChar(inputTextStream, errorOnEof, eofValue, _isRecursive));
        }

        internal static LispObject PeekChar(LispObject peekType, LispTextStream inputTextStream, bool errorOnEof, LispObject eofValue, bool isRecursive)
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

            char peekedChar;
            while (true)
            {
                if (inputTextStream.Peek() is LispCharacter lc)
                {
                    peekedChar = lc.Value;
                    if (!shouldConsumeAndSkip(peekedChar))
                    {
                        break;
                    }

                    // swallow it
                    inputTextStream.Read();
                }
                else
                {
                    if (errorOnEof)
                    {
                        return new LispError("EOF");
                    }
                    else
                    {
                        return eofValue;
                    }
                }
            }

            return new LispCharacter(peekedChar);
        }

        internal static LispObject ReadChar(LispTextStream inputTextStream, bool errorOnEof, LispObject eofValue, bool isRecursive)
        {
            if (inputTextStream.Read() is LispCharacter lc)
            {
                return lc;
            }

            if (errorOnEof)
            {
                return new LispError("EOF");
            }
            else
            {
                return eofValue;
            }
        }

        [LispFunction("WRITE-CHAR")]
        public Task<LispObject> WriteChar(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length >= 1 &&
                args[0] is LispCharacter lc)
            {
                var outputTextStream = host.TerminalIO;
                if (args.Length == 2 &&
                    args[1] is LispTextStream output)
                {
                    outputTextStream = output;
                }

                outputTextStream.Output.Write(lc.Value);
                return Task.FromResult<LispObject>(lc);
            }

            return Task.FromResult<LispObject>(new LispError("Expected a character and an optional stream"));
        }

        [LispFunction("READ")]
        public async Task<LispObject> Read(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            var inputTextStream = host.TerminalIO;
            var errorOnEof = true;
            var eofValue = host.Nil;
            var isRecursive = false;
            if (args.Length >= 1)
            {
                if (args[0] is LispTextStream stream)
                {
                    inputTextStream = stream;
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
            try
            {
                host.ObjectReader.PushReaderStream(inputTextStream);
                var readerResult = await host.ObjectReader.ReadAsync(executionState.StackFrame, errorOnEof, eofValue, isRecursive, cancellationToken);
                result = readerResult.LastResult;
            }
            finally
            {
                host.ObjectReader.PopReaderStream();
            }

            return result;
        }

        [LispMacro("WITH-OPEN-FILE")]
        public async Task<LispObject> WithOpenFile(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
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
                var fileMode = directionArgument is LispResolvedSymbol symbol && symbol.IsKeyword && symbol.Value == ":OUTPUT"
                    ? FileMode.OpenOrCreate
                    : FileMode.Open;

                var candidateFilePath = await host.EvalAtStackFrameAsync(executionState.StackFrame, filePathList.Value, cancellationToken);
                if (candidateFilePath is LispString filePath)
                {
                    var resolvedStreamName = streamName.Resolve(host.CurrentPackage);
                    var fileStream = new FileStream(filePath.Value, fileMode);
                    var streamObject = new LispFileStream(filePath.Value, fileStream);
                    var body = args.Skip(1);
                    var result = body.PerformMacroReplacements(host.CurrentPackage, new Dictionary<string, LispObject>() { { resolvedStreamName.Value, streamObject } });
                    var closeExpression = LispList.FromEnumerable(new LispObject[] { LispSymbol.CreateFromString("COMMON-LISP:CLOSE"), streamObject }); // (close fileStream)
                    var finalResult = new LispList(LispSymbol.CreateFromString("COMMON-LISP:PROGN"), new LispList(result, new LispList(closeExpression)));
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
        public Task<LispObject> Close(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 1 &&
                args[0] is LispFileStream fileStream)
            {
                fileStream.FileStream.Flush();
                fileStream.FileStream.Dispose();
                return Task.FromResult(host.Nil);
            }

            return Task.FromResult<LispObject>(new LispError("Expected a file stream"));
        }

        [LispFunction("DRIBBLE")]
        public async Task<LispObject> Dribble(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
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
                var filePath = await host.EvalAtStackFrameAsync(executionState.StackFrame, args[0], cancellationToken);
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
        public async Task<LispObject> SetValue(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: properly validate types
            LispObject last = host.Nil;
            for (int i = 0; i < args.Length - 1; i += 2)
            {
                var destination = args[i];
                var rawValue = args[i + 1];
                var value = await host.EvalAtStackFrameAsync(executionState.StackFrame, rawValue, cancellationToken);
                if (destination is LispSymbol symbol)
                {
                    var resolvedSymbol = symbol.Resolve(host.CurrentPackage);
                    executionState.StackFrame.SetValueInParentScope(resolvedSymbol, value);
                }
                else
                {
                    destination = await host.EvalAtStackFrameAsync(executionState.StackFrame, destination, cancellationToken);
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

            return LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:QUOTE"), last);
        }

        [LispFunction("VECTOR")]
        public Task<LispObject> Vector(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            return Task.FromResult<LispObject>(LispVector.CreateFixed(args));
        }

        [LispFunction("VECTOR-POP", Signature = "THE-VECTOR", Documentation = "Removes the last item from `THE-VECTOR` and returns the value.")]
        public Task<LispObject> VectorPop(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 1 &&
                args[0] is LispVector vector)
            {
                vector.TryPop(out var result);
                return Task.FromResult(result);
            }

            return Task.FromResult<LispObject>(new LispError("Expected a vector"));
        }

        [LispFunction("VECTOR-PUSH", Signature = "VALUE THE-VECTOR", Documentation = "Adds `VALUE` to the end of `THE-VECTOR`.")]
        public Task<LispObject> VectorPush(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 2 &&
                args[0] is LispObject value &&
                args[1] is LispVector vector)
            {
                var previousCount = vector.Count;
                if (vector.TryAdd(value, out var error))
                {
                    return Task.FromResult<LispObject>(new LispInteger(previousCount));
                }

                return Task.FromResult<LispObject>(error);
            }

            return Task.FromResult<LispObject>(new LispError("Expected value and vector"));
        }

        [LispFunction("MAKE-ARRAY")]
        public Task<LispObject> MakeArray(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 0)
            {
                return Task.FromResult<LispObject>(new LispError("Expected array dimensions"));
            }

            int size;
            switch (args[0])
            {
                case LispInteger i:
                    size = i.Value;
                    break;
                case LispList l when l.Value is LispInteger i:
                    // TODO: multidimensional arrays
                    size = i.Value;
                    break;
                default:
                    return Task.FromResult<LispObject>(new LispError("Expected array dimensions"));
            }
            
            var initialElement = GetKeywordArgument(args.Skip(1), ":INITIAL-ELEMENT");
            var isAdjustable = GetKeywordArgument(args.Skip(1), ":ADJUSTABLE").IsTLike();
            var count = size;
            if (GetKeywordArgument(args.Skip(1), ":FILL-POINTER") is LispInteger fillPointer)
            {
                count = fillPointer.Value;
            }

            var items = new LispObject[count];
            for (int i = 0; i < count; i++)
            {
                items[i] = initialElement;
            }

            return Task.FromResult<LispObject>(isAdjustable
                ? LispVector.CreateAdjustable(size, items)
                : LispVector.CreateFixed(items));
        }

        [LispFunction("NUMBERP")]
        public Task<LispObject> NumberP(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispInteger _:
                case LispFloat _:
                case LispRatio _:
                    return Task.FromResult(host.T);
                default:
                    return Task.FromResult(host.Nil);
            }
        }

        [LispFunction("STRINGP")]
        public Task<LispObject> StringP(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispString _:
                    return Task.FromResult(host.T);
                default:
                    return Task.FromResult(host.Nil);
            }
        }

        [LispFunction("KEYWORDP")]
        public Task<LispObject> KeywordP(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispResolvedSymbol symbol when symbol.IsKeyword:
                    return Task.FromResult(host.T);
                default:
                    return Task.FromResult(host.Nil);
            }
        }

        [LispFunction("SYMBOLP")]
        public Task<LispObject> SymbolP(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispSymbol _:
                    return Task.FromResult(host.T);
                default:
                    return Task.FromResult(host.Nil);
            }
        }

        [LispFunction("SYMBOL-NAME")]
        public Task<LispObject> SymbolName(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 1 &&
                args[0] is LispSymbol symbol)
            {
                return Task.FromResult<LispObject>(new LispString(symbol.LocalName));
            }

            return Task.FromResult<LispObject>(new LispError("Expected exactly 1 symbol."));
        }

        [LispFunction("ZEROP")]
        public Task<LispObject> ZeroP(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispInteger i when i.IsZero:
                    return Task.FromResult(host.T);
                case LispFloat f when f.IsZero:
                    return Task.FromResult<LispObject>(host.T);
                case LispRatio r when r.IsZero:
                    return Task.FromResult<LispObject>(host.T);
                default:
                    return Task.FromResult<LispObject>(host.Nil);
            }
        }

        [LispFunction("PLUSP")]
        public Task<LispObject> PlusP(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: validate argument count
            if (args[0] is LispNumber num)
            {
                switch (num)
                {
                    case LispInteger i when i.Value > 0:
                        return Task.FromResult<LispObject>(host.T);
                    case LispFloat f when f.Value > 0.0:
                        return Task.FromResult<LispObject>(host.T);
                    case LispRatio r when !r.IsZero && Math.Sign(r.Numerator) == Math.Sign(r.Denominator):
                        return Task.FromResult<LispObject>(host.T);
                    default:
                        return Task.FromResult<LispObject>(host.Nil);
                }
            }

            return Task.FromResult<LispObject>(new LispError("wrong type input"));
        }

        [LispFunction("EVENP")]
        public Task<LispObject> EvenP(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispInteger num when num.IsEven:
                    return Task.FromResult<LispObject>(host.T);
                case LispFloat num when num.IsEven:
                    return Task.FromResult<LispObject>(host.T);
                default:
                    return Task.FromResult<LispObject>(host.Nil);
            }
        }

        [LispFunction("ODDP")]
        public Task<LispObject> OddP(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: validate argument count
            switch (args[0])
            {
                case LispInteger num when num.IsOdd:
                    return Task.FromResult<LispObject>(host.T);
                case LispFloat num when num.IsOdd:
                    return Task.FromResult<LispObject>(host.T);
                default:
                    return Task.FromResult<LispObject>(host.Nil);
            }
        }

        [LispFunction("LISTP")]
        public Task<LispObject> ListP(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: validate single argument
            return Task.FromResult<LispObject>(args[0] is LispList
                ? host.T
                : host.Nil);
        }

        [LispFunction("CONSP")]
        public Task<LispObject> ConsP(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            return Task.FromResult<LispObject>(args[0] is LispList list && !list.IsNil()
                ? host.T
                : host.Nil);
        }

        [LispFunction("STREAMP")]
        public Task<LispObject> StreamP(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            return Task.FromResult<LispObject>(args.Length == 1 && args[0] is LispStream
                ? host.T
                : host.Nil);
        }

        [LispMacro("FUNCTION")]
        public async Task<LispObject> Function(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: validate argument count
            if (args[0] is LispSymbol symbol)
            {
                var resolvedSymbol = symbol.Resolve(host.CurrentPackage);
                return new LispQuotedNamedFunctionReference(resolvedSymbol.Value);
            }
            else if (args[0] is LispList list)
            {
                var potentialLambda = await host.EvalAtStackFrameAsync(executionState.StackFrame, list, cancellationToken);
                return potentialLambda;
            }
            else
            {
                return new LispError("Expected a function symbol");
            }
        }

        [LispFunction("CONS")]
        public Task<LispObject> Cons(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: validate arguments
            return Task.FromResult<LispObject>(new LispList(args[0], args[1]));
        }

        [LispFunction("LIST")]
        public Task<LispObject> List(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            return Task.FromResult<LispObject>(LispList.FromEnumerable(args));
        }

        [LispFunction("LENGTH")]
        public Task<LispObject> Length(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 1)
            {
                switch (args[0])
                {
                    case LispList list:
                        return Task.FromResult<LispObject>(new LispInteger(list.Length));
                    case LispVector vector:
                        return Task.FromResult<LispObject>(new LispInteger(vector.Count));
                }
            }

            return Task.FromResult<LispObject>(new LispError("Expected a single sequence"));
        }

        [LispFunction("ELT")]
        public Task<LispObject> Elt(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 2 &&
                args[1] is LispInteger indexValue)
            {
                var index = indexValue.Value;
                switch (args[0])
                {
                    case LispList list when index < list.Length:
                        {
                            var result = list;
                            for (int i = 0; i < index; i++)
                            {
                                result = (LispList)result.Next;
                            }

                            result.Value.SetPointerValue = (value) => result.Value = value;
                            return Task.FromResult<LispObject>(result.Value);
                        }
                    case LispVector vector when index < vector.Count:
                        {
                            var result = vector[index];
                            result.SetPointerValue = (value) => vector[index] = value;
                            return Task.FromResult<LispObject>(result);
                        }
                }
            }

            return Task.FromResult<LispObject>(new LispError("Expected a sequence and an index"));
        }

        [LispFunction("CAR")]
        [LispFunction("FIRST")]
        public Task<LispObject> First(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: validate single argument
            if (args[0] is LispList list)
            {
                var result = list.Value;
                result.SetPointerValue = (value) => list.Value = value;
                return Task.FromResult<LispObject>(result);
            }
            else
            {
                return Task.FromResult<LispObject>(new LispError($"Expected a list, found {args[0]}"));
            }
        }

        [LispFunction("CDR")]
        [LispFunction("REST")]
        public Task<LispObject> Rest(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: validate single argument
            if (args[0] is LispList list)
            {
                var result = list.Next;
                result.SetPointerValue = (value) => list.Next = value;
                return Task.FromResult<LispObject>(result);
            }
            else
            {
                return Task.FromResult<LispObject>(new LispError($"Expected a list, found {args[0]}"));
            }
        }

        [LispFunction("KERNEL:APPEND/2")]
        public Task<LispObject> TwoArgumentAppend(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
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

                return Task.FromResult<LispObject>(result);
            }

            return Task.FromResult<LispObject>(new LispError("Expected a list and a tail"));
        }

        [LispMacro("NCONC")]
        public async Task<LispObject> Nconc(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            LispObject lastResult = host.Nil;
            for (int i = args.Length - 2; i >= 0; i--)
            {
                var a1Raw = args[i];
                var a2 = args[i + 1];

                var evalResult = await host.EvalAsync(a1Raw, cancellationToken);
                var a1 = evalResult.LastResult;

                // lastResult = (append a1 a2)
                var simulatedFunctionCall1 = LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:APPEND"), LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:QUOTE"), a1), a2);
                lastResult = await host.EvalAtStackFrameAsync(executionState.StackFrame, simulatedFunctionCall1, cancellationToken);
                if (lastResult is LispError error)
                {
                    return error;
                }

                lastResult = LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:QUOTE"), lastResult);

                // a1 = lastResult
                if (!a1.IsNil())
                {
                    var simulatedFunctionCall2 = LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:SETF"), a1Raw, lastResult);
                    await host.EvalAtStackFrameAsync(executionState.StackFrame, simulatedFunctionCall2, cancellationToken);
                }
            }

            return lastResult;
        }

        [LispFunction("REVERSE")]
        public Task<LispObject> Reverse(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 1 && args[0] is LispList list)
            {
                var values = list.ToList().Reverse();
                var result = LispList.FromEnumerable(values);
                return Task.FromResult<LispObject>(result);
            }
            else
            {
                return Task.FromResult<LispObject>(new LispError("Expected a single list"));
            }
        }

        [LispFunction("INTERSECTION")]
        public Task<LispObject> Intersection(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 2 &&
                args[0] is LispList a &&
                args[1] is LispList b)
            {
                var valuesInOne = new HashSet<LispObject>(a.ToList(), LispObject.Comparer);
                var finalSet = b.ToList().Where(i => valuesInOne.Contains(i, LispObject.Comparer));
                return Task.FromResult<LispObject>(LispList.FromEnumerable(finalSet));
            }
            else
            {
                return Task.FromResult<LispObject>(new LispError("Expected 2 lists"));
            }
        }

        [LispFunction("UNION")]
        public Task<LispObject> Union(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
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

                return Task.FromResult<LispObject>(LispList.FromEnumerable(finalSet));
            }
            else
            {
                return Task.FromResult<LispObject>(new LispError("Expected 2 lists"));
            }
        }

        [LispFunction("SET-DIFFERENCE")]
        public Task<LispObject> SetDifference(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
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

                return Task.FromResult<LispObject>(LispList.FromEnumerable(finalSet));
            }
            else
            {
                return Task.FromResult<LispObject>(new LispError("Expected 2 lists"));
            }
        }

        [LispFunction("SET-EXCLUSIVE-OR")]
        public Task<LispObject> SetExclusiveOr(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 2 &&
               args[0] is LispList a &&
               args[1] is LispList b)
            {
                var inA = new HashSet<LispObject>(a.ToList(), LispObject.Comparer);
                var inB = new HashSet<LispObject>(b.ToList(), LispObject.Comparer);
                var inBoth = inA.Intersect(inB, LispObject.Comparer);
                var finalSet = inA.Union(inB, LispObject.Comparer).Except(inBoth, LispObject.Comparer);
                return Task.FromResult<LispObject>(LispList.FromEnumerable(finalSet));
            }
            else
            {
                return Task.FromResult<LispObject>(new LispError("Expected 2 lists"));
            }
        }

        [LispFunction("REMOVE")]
        public Task<LispObject> Remove(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
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

            return Task.FromResult<LispObject>(LispList.FromEnumerable(result));
        }

        [LispFunction("REMOVE-DUPLICATES")]
        public Task<LispObject> RemoveDuplicates(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
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

                return Task.FromResult<LispObject>(LispList.FromEnumerable(finalItems));
            }
            else
            {
                return Task.FromResult<LispObject>(new LispError("Expected a list"));
            }
        }

        [LispFunction("FIND-IF")]
        public async Task<LispObject> FindIf(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
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
                        LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:QUOTE"), item)
                    };
                    var result = await FunCallAsync(host, executionState.StackFrame, functionReference, functionArguments, cancellationToken);
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
        public async Task<LispObject> RemoveIf(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
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
                    var result = await FunCallAsync(host, executionState.StackFrame, functionReference, new LispObject[] { item }, cancellationToken);
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
        public async Task<LispObject> RemoveIfNot(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
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
                    var result = await FunCallAsync(host, executionState.StackFrame, functionReference, new LispObject[] { item }, cancellationToken);
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
        public async Task<LispObject> Reduce(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
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
                    var result = await FunCallAsync(host, executionState.StackFrame, functionReference, new LispObject[] { LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:QUOTE"), arg1), LispList.FromItems(LispSymbol.CreateFromString("COMMON-LISP:QUOTE"), arg2) }, cancellationToken);
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
        public async Task<LispObject> Every(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            var result = await MapCar(host, executionState, args, cancellationToken);
            switch (result)
            {
                case LispList list when list.ToList().Any(o => o.IsNil()):
                    return host.Nil;
                default:
                    return host.T;
            }
        }

        [LispFunction("MAPCAR")]
        public async Task<LispObject> MapCar(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length >= 2)
            {
                var candidateLists = args.Skip(1).Select(a => a as LispList).ToArray();
                if (candidateLists.Any(a => a == null))
                {
                    return new LispError("Expected function reference and only lists");
                }

                Func<IEnumerable<LispObject>, Task<LispObject>> evaluator;
                switch (args[0])
                {
                    case LispInvocableObject directlyInvocable:
                        evaluator = (functionArguments) =>
                        {
                            var manualInvokeItems = new List<LispObject>() { directlyInvocable };
                            manualInvokeItems.AddRange(functionArguments);
                            var manualInvokeList = LispList.FromEnumerable(manualInvokeItems);
                            return host.EvalAtStackFrameAsync(executionState.StackFrame, manualInvokeList, cancellationToken);
                        };
                        break;
                    case LispFunctionReference functionRef:
                        evaluator = (functionArguments) => FunCallAsync(host, executionState.StackFrame, functionRef, functionArguments, cancellationToken);
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
                    var result = await evaluator(functionArguments);
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

        [LispFunction("COMPLEX")]
        public Task<LispObject> Complex(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 2 &&
                args[0] is LispSimpleNumber real &&
                args[1] is LispSimpleNumber img)
            {
                return Task.FromResult<LispObject>(new LispComplexNumber(real, img).Reduce());
            }

            return Task.FromResult<LispObject>(new LispError("Expected exactly 2 simple numbers"));
        }

        [LispFunction("<")]
        public Task<LispObject> LessThan(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            return Task.FromResult<LispObject>(FoldComparison(executionState.StackFrame, args, (a, b) => LispNumber.LessThan(a, b)));
        }

        [LispFunction("<=")]
        public Task<LispObject> LessThanOrEqual(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            return Task.FromResult<LispObject>(FoldComparison(executionState.StackFrame, args, (a, b) => LispNumber.LessThanOrEqual(a, b)));
        }

        [LispFunction(">")]
        public Task<LispObject> GreaterThan(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            return Task.FromResult<LispObject>(FoldComparison(executionState.StackFrame, args, (a, b) => LispNumber.GreaterThan(a, b)));
        }

        [LispFunction(">=")]
        public Task<LispObject> GreaterThanOrEqual(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            return Task.FromResult<LispObject>(FoldComparison(executionState.StackFrame, args, (a, b) => LispNumber.GreaterThanOrEqual(a, b)));
        }

        [LispFunction("=")]
        public Task<LispObject> NumberEqual(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            return Task.FromResult<LispObject>(FoldComparison(executionState.StackFrame, args, (a, b) => LispNumber.Equal(a, b)));
        }

        [LispFunction("EQUAL")]
        public Task<LispObject> Equal(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            return Task.FromResult<LispObject>(FoldObj(executionState.StackFrame, args, (a, b) => a.Equals(b)));
        }

        [LispFunction("EQUALP")]
        public Task<LispObject> EqualP(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            return Task.FromResult<LispObject>(FoldObj(executionState.StackFrame, args, (a, b) =>
            {
                if (a is LispString sa && b is LispString sb)
                {
                    return string.Compare(sa.Value, sb.Value, StringComparison.OrdinalIgnoreCase) == 0;
                }

                return a.Equals(b);
            }));
        }

        [LispFunction("EQ")]
        public Task<LispObject> Eq(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            return Task.FromResult<LispObject>(FoldObj(executionState.StackFrame, args, (a, b) => ReferenceEquals(a, b)));
        }

        [LispFunction("EQL")]
        public Task<LispObject> Eql(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            return Task.FromResult<LispObject>(FoldObj(executionState.StackFrame, args, (a, b) =>
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

                if (a is LispSymbol ssa && b is LispSymbol ssb)
                {
                    var resolvedA = ssa.Resolve(host.CurrentPackage);
                    var resolvedB = ssb.Resolve(host.CurrentPackage);
                    return resolvedA.Value == resolvedB.Value;
                }

                if (a is LispNilList && b is LispNilList)
                {
                    return true;
                }

                return ReferenceEquals(a, b);
            }));
        }

        [LispFunction("!=")]
        [LispFunction("<>")]
        public Task<LispObject> NotEqual(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            return Task.FromResult<LispObject>(FoldObj(executionState.StackFrame, args, (a, b) => !a.Equals(b)));
        }

        [LispMacro("AND")]
        public Task<LispObject> And(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            return FoldBoolean(host, executionState.StackFrame, args, true, false, (a, b) => a && b, cancellationToken);
        }

        [LispMacro("OR")]
        public Task<LispObject> Or(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            return FoldBoolean(host, executionState.StackFrame, args, false, true, (a, b) => a || b, cancellationToken);
        }

        [LispMacro("COND")]
        public async Task<LispObject> Cond(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            foreach (var arg in args)
            {
                if (arg is LispList list && list.Length == 2)
                {
                    var values = list.ToList();
                    var predicate = await host.EvalAtStackFrameAsync(executionState.StackFrame, values[0], cancellationToken);
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
        public Task<LispObject> Abs(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: validate arguments
            switch (args[0])
            {
                case LispInteger i:
                    return Task.FromResult<LispObject>(new LispInteger(Math.Abs(i.Value)));
                case LispFloat f:
                    return Task.FromResult<LispObject>(new LispFloat(Math.Abs(f.Value)));
                case LispRatio r:
                    return Task.FromResult<LispObject>(new LispRatio(Math.Abs(r.Numerator), Math.Abs(r.Denominator)));
                default:
                    return Task.FromResult<LispObject>(new LispError($"Expected {nameof(LispInteger)} or {nameof(LispFloat)} but found {args[0].GetType().Name} with value {args[0]}"));
            }
        }

        [LispFunction("SQRT")]
        public Task<LispObject> Sqrt(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            // TODO: validate arguments
            switch (args[0])
            {
                case LispInteger i:
                    return Task.FromResult<LispObject>(new LispFloat(Math.Sqrt(i.Value)));
                case LispFloat f:
                    return Task.FromResult<LispObject>(new LispFloat(Math.Sqrt(f.Value)));
                case LispRatio r:
                    return Task.FromResult<LispObject>(new LispFloat(Math.Sqrt(((LispFloat)r).Value)));
                default:
                    return Task.FromResult<LispObject>(new LispError($"Expected {nameof(LispInteger)} or {nameof(LispFloat)} but found {args[0].GetType().Name} with value {args[0]}"));
            }
        }

        [LispFunction("KERNEL:+/2")]
        public Task<LispObject> TwoAgumentPlus(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 2 &&
                args[0] is LispNumber n1 &&
                args[1] is LispNumber n2)
            {
                var result = LispNumber.Add(n1, n2);
                return Task.FromResult<LispObject>(result);
            }

            return Task.FromResult<LispObject>(new LispError("Expected exactly two numbers"));
        }

        [LispFunction("KERNEL:-/1")]
        public Task<LispObject> OneArgumentMinus(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 1)
            {
                // simple negation
                var value = args[0];
                switch (value)
                {
                    case LispInteger num:
                        return Task.FromResult<LispObject>(new LispInteger(num.Value * -1));
                    case LispFloat num:
                        return Task.FromResult<LispObject>(new LispFloat(num.Value * -1.0));
                    case LispRatio num:
                        return Task.FromResult<LispObject>(new LispRatio(num.Numerator * -1, num.Denominator).Reduce());
                    default:
                        return Task.FromResult<LispObject>(new LispError($"Expected type number but found {value.GetType()}"));
                }
            }

            return Task.FromResult<LispObject>(new LispError("Expected exactly two numbers"));
        }

        [LispFunction("KERNEL:-/2")]
        public Task<LispObject> TwoArgumentMinus(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 2 &&
                args[0] is LispNumber n1 &&
                args[1] is LispNumber n2)
            {
                var result = LispNumber.Sub(n1, n2);
                return Task.FromResult<LispObject>(result);
            }

            return Task.FromResult<LispObject>(new LispError("Expected exactly two numbers"));
        }

        [LispFunction("KERNEL:*/2")]
        public Task<LispObject> TwoArgumentAsterisk(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 2 &&
                args[0] is LispNumber n1 &&
                args[1] is LispNumber n2)
            {
                var result = LispNumber.Mul(n1, n2);
                return Task.FromResult<LispObject>(result);
            }

            return Task.FromResult<LispObject>(new LispError("Expected exactly two numbers"));
        }

        [LispFunction("KERNEL://2")]
        public Task<LispObject> TwoArgumentSlash(LispHost host, LispExecutionState executionState, LispObject[] args, CancellationToken cancellationToken)
        {
            if (args.Length == 2 &&
                args[0] is LispNumber n1 &&
                args[1] is LispNumber n2)
            {
                var result = LispNumber.Div(n1, n2);
                return Task.FromResult<LispObject>(result);
            }

            return Task.FromResult<LispObject>(new LispError("Expected exactly two numbers"));
        }

        private static async Task<LispObject> FoldBoolean(LispHost host, LispStackFrame frame, LispObject[] args, bool init, bool shortCircuitValue, Func<bool, bool, bool> operation, CancellationToken cancellationToken)
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
                    var evaluated = await host.EvalAtStackFrameAsync(frame, value, cancellationToken);
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
                    return frame.Root.Nil;
                }
            }

            return frame.Root.T;
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
                    return frame.Root.Nil;
                }

                lastValue = nextValue;
            }

            return frame.Root.T;
        }

        private static LispObject GetKeywordArgument(IEnumerable<LispObject> args, string keyword)
        {
            LispObject result = LispNilList.Instance;
            var enumerator = args.GetEnumerator();
            while (enumerator.MoveNext())
            {
                if (enumerator.Current is LispResolvedSymbol symbol && symbol.IsKeyword && symbol.Value == keyword)
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
