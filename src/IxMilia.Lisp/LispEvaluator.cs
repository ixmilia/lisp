using System;
using System.Collections.Generic;
using System.Linq;

namespace IxMilia.Lisp
{
    internal static class LispEvaluator
    {
        internal static LispObject Evaluate(LispObject obj, LispStackFrame frame, bool errorOnMissingValue)
        {
            return Evaluate(new List<Tuple<LispObject, LispStackFrame>>() { Tuple.Create<LispObject, LispStackFrame>(obj, null) }, frame, errorOnMissingValue);
        }

        private static LispObject Evaluate(List<Tuple<LispObject, LispStackFrame>> stack, LispStackFrame frame, bool errorOnMissingValue)
        {
            var rootFrame = frame.Root;
            var result = frame.Nil;
            while (stack.Count > 0)
            {
                var item = stack[0].Item1;
                var lastFrame = stack[0].Item2;
                stack.RemoveAt(0);

                frame.UpdateCallStackLocation(item);
                switch (item)
                {
                    case LispError _:
                        return item;
                    case LispInteger _:
                    case LispFloat _:
                    case LispRatio _:
                    case LispString _:
                    case LispKeyword _:
                    case LispQuotedNamedFunctionReference _:
                    case LispStream _:
                        result = item;
                        break;
                    case LispQuotedLambdaFunctionReference lambda:
                        lambda.StackFrame = frame;
                        result = lambda;
                        break;
                    case LispQuotedObject quote:
                        result = quote.Value;
                        break;
                    case LispSymbol symbol:
                        {
                            result = frame.GetValue(symbol.Value);
                            if (result == null && errorOnMissingValue)
                            {
                                var error = new LispError($"Symbol '{symbol.Value}' not found");
                                TryApplyLocation(error, symbol);
                                result = error;
                            }

                            break;
                        }
                    case LispList list:
                        {
                            if (list.IsNil())
                            {
                                result = list;
                                break;
                            }

                            var functionNameSymbol = (LispSymbol)list.Value;
                            var functionName = functionNameSymbol.Value;
                            var args = list.ToList().Skip(1).ToArray();
                            var value = frame.GetValue(functionName);
                            frame.UpdateCallStackLocation(functionNameSymbol);
                            if (value is LispMacro macro)
                            {
                                frame = frame.Push(macro.Name);

                                var firstError = args.OfType<LispError>().FirstOrDefault();
                                if (firstError != null)
                                {
                                    result = firstError;
                                }
                                else
                                {
                                    List<Tuple<LispObject, LispStackFrame>> macroExpansion;
                                    switch (macro)
                                    {
                                        case LispCodeMacro codeMacro:
                                            macroExpansion = codeMacro.ExpandBody(args).Select(m => Tuple.Create<LispObject, LispStackFrame>(m, null)).ToList();
                                            break;
                                        case LispNativeMacro nativeMacro:
                                            macroExpansion = nativeMacro.Macro.Invoke(frame, args).Select(m => Tuple.Create<LispObject, LispStackFrame>(m, null)).ToList();
                                            break;
                                        default:
                                            throw new InvalidOperationException($"Unsupported macro type {macro.GetType().Name}");
                                    }

                                    if (macroExpansion.Count > 0)
                                    {
                                        result = macroExpansion[macroExpansion.Count - 1].Item1;
                                    }

                                    stack.InsertRange(0, macroExpansion);
                                }

                                frame = frame.Pop();
                            }
                            else if (value is LispFunction function)
                            {
                                // TODO: what if it's a regular variable?
                                frame = frame.Push(function.Name);
                                var doPop = true;

                                // evaluate arguments
                                var evaluatedArgs = args.Select(a => Evaluate(a, frame, true)).ToArray();

                                // tracing
                                rootFrame.OnFunctionEnter(frame, evaluatedArgs);

                                // early exit
                                var firstError = evaluatedArgs.OfType<LispError>().FirstOrDefault();
                                if (firstError != null)
                                {
                                    result = firstError;
                                }
                                else
                                {
                                    switch (function)
                                    {
                                        case LispCodeFunction codeFunction:
                                            result = frame.Nil;
                                            if (!codeFunction.TryBindArguments(evaluatedArgs, frame, out var bindingError))
                                            {
                                                result = bindingError;
                                            }
                                            else
                                            {
                                                for (int i = 0; i < codeFunction.Commands.Length; i++)
                                                {
                                                    if (i == codeFunction.Commands.Length - 1)
                                                    {
                                                        // do tail call
                                                        stack.Insert(0, Tuple.Create(codeFunction.Commands[i], frame));
                                                        frame = frame.PopForTailCall(codeFunction.Arguments);
                                                        doPop = false;
                                                        break;
                                                    }

                                                    result = Evaluate(codeFunction.Commands[i], frame, true);
                                                }
                                            }
                                            break;
                                        case LispNativeFunction nativeFunction:
                                            result = nativeFunction.Function.Invoke(frame, evaluatedArgs);
                                            break;
                                        default:
                                            throw new InvalidOperationException($"Unsupported function type {function.GetType().Name}");
                                    }
                                }

                                if (doPop)
                                {
                                    // non-tail-call exit of code function
                                    rootFrame.OnFunctionReturn(frame, result);
                                    frame = frame.Pop();
                                }
                            }
                            else
                            {
                                result = GenerateError($"Undefined macro/function '{functionName}', found '{value?.ToString() ?? "<null>"}'", frame);
                            }

                            TryApplyLocation(result, functionNameSymbol);
                            break;
                        }
                    case LispForwardListReference forwardRef:
                        result = EvalForwardReference(forwardRef, frame);
                        break;
                    default:
                        result = frame.Nil;
                        break;
                }

                if (lastFrame != null)
                {
                    // tail-call exit of code function
                    rootFrame.OnFunctionReturn(lastFrame, result);
                }
            }

            return result;
        }

        private static LispObject EvalForwardReference(LispForwardListReference forwardRef, LispStackFrame frame)
        {
            LispObject result;
            var finalList = new LispCircularList();
            frame.SetValue(forwardRef.ForwardReference.SymbolReference, finalList);
            var values = forwardRef.List.ToList();
            var evaluatedValues = values.Select(v => LispEvaluator.Evaluate(v, frame, true));
            var firstError = evaluatedValues.OfType<LispError>().FirstOrDefault();
            if (firstError != null)
            {
                result = firstError;
            }
            else
            {
                var tempList = forwardRef.List.IsProperList
                    ? LispList.FromEnumerable(evaluatedValues)
                    : LispList.FromEnumerableImproper(evaluatedValues.First(), evaluatedValues.Skip(1).First(), evaluatedValues.Skip(2));
                finalList.ApplyForCircularReference(tempList, isProperList: forwardRef.List.IsProperList);
                result = finalList;
            }

            TryApplyLocation(result, forwardRef);
            return result;
        }

        private static void TryApplyLocation(LispObject obj, LispObject parent)
        {
            if (obj.Line == 0 && obj.Column == 0)
            {
                obj.Line = parent.Line;
                obj.Column = parent.Column;
            }
        }

        private static LispError GenerateError(string message, LispStackFrame frame)
        {
            var error = new LispError(message);
            error.TryApplyStackFrame(frame);
            return error;
        }
    }
}
