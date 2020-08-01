using System;
using System.Linq;

namespace IxMilia.Lisp
{
    internal static class LispEvaluator
    {
        internal static LispObject Evaluate(LispObject obj, LispStackFrame frame, bool errorOnMissingValue)
        {
        evalTop:
            if (obj is LispError)
            {
                return obj;
            }

            frame.UpdateCallStackLocation(obj);
            switch (obj)
            {
                case LispInteger _:
                case LispFloat _:
                case LispRatio _:
                case LispString _:
                case LispKeyword _:
                    return obj;
                case LispQuotedObject quote:
                    return quote.Value;
                case LispSymbol symbol:
                    {
                        var symbolValue = frame.GetValue(symbol.Value, allowTailCallSentinel: true);
                        if (symbolValue == null && errorOnMissingValue)
                        {
                            var error = new LispError($"Symbol '{symbol.Value}' not found");
                            TryApplyLocation(error, symbol);
                            return error;
                        }

                        if (symbolValue is LispTailCall tailCall)
                        {
                            obj = tailCall.Value;
                            goto evalTop;
                        }

                        return symbolValue;
                    }
                case LispList list:
                    {
                        if (list.IsNil)
                        {
                            return list;
                        }

                        var functionNameSymbol = (LispSymbol)list.Value;
                        var functionName = functionNameSymbol.Value;
                        var args = list.ToList().Skip(1).ToArray();
                        var value = frame.GetValue(functionName);
                        frame.UpdateCallStackLocation(functionNameSymbol);
                        LispObject result;
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
                                switch (macro)
                                {
                                    case LispCodeMacro codeMacro:
                                        result = frame.Nil;
                                        codeMacro.ExpandBody(args, frame);
                                        for (int i = 0; i < codeMacro.Body.Length; i++)
                                        {
                                            if (i == codeMacro.Body.Length - 1)
                                            {
                                                // do tail call
                                                obj = codeMacro.Body[i];
                                                frame = frame.PopForTailCall(codeMacro.Arguments);
                                                goto evalTop;
                                            }

                                            result = Evaluate(codeMacro.Body[i], frame, true);
                                            if (result is LispError)
                                            {
                                                break;
                                            }
                                        }
                                        break;
                                    case LispNativeMacro nativeMacro:
                                        result = nativeMacro.Macro.Invoke(frame, args);
                                        if (result is LispTailCall tailCall)
                                        {
                                            // do tail call
                                            obj = tailCall.Value;
                                            frame = frame.PopForTailCall();
                                            goto evalTop;
                                        }
                                        break;
                                    default:
                                        throw new InvalidOperationException($"Unsupported macro type {macro.GetType().Name}");
                                }
                            }

                            frame = frame.Pop();
                        }
                        else if (value is LispFunction function)
                        {
                            // TODO: what if it's a regular variable?
                            frame = frame.Push(function.Name);

                            // evaluate arguments
                            var evaluatedArgs = args.Select(a => Evaluate(a, frame, true)).ToArray();
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
                                        codeFunction.BindArguments(evaluatedArgs, frame);
                                        for (int i = 0; i < codeFunction.Commands.Length; i++)
                                        {
                                            if (i == codeFunction.Commands.Length - 1)
                                            {
                                                // do tail call
                                                obj = codeFunction.Commands[i];
                                                frame = frame.PopForTailCall(codeFunction.Arguments);
                                                goto evalTop;
                                            }

                                            result = Evaluate(codeFunction.Commands[i], frame, true);
                                            if (result is LispError)
                                            {
                                                break;
                                            }
                                        }
                                        break;
                                    case LispNativeFunction nativeFunction:
                                        result = nativeFunction.Function.Invoke(frame, evaluatedArgs);
                                        if (result is LispTailCall tailCall)
                                        {
                                            // do tail call
                                            obj = tailCall.Value;
                                            frame = frame.PopForTailCall();
                                            goto evalTop;
                                        }
                                        break;
                                    default:
                                        throw new InvalidOperationException($"Unsupported function type {function.GetType().Name}");
                                }
                            }

                            frame = frame.Pop();
                        }
                        else
                        {
                            result = GenerateError($"Undefined macro/function '{functionName}', found '{value?.ToString() ?? "<null>"}'", frame);
                        }

                        TryApplyLocation(result, functionNameSymbol);
                        return result;
                    }
                case LispForwardListReference forwardRef:
                    return EvalForwardReference(forwardRef, frame);
                default:
                    return frame.Nil;
            }
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
