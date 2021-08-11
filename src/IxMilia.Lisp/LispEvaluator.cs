using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace IxMilia.Lisp
{
    internal class LispEvaluator
    {
        public static LispExecutionState Evaluate(LispExecutionState executionState)
        {
            var shouldDribbleReturnValue = executionState.StackFrame.Root.DribbleStream != null;
            while (executionState.TryDequeueOperation(out var operation))
            {
                if (executionState.LastResult is LispError error)
                {
                    // re-queue, because we can never finish
                    executionState.InsertOperation(operation);
                    executionState.StackFrame.Root.OnErrorOccured(error, executionState.StackFrame);
                    return executionState;
                }

                // value setting can only occur when evaluating a native macro or native function; if any of the set operations wants to halt, we do that below
                var captureValueSetHalt = false;
                var haltDueToValueSet = false;
                var valueSet = new EventHandler<LispValueSetEventArgs>((s, e) =>
                {
                    if (captureValueSetHalt)
                    {
                        haltDueToValueSet = haltDueToValueSet || e.HaltExecution;
                    }
                });
                executionState.StackFrame.Root.ValueSet += valueSet;
                switch (operation)
                {
                    case LispEvaluatorPopForTailCall tailPop:
                        {
                            // the shape of the operation stack at the time of a tail call should be either:
                            //   pop
                            //   tail-call-expression
                            //   function-return
                            // or
                            //   tail-call-expression
                            //   function-return
                            ILispEvaluatorOperation tailCallExpression = null;
                            ILispEvaluatorOperation invocationExit = null;
                            if ((executionState.PeekOperation() is LispEvaluatorPopArgument pop &&
                                    executionState.TryDequeueOperation(out var _) &&
                                    executionState.PeekOperation() is LispEvaluatorObjectExpression &&
                                    executionState.TryDequeueOperation(out tailCallExpression) &&
                                    executionState.PeekOperation() is LispEvaluatorInvocationExit &&
                                    executionState.TryDequeueOperation(out invocationExit)) ||
                                (executionState.PeekOperation() is LispEvaluatorObjectExpression &&
                                    executionState.TryDequeueOperation(out tailCallExpression) &&
                                    executionState.PeekOperation() is LispEvaluatorInvocationExit &&
                                    executionState.TryDequeueOperation(out invocationExit)))
                            {
                                var concreteInvocationExit = (LispEvaluatorInvocationExit)invocationExit;
                                Debug.Assert(ReferenceEquals(tailPop.InvocationObject, concreteInvocationExit.InvocationObject));

                                // restore the tail call operation and pop the stack
                                executionState.InsertOperation(concreteInvocationExit.WithoutFramePop());
                                executionState.InsertOperation(tailCallExpression);
                                executionState.StackFrame.CopyLocalsToParentForTailCall(new HashSet<string>(tailPop.InvocationArgumentNames));
                                executionState.StackFrame = executionState.StackFrame.Parent;
                            }
                            else
                            {
                                throw new InvalidOperationException("Unrecognized tail call operation pattern");
                            }
                        }
                        break;
                    case LispEvaluatorObjectExpression expression:
                        {
                            executionState.StackFrame.UpdateCallStackLocation(expression.Expression.SourceLocation);

                            var halt = executionState.StackFrame.Root.OnEvaluatingExpression(expression.Expression, executionState.StackFrame);
                            if (executionState.AllowHalting && halt)
                            {
                                executionState.InsertOperation(expression);
                                return executionState;
                            }

                            switch (expression.Expression)
                            {
                                case LispError expressionError:
                                    executionState.PushArgument(expressionError);
                                    break;
                                case LispInteger _:
                                case LispFloat _:
                                case LispRatio _:
                                case LispString _:
                                case LispKeyword _:
                                case LispLambdaListKeyword _:
                                case LispQuotedNamedFunctionReference _:
                                case LispStream _:
                                    executionState.PushArgument(expression.Expression);
                                    break;
                                case LispQuotedLambdaFunctionReference lambda:
                                    lambda.StackFrame = executionState.StackFrame;
                                    executionState.PushArgument(expression.Expression);
                                    break;
                                case LispQuotedObject quote:
                                    executionState.PushArgument(quote.Value);
                                    break;
                                case LispForwardListReference forwardRef:
                                    {
                                        LispObject result;
                                        var finalList = new LispCircularList();
                                        executionState.StackFrame.SetValue(forwardRef.ForwardReference.SymbolReference, finalList);
                                        var values = forwardRef.List.ToList();
                                        var evaluatedValues = values.Select(v =>
                                        {
                                        // TODO: evaluate using the operation queue
                                        var itemExecutionState = LispExecutionState.CreateExecutionState(executionState.StackFrame, new LispObject[] { v }, executionState.UseTailCalls, allowHalting: false, createDribbleInstructions: false);
                                            var itemResult = Evaluate(itemExecutionState);
                                            return itemResult.LastResult;
                                        });
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

                                        TryApplySourceLocation(result, forwardRef);
                                        executionState.PushArgument(result);
                                    }
                                    break;
                                case LispSymbol symbol:
                                    {
                                        var value = executionState.StackFrame.GetValue(symbol.Value);
                                        if (value is null)
                                        {
                                            executionState.ReportError(new LispError($"Symbol '{symbol.Value}' not found"), symbol);
                                            break;
                                        }
                                        else
                                        {
                                            executionState.PushArgument(value);
                                        }
                                    }
                                    break;
                                case LispList list when list.IsNil():
                                    executionState.PushArgument(list);
                                    break;
                                case LispList sList:
                                    {
                                        if (!(sList.Value is LispSymbol invocationSymbol))
                                        {
                                            executionState.ReportError(new LispError($"Expected symbol for invocation, but found [{sList.Value}]"), sList.Value);
                                            break;
                                        }

                                        var arguments = sList.ToList().Skip(1).ToList();
                                        var invocationObject = executionState.StackFrame.GetValue<LispMacroOrFunction>(invocationSymbol.Value);
                                        if (invocationObject is null)
                                        {
                                            executionState.ReportError(new LispError($"Undefined macro/function '{invocationSymbol.Value}', found '<null>'"), sList.Value);
                                            break;
                                        }
                                        executionState.InsertOperation(new LispEvaluatorInvocationExit(invocationObject, invocationSymbol.SourceLocation));

                                        // insert function body back to front
                                        switch (invocationObject)
                                        {
                                            case LispNativeMacro _:
                                            case LispCodeMacro _:
                                            case LispNativeFunction _:
                                                // nothing; handled during execution
                                                break;
                                            case LispCodeFunction codeFunction:
                                                for (int i = codeFunction.Commands.Length - 1; i >= 0; i--)
                                                {
                                                    executionState.InsertOperation(new LispEvaluatorObjectExpression(codeFunction.Commands[i]));
                                                    if (i != 0)
                                                    {
                                                        executionState.InsertOperation(new LispEvaluatorPopArgument());
                                                    }

                                                    var isTailCallCandidate = i == codeFunction.Commands.Length - 1;
                                                    if (executionState.UseTailCalls && isTailCallCandidate)
                                                    {
                                                        // the previously inserted operation is a candidate for a tail call
                                                        executionState.InsertOperation(new LispEvaluatorPopForTailCall(invocationObject, codeFunction.ArgumentCollection.ArgumentNames));
                                                    }
                                                }
                                                break;
                                            default:
                                                throw new NotSupportedException($"Unexpected function/macro object '{invocationObject.GetType().Name}'");
                                        }

                                        executionState.InsertOperation(new LispEvaluatorInvocation(invocationObject, sList.SourceLocation, arguments.Count));

                                        // evaluate/add arguments
                                        switch (invocationObject)
                                        {
                                            case LispMacro _:
                                                for (int i = 0; i < arguments.Count; i++)
                                                {
                                                    executionState.PushArgument(arguments[i]);
                                                }
                                                break;
                                            case LispFunction _:
                                                for (int i = arguments.Count - 1; i >= 0; i--)
                                                {
                                                    executionState.InsertOperation(new LispEvaluatorObjectExpression(arguments[i]));
                                                }
                                                break;
                                            default:
                                                throw new NotSupportedException($"Unexpected function/macro object '{invocationObject.GetType().Name}'");
                                        }
                                    }
                                    break;
                                default:
                                    throw new NotSupportedException($"Unexpected object type {expression.Expression.GetType().Name} with value {expression.Expression}");
                            }
                        }
                        break;
                    case LispEvaluatorDribbleEnter dribbleEnter:
                        {
                            var dribbleOutput = executionState.StackFrame.Root.DribbleStream?.Output;
                            if (dribbleOutput != null)
                            {
                                shouldDribbleReturnValue = true;
                                dribbleOutput.WriteLine($"> {dribbleEnter.Expression}");
                            }
                            else
                            {
                                shouldDribbleReturnValue = false;
                            }
                        }
                        break;
                    case LispEvaluatorDribbleExit _:
                        {
                            var dribbleOutput = executionState.StackFrame.Root.DribbleStream?.Output;
                            if (shouldDribbleReturnValue && dribbleOutput != null)
                            {
                                dribbleOutput.WriteLine(executionState.LastResult?.ToString());
                                dribbleOutput.WriteLine();
                            }
                        }
                        break;
                    case LispEvaluatorInvocationExit exit:
                        {
                            if (executionState.LastResult != null)
                            {
                                executionState.LastResult.SourceLocation = exit.InvocationLocation;
                            }

                            var halt = executionState.StackFrame.Root.OnFunctionReturn(exit.InvocationObject, executionState.StackFrame, executionState.LastResult);
                            if (exit.PopFrame)
                            {
                                executionState.StackFrame = executionState.StackFrame.Parent;
                            }

                            if (executionState.AllowHalting && halt)
                            {
                                return executionState;
                            }
                        }
                        break;
                    case LispEvaluatorPopArgument _:
                        if (!executionState.TryPopArgument(out var _))
                        {
                            executionState.ReportError(new LispError($"Expected argument to pop off the stack but found nothing"), null);
                            return executionState;
                        }
                        break;
                    case LispEvaluatorInvocation invocation:
                        {
                            var arguments = new LispObject[invocation.ArgumentCount];
                            var foundArgumentCount = 0;
                            for (int i = invocation.ArgumentCount - 1; i >= 0; i--)
                            {
                                if (!executionState.TryPopArgument(out arguments[i]))
                                {
                                    executionState.ReportError(new LispError($"Insufficient arguments for function '{invocation.InvocationObject.Name}'.  Expected {invocation.ArgumentCount} arguments but only found {foundArgumentCount}"), invocation.InvocationObject);
                                }

                                foundArgumentCount++;
                            }

                            executionState.StackFrame = new LispStackFrame(invocation.InvocationObject.Name, executionState.StackFrame);
                            var halt = executionState.StackFrame.Root.OnFunctionEnter(executionState.StackFrame, arguments);
                            if (executionState.AllowHalting && halt)
                            {
                                return executionState;
                            }

                            // bind arguments
                            switch (invocation.InvocationObject)
                            {
                                case LispMacro macro:
                                    string[] macroArguments;
                                    List<LispObject> macroExpansion;
                                    switch (macro)
                                    {
                                        case LispCodeMacro codeMacro:
                                            macroArguments = codeMacro.Arguments;
                                            macroExpansion = codeMacro.ExpandBody(arguments).ToList();
                                            break;
                                        case LispNativeMacro nativeMacro:
                                            captureValueSetHalt = true;
                                            macroArguments = Array.Empty<string>();
                                            macroExpansion = nativeMacro.Macro.Invoke(executionState.StackFrame, arguments).ToList();
                                            break;
                                        default:
                                            throw new NotImplementedException($"Unsupported macro object {invocation.InvocationObject.GetType().Name}");
                                    }

                                    for (int i = macroExpansion.Count - 1; i >= 0; i--)
                                    {
                                        executionState.InsertOperation(new LispEvaluatorObjectExpression(macroExpansion[i]));
                                        if (i != 0)
                                        {
                                            executionState.InsertOperation(new LispEvaluatorPopArgument());
                                        }

                                        var isTailCallCandidate = i == macroExpansion.Count - 1;
                                        if (executionState.UseTailCalls && isTailCallCandidate)
                                        {
                                            // the previously inserted operation is a candidate for a tail call
                                            executionState.InsertOperation(new LispEvaluatorPopForTailCall(invocation.InvocationObject, macroArguments));
                                        }
                                    }
                                    break;
                                case LispCodeFunction codeFunction:
                                    if (!codeFunction.TryBindArguments(arguments, executionState.StackFrame, out var bindError))
                                    {
                                        executionState.ReportError(bindError, invocation.InvocationObject);
                                        goto invocation_done;
                                    }
                                    break;
                                case LispNativeFunction nativeFunction:
                                    captureValueSetHalt = true;
                                    var evaluationResult = nativeFunction.Function.Invoke(executionState.StackFrame, arguments);
                                    if (!evaluationResult.SourceLocation.HasValue)
                                    {
                                        evaluationResult.SourceLocation = invocation.SourceLocation;
                                    }

                                    executionState.PushArgument(evaluationResult);
                                    break;
                                default:
                                    throw new NotImplementedException($"Unsupported macro/function object {invocation.InvocationObject.GetType().Name}");
                            }
                        }
                    invocation_done:
                        break;
                    default:
                        throw new NotImplementedException($"Unhandled RPN operation {operation.GetType().Name}");
                }

                executionState.StackFrame.Root.ValueSet -= valueSet;
                if (executionState.AllowHalting && haltDueToValueSet)
                {
                    return executionState;
                }
            }

            return executionState;
        }

        private static void TryApplySourceLocation(LispObject obj, LispObject parent)
        {
            if (!obj.SourceLocation.HasValue)
            {
                obj.SourceLocation = parent.SourceLocation;
            }
        }
    }
}
