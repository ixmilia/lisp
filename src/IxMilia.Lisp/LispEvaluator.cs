﻿using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace IxMilia.Lisp
{
    internal class LispEvaluator
    {
        public static async Task<LispEvaluationState> EvaluateAsync(LispHost host, LispExecutionState executionState, CancellationToken cancellationToken = default)
        {
            var shouldDribbleReturnValue = executionState.StackFrame.Root.DribbleStream != null;
            while (executionState.TryDequeueOperation(out var operation))
            {
                cancellationToken.ThrowIfCancellationRequested();
                if (executionState.LastResult is LispError error)
                {
                    // re-queue, because we can never finish
                    executionState.InsertOperation(operation);
                    error.StackFrame ??= executionState.StackFrame;
                    executionState.StackFrame.Root.OnErrorOccured(error, executionState.StackFrame);
                    return LispEvaluationState.FatalHalt;
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
                                    executionState.PeekOperation() is LispEvaluatorFunctionExit &&
                                    executionState.TryDequeueOperation(out invocationExit)) ||
                                (executionState.PeekOperation() is LispEvaluatorObjectExpression &&
                                    executionState.TryDequeueOperation(out tailCallExpression) &&
                                    executionState.PeekOperation() is LispEvaluatorFunctionExit &&
                                    executionState.TryDequeueOperation(out invocationExit)))
                            {
                                var concreteInvocationExit = (LispEvaluatorFunctionExit)invocationExit;
                                Debug.Assert(ReferenceEquals(tailPop.Function, concreteInvocationExit.Function));

                                // restore the tail call operation and pop the stack
                                executionState.InsertOperation(concreteInvocationExit.WithoutFramePop());
                                executionState.InsertOperation(tailCallExpression);
                                executionState.StackFrame.CopyLocalsToParentForTailCall(host.CurrentPackage, new HashSet<string>(tailPop.InvocationArgumentNames));
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
                                return LispEvaluationState.NonFatalHalt;
                            }

                            switch (expression.Expression)
                            {
                                case LispError _:
                                case LispNumber _:
                                case LispCharacter _:
                                case LispString _:
                                case LispVector _:
                                case LispLambdaListKeyword _:
                                case LispCodeFunction _:
                                case LispQuotedNamedFunctionReference _:
                                case LispPackage _:
                                case LispStream _:
                                case LispReadTable _:
                                    executionState.PushArgument(expression.Expression);
                                    break;
                                case LispQuotedLambdaFunctionReference lambda:
                                    lambda.StackFrame = executionState.StackFrame;
                                    executionState.PushArgument(expression.Expression);
                                    break;
                                case LispForwardListReference forwardRef:
                                    {
                                        LispObject result;
                                        var finalList = new LispCircularList();
                                        executionState.StackFrame.SetValue(forwardRef.SymbolReference, finalList);
                                        var values = forwardRef.List.ToList();
                                        var evaluatedValues = values.Select(async v =>
                                        {
                                            // TODO: evaluate using the operation queue
                                            var itemExecutionState = LispExecutionState.CreateExecutionState(executionState.StackFrame, "TODO: input name", v, executionState.UseTailCalls, allowHalting: false, createDribbleInstructions: false);
                                            await EvaluateAsync(host, itemExecutionState, cancellationToken);
                                            return itemExecutionState.LastResult;
                                        }).Select(t => t.Result);
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

                                        executionState.PushArgument(result);
                                    }
                                    break;
                                case LispSymbol symbol:
                                    {
                                        var resolvedSymbol = symbol.Resolve(host.CurrentPackage);
                                        var value = executionState.StackFrame.GetValue(resolvedSymbol);
                                        if (value is null)
                                        {
                                            executionState.ReportError(new LispError($"Symbol '{resolvedSymbol.LocalName}' not found"), symbol);
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
                                        LispInvocableObject invocationObject;
                                        if (sList.Value is LispSymbol invocationSymbol)
                                        {
                                            var resolvedInvocationSymbol = invocationSymbol.Resolve(host.CurrentPackage);
                                            var candidateInvocationObject = executionState.StackFrame.GetValue(resolvedInvocationSymbol);
                                            if (candidateInvocationObject is LispInvocableObject invokable)
                                            {
                                                invocationObject = invokable;
                                            }
                                            else if (candidateInvocationObject is null)
                                            {
                                                executionState.ReportError(new LispError($"Undefined macro/function '{invocationSymbol.LocalName}', found '<null>'"), sList.Value);
                                                break;
                                            }
                                            else
                                            {
                                                executionState.ReportError(new LispError($"Expected macro or function, but found {candidateInvocationObject}"), sList.Value);
                                                break;
                                            }
                                        }
                                        else if (sList.Value is LispInvocableObject directInvocationObject)
                                        {
                                            invocationObject = directInvocationObject;
                                        }
                                        else
                                        {
                                            executionState.ReportError(new LispError($"Unsupported invocation object '{sList.Value}'"), sList.Value);
                                            break;
                                        }

                                        var arguments = sList.ToList().Skip(1).ToList();

                                        // insert function body back to front
                                        switch (invocationObject)
                                        {
                                            case LispSpecialOperator _:
                                            case LispMacro _:
                                                // nothing
                                                break;
                                            case LispFunction function:
                                                executionState.InsertOperation(new LispEvaluatorFunctionExit(function, expression.Expression));
                                                switch (function)
                                                {
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
                                                                var tailCallArgumentNames = codeFunction.ArgumentCollection.ArgumentNames.Select(a => LispSymbol.CreateFromString(a).Resolve(host.CurrentPackage).Value).ToList();
                                                                executionState.InsertOperation(new LispEvaluatorPopForTailCall(invocationObject, tailCallArgumentNames));
                                                            }
                                                        }
                                                        break;
                                                    default:
                                                        throw new NotSupportedException($"Unexpected function object '{invocationObject.GetType().Name}'");
                                                }
                                                break;
                                            default:
                                                throw new NotSupportedException($"Unexpected function/macro object '{invocationObject.GetType().Name}'");
                                        }

                                        executionState.InsertOperation(new LispEvaluatorInvocation(invocationObject, arguments.Count));

                                        // evaluate/add arguments
                                        switch (invocationObject)
                                        {
                                            // unevaluated arguments
                                            case LispSpecialOperator _:
                                            case LispMacro _:
                                                for (int i = 0; i < arguments.Count; i++)
                                                {
                                                    executionState.PushArgument(arguments[i]);
                                                }
                                                break;
                                            // evaluated arguments
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

                            halt = executionState.StackFrame.Root.OnEvaluatedExpression(expression.Expression, executionState.LastResult, executionState.StackFrame);
                            if (executionState.AllowHalting && halt)
                            {
                                return LispEvaluationState.NonFatalHalt;
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
                    case LispEvaluatorFunctionExit exit:
                        {
                            var halt = executionState.StackFrame.Root.OnFunctionReturn(exit.Function, executionState.StackFrame, executionState.LastResult);
                            if (exit.PopFrame)
                            {
                                executionState.StackFrame = executionState.StackFrame.Parent;
                            }

                            halt = executionState.StackFrame.Root.OnEvaluatedExpression(exit.InvocationExpression, executionState.LastResult, executionState.StackFrame) || halt;

                            if (executionState.AllowHalting && halt)
                            {
                                return LispEvaluationState.NonFatalHalt;
                            }
                        }
                        break;
                    case LispEvaluatorPopArgument _:
                        if (!executionState.TryPopArgument(out var _))
                        {
                            executionState.ReportError(new LispError($"Expected argument to pop off the stack but found nothing"), null);
                            return LispEvaluationState.FatalHalt;
                        }
                        break;
                    case LispEvaluatorInvocation invocation:
                        {
                            await Task.Yield();
                            var arguments = new LispObject[invocation.ArgumentCount];
                            var foundArgumentCount = 0;
                            for (int i = invocation.ArgumentCount - 1; i >= 0; i--)
                            {
                                if (!executionState.TryPopArgument(out arguments[i]))
                                {
                                    executionState.ReportError(new LispError($"Insufficient arguments for function '{invocation.InvocationObject.NameSymbol.LocalName}'.  Expected {invocation.ArgumentCount} arguments but only found {foundArgumentCount}"), invocation.InvocationObject);
                                }

                                foundArgumentCount++;
                            }

                            // bind arguments
                            switch (invocation.InvocationObject)
                            {
                                case LispSpecialOperator specialOperator:
                                    await specialOperator.Delegate.Invoke(host, executionState, arguments, cancellationToken);
                                    break;
                                case LispMacro macro:
                                    {
                                        LispObject result;
                                        switch (macro)
                                        {
                                            case LispCodeMacro codeMacro:
                                                if (!codeMacro.ArgumentCollection.TryMatchInvocationArguments(arguments, out var matchedArguments, out var argumentBindError))
                                                {
                                                    executionState.ReportError(argumentBindError, macro);
                                                    goto invocation_done;
                                                }

                                                var replacements = new Dictionary<string, LispObject>();
                                                foreach (var matchedArgument in matchedArguments)
                                                {
                                                    var resolvedSymbol = LispSymbol.CreateFromString(matchedArgument.Item1.Name).Resolve(host.CurrentPackage);
                                                    replacements[resolvedSymbol.Value] = matchedArgument.Item2;
                                                }

                                                result = codeMacro.Body.PerformMacroReplacements(host.CurrentPackage, replacements);
                                                break;
                                            case LispNativeMacro nativeMacro:
                                                captureValueSetHalt = true;
                                                result = await nativeMacro.Macro.Invoke(host, executionState, arguments, cancellationToken);
                                                break;
                                            default:
                                                throw new NotImplementedException($"Unsupported macro object {invocation.InvocationObject.GetType().Name}");
                                        }

                                        var halt = executionState.StackFrame.Root.OnMacroExpanded(macro, executionState.StackFrame, result);
                                        executionState.InsertOperation(new LispEvaluatorObjectExpression(result));

                                        if (executionState.AllowHalting && halt)
                                        {
                                            return LispEvaluationState.NonFatalHalt;
                                        }
                                    }
                                    break;
                                case LispFunction function:
                                    {
                                        executionState.StackFrame = new LispStackFrame(invocation.InvocationObject, executionState.StackFrame);
                                        switch (function)
                                        {
                                            case LispCodeFunction codeFunction:
                                                {
                                                    var (success, bindError) = await codeFunction.TryBindArgumentsAsync(arguments, host, executionState.StackFrame, cancellationToken);
                                                    if (!success)
                                                    {
                                                        executionState.ReportError(bindError, invocation.InvocationObject);
                                                        goto invocation_done;
                                                    }

                                                    // only mark the function as entered once the parameters have been bound
                                                    var halt = executionState.StackFrame.Root.OnFunctionEnter(executionState.StackFrame, arguments);
                                                    if (executionState.AllowHalting && halt)
                                                    {
                                                        return LispEvaluationState.NonFatalHalt;
                                                    }
                                                }
                                                break;
                                            case LispNativeFunction nativeFunction:
                                                {
                                                    var halt = executionState.StackFrame.Root.OnFunctionEnter(executionState.StackFrame, arguments);
                                                    if (executionState.AllowHalting && halt)
                                                    {
                                                        return LispEvaluationState.NonFatalHalt;
                                                    }

                                                    captureValueSetHalt = true;
                                                    var evaluationResult = await nativeFunction.Function.Invoke(host, executionState, arguments, cancellationToken);

                                                    executionState.PushArgument(evaluationResult);
                                                }
                                                break;
                                            default:
                                                throw new NotImplementedException($"Unsupported function object {function.GetType().Name}");
                                        }
                                    }
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
                    return LispEvaluationState.NonFatalHalt;
                }
            }

            return LispEvaluationState.Complete;
        }
    }
}
