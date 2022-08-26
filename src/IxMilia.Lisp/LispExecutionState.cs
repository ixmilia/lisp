using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace IxMilia.Lisp
{
    public class LispExecutionState
    {
        private Stack<LispObject> _argumentStack = new Stack<LispObject>();
        private List<ILispEvaluatorOperation> _operationQueue;
        internal LispTextStream CodeInputStream;
        public LispStackFrame StackFrame { get; internal set; }
        internal bool UseTailCalls { get; }
        internal bool AllowHalting { get; }
        internal LispError LastReportedError { get; set; }

        public LispObject LastResult => _argumentStack.Count > 0 ? _argumentStack.Peek() : null;

        public bool IsExecutionComplete => _operationQueue.Count == 0 && CodeInputStream.IsInputComplete;

        private LispExecutionState(LispStackFrame stackFrame, string inputName, TextReader codeReader, bool useTailCalls, bool allowHalting)
        {
            _operationQueue = new List<ILispEvaluatorOperation>();
            CodeInputStream = new LispTextStream(inputName, codeReader, TextWriter.Null);
            StackFrame = stackFrame;
            UseTailCalls = useTailCalls;
            AllowHalting = allowHalting;
        }

        internal ILispEvaluatorOperation PeekOperation()
        {
            if (_operationQueue.Count > 0)
            {
                return _operationQueue[0];
            }

            return null;
        }

        internal LispObject PeekCurrentExpression()
        {
            return _operationQueue.FirstOrDefault() is LispEvaluatorObjectExpression objectExpression
                ? objectExpression.Expression
                : null;
        }

        internal void InsertOperation(ILispEvaluatorOperation operation)
        {
            _operationQueue.Insert(0, operation);
        }

        internal void InsertObjectOperations(LispObject obj, bool createDribbleInstructions)
        {
            if (createDribbleInstructions)
            {
                InsertOperation(new LispEvaluatorDribbleExit());
            }

            InsertOperation(new LispEvaluatorObjectExpression(obj));

            if (createDribbleInstructions)
            {
                InsertOperation(new LispEvaluatorDribbleEnter(obj));
            }
        }

        internal bool TryDequeueOperation(out ILispEvaluatorOperation operation)
        {
            operation = default;
            if (_operationQueue.Count > 0)
            {
                operation = _operationQueue[0];
                _operationQueue.RemoveAt(0);
                return true;
            }

            return false;
        }

        internal void ReportError(LispError error, LispObject parent)
        {
            if (!error.SourceLocation.HasValue)
            {
                error.SourceLocation = parent?.SourceLocation;
            }

            if (error.StackFrame is null)
            {
                error.StackFrame = StackFrame;
            }

            PushArgument(error);
        }

        internal void PushArgument(LispObject argument)
        {
            _argumentStack.Push(argument);
        }

        internal bool TryPopArgument(out LispObject arg)
        {
            arg = default;
            if (_argumentStack.Count > 0)
            {
                arg = _argumentStack.Pop();
                return true;
            }

            return false;
        }

        internal void PrepareForInvocation(LispObject invokable, LispObject argumentsCandidate)
        {
            if (argumentsCandidate is LispList argumentsList)
            {
                InsertOperation(new LispEvaluatorPreInvoke(argumentsList.ToList().ToArray()));
                InsertOperation(new LispEvaluatorObjectExpression(invokable)); // TODO: do a lisp-2 and invoke to get a function value?
            }
            else
            {
                ReportError(new LispError("Expected arguments list"), argumentsCandidate);
            }
        }

        internal async Task<LispObject> FunCallAsync(LispHost host, LispFunctionReference functionReference, IEnumerable<LispObject> functionArguments, CancellationToken cancellationToken = default)
        {
            var synthesizedFunctionName = functionReference.Function.NameSymbol.Value;
            Action preExecute = null;
            Action postExecute = null;
            if (functionReference.Function is LispCodeFunction codeFunction)
            {
                //evaluatingFrame = codeFunction.CapturedStackFrame;
                //preExecute = () => evaluatingFrame.SetValue(functionReference.Function.NameSymbol, functionReference.Function);
                //postExecute = () => evaluatingFrame.DeleteValue(functionReference.Function.NameSymbol);
            }

            var synthesizedSymbol = LispSymbol.CreateFromString(synthesizedFunctionName);
            synthesizedSymbol.SourceLocation = functionReference.SourceLocation;
            var synthesizedFunctionItems = new List<LispObject>();
            synthesizedFunctionItems.Add(synthesizedSymbol);
            synthesizedFunctionItems.AddRange(functionArguments);
            var synthesizedFunctionCall = LispList.FromEnumerable(synthesizedFunctionItems);
            synthesizedFunctionCall.SourceLocation = functionReference.SourceLocation;

            preExecute?.Invoke();
            InsertOperation(new LispEvaluatorPopArgument()); // discard what eventually gets produced
            InsertOperation(new LispEvaluatorReturnIntermediate()); // break out here
            InsertOperation(new LispEvaluatorObjectExpression(synthesizedFunctionCall)); // and do this

            var result = await LispEvaluator.EvaluateAsync(host, this, cancellationToken);

            postExecute?.Invoke();

            return LastResult;
        }

        internal static LispExecutionState CreateExecutionState(LispStackFrame stackFrame, string inputName, string code, bool useTailCalls, bool allowHalting)
        {
            var reader = new StringReader(code);
            var executionState = new LispExecutionState(stackFrame, inputName, reader, useTailCalls, allowHalting);
            return executionState;
        }

        internal static LispExecutionState CreateExecutionState(LispStackFrame stackFrame, string inputName, LispObject obj, bool useTailCalls, bool allowHalting, bool createDribbleInstructions)
        {
            var executionState = CreateExecutionState(stackFrame, inputName, string.Empty, useTailCalls, allowHalting);
            executionState.InsertObjectOperations(obj, createDribbleInstructions);
            return executionState;
        }
    }
}
