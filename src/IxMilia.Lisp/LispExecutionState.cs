using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace IxMilia.Lisp
{
    public class LispExecutionState
    {
        private Stack<LispObject> _argumentStack = new Stack<LispObject>();
        private List<ILispEvaluatorOperation> _operationQueue;
        private ReportingStringReader _codeReader;
        internal LispStream CodeInputStream;
        internal LispStackFrame StackFrame { get; set; }
        internal bool UseTailCalls { get; }
        internal bool AllowHalting { get; }

        public LispObject LastResult => _argumentStack.Count > 0 ? _argumentStack.Peek() : null;

        public bool IsExecutionComplete => _operationQueue.Count == 0 && _codeReader.IsComplete;

        private LispExecutionState(LispStackFrame stackFrame, string inputName, ReportingStringReader codeReader, bool useTailCalls, bool allowHalting)
        {
            _operationQueue = new List<ILispEvaluatorOperation>();
            _codeReader = codeReader;
            CodeInputStream = new LispStream(inputName, _codeReader, TextWriter.Null);
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

        internal static LispExecutionState CreateExecutionState(LispStackFrame stackFrame, string inputName, string code, bool useTailCalls, bool allowHalting)
        {
            var reader = new ReportingStringReader(code);
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
