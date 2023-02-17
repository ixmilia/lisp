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
        private Stack<LispObject> _argumentStack = new Stack<LispObject>(16);
        private Stack<ILispEvaluatorOperation> _operationStack = new Stack<ILispEvaluatorOperation>(128);
        public ulong InstructionCount = 0;

        public ulong MaxArgumentCount = 0;
        public ulong MaxOperationCount = 0;

        public LispStackFrame StackFrame { get; internal set; }
        internal bool AllowHalting { get; }
        public LispError LastReportedError { get; internal set; }

        public LispObject LastResult => _argumentStack.Count > 0 ? _argumentStack.Peek() : null;

        public bool IsExecutionComplete => _operationStack.Count == 0;

        private LispExecutionState(LispStackFrame stackFrame, bool allowHalting)
        {
            StackFrame = stackFrame;
            AllowHalting = allowHalting;
        }

        internal ILispEvaluatorOperation PeekOperation()
        {
            if (_operationStack.Count > 0)
            {
                return _operationStack.Peek();
            }

            return null;
        }

        internal LispObject PeekCurrentExpression()
        {
            return _operationStack.FirstOrDefault() is LispEvaluatorObjectExpression objectExpression
                ? objectExpression.Expression
                : null;
        }

        internal void InsertCodeOperations(string inputName, string code)
        {
            var reader = new ObservableStringReader(code);
            var stream = new LispTextStream(inputName, reader, TextWriter.Null);
            InsertStreamOperations(stream);
        }

        internal void InsertStreamOperations(LispTextStream stream)
        {
            var operation = LispList.FromItems(
                new LispResolvedSymbol("COMMON-LISP", "EVAL-STREAM", true),
                stream,
                LispNilList.Instance); // last value
            InsertExpressionOperation(operation);
        }

        internal void InsertExpressionOperation(LispObject expression)
        {
            InsertOperation(new LispEvaluatorPopArgument());
            InsertOperation(new LispEvaluatorReturnImmediate());
            InsertOperation(new LispEvaluatorObjectExpression(expression));
        }

        internal void InsertOperation(ILispEvaluatorOperation operation, int position = 0)
        {
            if (position == 0)
            {
                _operationStack.Push(operation);
            }
            else
            {
                var toSave = new Stack<ILispEvaluatorOperation>();
                for (int i = 0; i < position; i++)
                {
                    toSave.Push(_operationStack.Pop());
                }

                _operationStack.Push(operation);
                for (int i = 0; i < position; i++)
                {
                    _operationStack.Push(toSave.Pop());
                }
            }

            MaxOperationCount = Math.Max(MaxOperationCount, (ulong)_operationStack.Count);
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
            if (_operationStack.Count > 0)
            {
                operation = _operationStack.Pop();
                return true;
            }

            return false;
        }

        public void ReportError(LispError error, LispObject parent = null, bool insertPop = false)
        {
            if (!error.SourceLocation.HasValue)
            {
                error.SourceLocation = parent?.SourceLocation;
            }

            if (error.StackFrame is null)
            {
                error.StackFrame = StackFrame;
            }

            InsertOperation(new LispEvaluatorThrowCondition(error));
            if (insertPop)
            {
                InsertOperation(new LispEvaluatorPopArgument());
            }
        }

        internal void PushArgument(LispObject argument)
        {
            _argumentStack.Push(argument);

            MaxArgumentCount = Math.Max(MaxArgumentCount, (ulong)_argumentStack.Count);
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

        internal bool TryRewindAndFindErrorHandler(LispObject errorObject, out (LispResolvedSymbol typeSpec, LispResolvedSymbol argument, LispObject form) handlerSet)
        {
            if (errorObject is not LispError error)
            {
                throw new NotSupportedException("Only actual error objects are currently supported");
            }

            handlerSet = default;
            while (TryDequeueOperation(out var candidateHandler))
            {
                switch (candidateHandler)
                {
                    case LispEvaluatorSetStackFrame sf:
                        StackFrame = sf.StackFrame;
                        break;
                    case LispEvaluatorPopArgument _:
                        TryPopArgument(out var a);
                        break;
                    case LispEvaluatorHandlerCaseGuard handlerCaseGuard:
                        foreach (var candidateHandlerSet in handlerCaseGuard.Handlers)
                        {
                            // this hard-coded set is terrible
                            switch (candidateHandlerSet.typeSpec.Value)
                            {
                                case "COMMON-LISP:END-OF-FILE":
                                    if (error.IsEof())
                                    {
                                        handlerSet = candidateHandlerSet;
                                        return true;
                                    }
                                    break;
                                case "COMMON-LISP:ERROR":
                                    handlerSet = candidateHandlerSet;
                                    return true;
                            }
                        }
                        break;
                }
            }

            return false;
        }

        internal static LispExecutionState CreateExecutionState(LispStackFrame stackFrame, bool allowHalting)
        {
            var executionState = new LispExecutionState(stackFrame, allowHalting);
            return executionState;
        }
    }
}
