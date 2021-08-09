﻿using System.Collections.Generic;
using System.Linq;

namespace IxMilia.Lisp
{
    public class LispExecutionState
    {
        private Stack<LispObject> _argumentStack = new Stack<LispObject>();
        private List<ILispEvaluatorOperation> _operationQueue;
        internal LispStackFrame StackFrame { get; set; }

        public LispObject LastResult => _argumentStack.Count > 0 ? _argumentStack.Peek() : null;

        private LispExecutionState(IEnumerable<ILispEvaluatorOperation> operations, LispStackFrame stackFrame)
        {
            _operationQueue = operations.ToList();
            StackFrame = stackFrame;
        }

        internal ILispEvaluatorOperation PeekOperation()
        {
            if (_operationQueue.Count > 0)
            {
                return _operationQueue[0];
            }

            return null;
        }

        internal void InsertOperation(ILispEvaluatorOperation operation)
        {
            _operationQueue.Insert(0, operation);
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

            InsertOperation(new LispEvaluatorObjectExpression(error));
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

        internal static LispExecutionState CreateExecutionState(LispStackFrame stackFrame, IEnumerable<LispObject> nodes)
        {
            var operations = new List<ILispEvaluatorOperation>();
            var nodeList = nodes.ToList();
            for (int i = 0; i < nodeList.Count; i++)
            {
                var node = nodeList[i];
                operations.Add(new LispEvaluatorDribbleEnter(node));
                operations.Add(new LispEvaluatorObjectExpression(node));
                operations.Add(new LispEvaluatorDribbleExit());
                if (i != nodeList.Count - 1)
                {
                    operations.Add(new LispEvaluatorPopArgument());
                }
            }

            return new LispExecutionState(operations, stackFrame);
        }
    }
}
