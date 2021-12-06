using System;
using System.Collections.Generic;
using System.IO;

namespace IxMilia.Lisp
{
    public class LispStackFrame
    {
        protected const string TString = "T";
        protected const string NilString = "NIL";
        protected const string TerminalIOString = "*TERMINAL-IO*";

        public LispMacroOrFunction Function { get; }
        public string FunctionName { get; }
        public LispStackFrame Parent { get; }
        public LispSourceLocation? SourceLocation { get; private set; }

        public virtual LispRootStackFrame Root { get; }
        public virtual int Depth { get; }

        public LispObject T => GetValue<LispSymbol>(TString);
        public LispObject Nil => GetValue<LispList>(NilString);
        public LispStream TerminalIO => GetValue<LispStream>(TerminalIOString);

        private Dictionary<string, LispObject> _values = new Dictionary<string, LispObject>();

        private LispStackFrame(LispStackFrame parent)
        {
            Parent = parent;
            Root = Parent?.Root;
            Depth = (Parent?.Depth ?? LispRootStackFrame.RootStackDepth) + 1;
        }

        internal LispStackFrame(LispMacroOrFunction function, LispStackFrame parent)
            : this(parent)
        {
            Function = function;
            FunctionName = function.Name;
        }

        protected LispStackFrame(string functionName, LispStackFrame parent)
            : this(parent)
        {
            FunctionName = functionName;
        }

        internal void CopyLocalsToParentForTailCall(HashSet<string> invocationArgumentNames)
        {
            foreach (var valuePair in _values)
            {
                if (invocationArgumentNames.Contains(valuePair.Key))
                {
                    Parent?.SetValue(valuePair.Key, valuePair.Value);
                }
            }
        }

        public void SetValue(string name, LispObject value)
        {
            _values[name] = value;
            Root.OnValueSet(name, value, this);
        }

        internal void SetValueInParentScope(string name, LispObject value)
        {
            var target = Parent ?? this;
            target.SetValue(name, value);
        }

        public LispObject GetValue(string name)
        {
            if (_values.TryGetValue(name, out var value))
            {
                return value;
            }

            return Parent?.GetValue(name);
        }

        public TObject GetValue<TObject>(string name) where TObject : LispObject
        {
            return (TObject)GetValue(name);
        }

        public void DeleteValue(string name)
        {
            _values.Remove(name);
        }

        public LispObject Eval(LispObject obj)
        {
            return EvalMany(new LispObject[] { obj });
        }

        public LispObject EvalMany(IEnumerable<LispObject> nodes)
        {
            var executionState = LispExecutionState.CreateExecutionState(this, nodes, useTailCalls: false, allowHalting: false, createDribbleInstructions: false);
            LispEvaluator.Evaluate(executionState);
            return executionState.LastResult;
        }

        internal void UpdateCallStackLocation(LispSourceLocation? sourceLocation)
        {
            SourceLocation = sourceLocation;
        }

        public override string ToString()
        {
            var filePath = SourceLocation?.FilePath == null
                ? string.Empty
                : $" in '{SourceLocation.Value.FilePath}'";
            return $"  at {FunctionName}{filePath}: ({SourceLocation?.Line}, {SourceLocation?.Column})\n{Parent}";
        }
    }

    public class LispRootStackFrame : LispStackFrame
    {
        private const string DribbleStreamString = "(DRIBBLE-STREAM)"; // should be un-utterable
        internal const int RootStackDepth = 0;

        public event EventHandler<LispFunctionEnteredEventArgs> FunctionEntered;
        public event EventHandler<LispFunctionReturnedEventArgs> FunctionReturned;
        public event EventHandler<LispMacroExpandedEventArgs> MacroExpanded;
        public event EventHandler<LispEvaluatingExpressionEventArgs> EvaluatingExpression;
        public event EventHandler<LispEvaluatedExpressionEventArgs> EvaluatedExpression;
        public event EventHandler<LispValueSetEventArgs> ValueSet;
        public event EventHandler<LispErrorOccuredEventArgs> ErrorOccured;
        public event EventHandler<LispEvaluationHaltedEventArgs> EvaluationHalted;

        internal LispFileStream DribbleStream
        {
            get => GetValue<LispFileStream>(DribbleStreamString);
            set
            {
                if (value is null)
                {
                    DeleteValue(DribbleStreamString);
                }
                else
                {
                    SetValue(DribbleStreamString, value);
                }
            }
        }

        public override LispRootStackFrame Root => this;
        public override int Depth => RootStackDepth;

        internal LispRootStackFrame(TextReader input, TextWriter output)
            : base("(ROOT)", null)
        {
            SetValue(TString, new LispSymbol(TString));
            SetValue(NilString, LispNilList.Instance);
            SetValue(TerminalIOString, new LispStream(TerminalIOString, input, output));
        }

        internal bool OnFunctionEnter(LispStackFrame frame, LispObject[] functionArguments)
        {
            var args = new LispFunctionEnteredEventArgs(frame, functionArguments);
            FunctionEntered?.Invoke(this, args);
            return args.HaltExecution;
        }

        internal bool OnFunctionReturn(LispFunction function, LispStackFrame frame, LispObject returnValue)
        {
            var args = new LispFunctionReturnedEventArgs(function, frame, returnValue);
            FunctionReturned?.Invoke(this, args);
            return args.HaltExecution;
        }

        internal bool OnMacroExpanded(LispMacro macro, LispStackFrame frame, LispObject[] expandedBody)
        {
            var args = new LispMacroExpandedEventArgs(macro, frame, expandedBody);
            MacroExpanded?.Invoke(this, args);
            return args.HaltExecution;
        }

        internal bool OnEvaluatingExpression(LispObject expression, LispStackFrame frame)
        {
            var args = new LispEvaluatingExpressionEventArgs(expression, frame);
            EvaluatingExpression?.Invoke(this, args);
            return args.HaltExecution;
        }

        internal bool OnEvaluatedExpression(LispObject expression, LispObject result, LispStackFrame frame)
        {
            var args = new LispEvaluatedExpressionEventArgs(expression, result, frame);
            EvaluatedExpression?.Invoke(this, args);
            return args.HaltExecution;
        }

        internal void OnValueSet(string name, LispObject value, LispStackFrame frame)
        {
            var args = new LispValueSetEventArgs(name, value, frame);
            ValueSet?.Invoke(this, args);
        }

        internal void OnErrorOccured(LispError error, LispStackFrame frame)
        {
            var args = new LispErrorOccuredEventArgs(error, frame);
            ErrorOccured?.Invoke(this, args);
        }

        internal void OnHalted(LispEvaluationState evaluationState)
        {
            if (evaluationState != LispEvaluationState.Complete)
            {
                var args = new LispEvaluationHaltedEventArgs(evaluationState);
                EvaluationHalted?.Invoke(this, args);
            }
        }
    }
}
