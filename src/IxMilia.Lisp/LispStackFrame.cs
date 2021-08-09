using System;
using System.Collections.Generic;
using System.IO;

namespace IxMilia.Lisp
{
    public class LispStackFrame
    {
        protected const string TString = "t";
        protected const string NilString = "nil";
        protected const string TerminalIOString = "*terminal-io*";

        public string FunctionName { get; }
        public LispStackFrame Parent { get; }
        public LispSourceLocation? SourceLocation { get; private set; }

        public virtual LispRootStackFrame Root { get; }
        public virtual int Depth { get; }

        public LispObject T => GetValue<LispSymbol>(TString);
        public LispObject Nil => GetValue<LispList>(NilString);
        public LispStream TerminalIO => GetValue<LispStream>(TerminalIOString);

        private Dictionary<string, LispObject> _values = new Dictionary<string, LispObject>();

        public LispStackFrame(string functionName, LispStackFrame parent)
        {
            FunctionName = functionName;
            Parent = parent;
            Root = Parent?.Root;
            Depth = (Parent?.Depth ?? LispRootStackFrame.RootStackDepth) + 1;
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
            var executionState = LispExecutionState.CreateExecutionState(this, nodes);
            var resultExecutionState = LispEvaluator.Evaluate(executionState);
            return resultExecutionState.LastResult;
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
        private const string DribbleStreamString = "(dribble-stream)"; // should be un-utterable
        internal const int RootStackDepth = 0;

        public event EventHandler<LispFunctionEnteredEventArgs> FunctionEntered;
        public event EventHandler<LispFunctionReturnedEventArgs> FunctionReturned;

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
            : base("(root)", null)
        {
            SetValue(TString, new LispSymbol(TString));
            SetValue(NilString, LispNilList.Instance);
            SetValue(TerminalIOString, new LispStream(TerminalIOString, input, output));
        }

        internal void OnFunctionEnter(LispStackFrame frame, LispObject[] functionArguments)
        {
            var args = new LispFunctionEnteredEventArgs(frame, functionArguments);
            FunctionEntered?.Invoke(this, args);
        }

        internal void OnFunctionReturn(LispMacroOrFunction invocationObject, LispStackFrame frame, LispObject returnValue)
        {
            var args = new LispFunctionReturnedEventArgs(invocationObject, frame, returnValue);
            FunctionReturned?.Invoke(this, args);
        }
    }
}
