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

        public LispInvocableObject Function { get; }
        public LispResolvedSymbol FunctionSymbol { get; }
        public LispStackFrame Parent { get; }
        public LispSourceLocation? SourceLocation { get; private set; }

        public virtual LispRootStackFrame Root => Parent.Root;
        public virtual int Depth { get; }

        private Dictionary<string, (LispResolvedSymbol Symbol, LispObject Value)> _values = new Dictionary<string, (LispResolvedSymbol, LispObject)>();

        protected LispStackFrame(LispResolvedSymbol functionSymbol, LispStackFrame parent)
        {
            FunctionSymbol = functionSymbol;
            Parent = parent;
            Depth = (Parent?.Depth ?? LispRootStackFrame.RootStackDepth) + 1;
        }

        internal LispStackFrame(LispInvocableObject function, LispStackFrame parent)
            : this(function.NameSymbol, parent)
        {
            Function = function;
        }

        internal void UpdateCallStackLocation(LispSourceLocation? sourceLocation)
        {
            SourceLocation = sourceLocation;
        }

        internal virtual void CopyLocalsToParentForTailCall(LispPackage currentPackage, HashSet<string> invocationArgumentNames)
        {
            foreach (var valuePair in _values)
            {
                if (invocationArgumentNames.Contains(valuePair.Key))
                {
                    Parent?.SetValue(LispSymbol.CreateFromString(valuePair.Key).Resolve(currentPackage), valuePair.Value.Value);
                }
            }
        }

        internal void SetValueInParentScope(LispResolvedSymbol symbol, LispObject value)
        {
            var target = Parent ?? this;
            target.SetValue(symbol, value);
        }

        public virtual void SetValue(LispResolvedSymbol symbol, LispObject value)
        {
            _values[symbol.Value] = (symbol, value);
            Root.OnValueSet(symbol, value, this);
        }

        public virtual LispObject GetValue(LispResolvedSymbol symbol)
        {
            if (_values.TryGetValue(symbol.Value, out var value))
            {
                return value.Value;
            }

            return Parent?.GetValue(symbol);
        }

        public TObject GetValue<TObject>(LispResolvedSymbol symbol) where TObject : LispObject
        {
            return (TObject)GetValue(symbol);
        }

        public virtual LispBoundValues GetBoundValues()
        {
            var boundValues = Parent is object
                ? Parent.GetBoundValues()
                : new LispBoundValues();
            foreach (var value in _values.Values)
            {
                boundValues.SetBoundValue(value.Symbol, value.Value);
            }

            return boundValues;
        }

        public virtual void DeleteValue(LispResolvedSymbol symbol)
        {
            _values.Remove(symbol.Value);
        }

        public override string ToString()
        {
            var filePath = SourceLocation?.FilePath == null
                ? string.Empty
                : $" in '{SourceLocation.Value.FilePath}'";
            return $"  at {FunctionSymbol.LocalName}{filePath}: ({SourceLocation?.Start.Line}, {SourceLocation?.Start.Column})\n{Parent}";
        }
    }

    public class LispRootStackFrame : LispStackFrame
    {
        public const string CommonLispPackageName = "COMMON-LISP";
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

        private Dictionary<string, LispPackage> _packages = new Dictionary<string, LispPackage>();
        private LispPackage _commonLispPackage;
        private LispPackage _keywordPackage;

        internal LispFileStream DribbleStream
        {
            get => _commonLispPackage.GetValue<LispFileStream>(DribbleStreamString);
            set
            {
                if (value is null)
                {
                    _commonLispPackage.DeleteValue(DribbleStreamString);
                }
                else
                {
                    _commonLispPackage.SetValue(DribbleStreamString, value);
                }
            }
        }

        public override LispRootStackFrame Root => this;
        public override int Depth => RootStackDepth;

        public LispObject T => _commonLispPackage.GetValue<LispSymbol>(TString);
        public LispObject Nil => _commonLispPackage.GetValue<LispNilList>(NilString);
        public LispTextStream TerminalIO => _commonLispPackage.GetValue<LispTextStream>(TerminalIOString);

        internal LispRootStackFrame(TextReader input, TextWriter output)
            : base(new LispResolvedSymbol("(ROOT)", "(ROOT)", isPublic: true), null)
        {
            _commonLispPackage = AddPackage(CommonLispPackageName);
            _keywordPackage = new LispKeywordPackage();
            _packages.Add(_keywordPackage.Name, _keywordPackage);

            var tSymbol = new LispResolvedSymbol(CommonLispPackageName, TString, isPublic: true);
            SetValue(tSymbol, tSymbol);

            var nilSymbol = new LispResolvedSymbol(CommonLispPackageName, NilString, isPublic: true);
            SetValue(nilSymbol, LispNilList.Instance);

            var terminalIoSymbol = new LispResolvedSymbol(CommonLispPackageName, TerminalIOString, isPublic: true);
            var terminalIoStream = new LispTextStream(TerminalIOString, input, output);
            SetValue(terminalIoSymbol, terminalIoStream);
        }

        public LispPackage AddPackage(string packageName, IEnumerable<LispPackage> inheritedPackages = null)
        {
            var package = new LispPackage(packageName, inheritedPackages);
            _packages.Add(packageName, package);
            return package;
        }

        public LispPackage GetPackage(string packageName)
        {
            if (_packages.TryGetValue(packageName, out var package))
            {
                return package;
            }

            return null;
        }

        public override void SetValue(LispResolvedSymbol symbol, LispObject value)
        {
            SetValue(symbol, value, createPackage: false);
        }

        internal LispObject SetValue(LispResolvedSymbol symbol, LispObject value, bool createPackage)
        {
            if (!_packages.TryGetValue(symbol.PackageName, out var package))
            {
                if (createPackage)
                {
                    package = AddPackage(symbol.PackageName);
                }
                else
                {
                    throw new Exception("asdf");
                    //return new LispError($"Package {packageName} not defined.");
                }
            }

            package.SetValue(symbol.LocalName, value);
            OnValueSet(symbol, value, this);
            return value;
        }

        public override LispObject GetValue(LispResolvedSymbol symbol)
        {
            if (!_packages.TryGetValue(symbol.PackageName, out var package))
            {
                return null;
            }

            return package.GetValue(symbol.LocalName);
        }

        public override void DeleteValue(LispResolvedSymbol symbol)
        {
            if (_packages.TryGetValue(symbol.PackageName, out var package))
            {
                package.DeleteValue(symbol.LocalName);
            }
        }

        public override LispBoundValues GetBoundValues()
        {
            var boundValues = new LispBoundValues();
            foreach (var package in _packages.Values)
            {
                foreach (var value in package.Values)
                {
                    var symbol = new LispResolvedSymbol(package.Name, value.Key, isPublic: true); // TODO: figure out isPublic
                    boundValues.SetBoundValue(symbol, value.Value);
                }
            }

            return boundValues;
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

        internal bool OnMacroExpanded(LispMacro macro, LispStackFrame frame, LispObject expandedBody)
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

        internal void OnValueSet(LispResolvedSymbol symbol, LispObject value, LispStackFrame frame)
        {
            var args = new LispValueSetEventArgs(symbol, value, frame);
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
