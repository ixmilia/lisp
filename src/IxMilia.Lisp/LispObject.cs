using System.Collections.Generic;
using System.Linq;

namespace IxMilia.Lisp
{
    public abstract class LispObject
    {
        public int Line { get; internal set; }
        public int Column { get; internal set; }
    }

    public class LispError : LispObject
    {
        public string Message { get; }
        public LispStackFrame StackFrame { get; internal set; }

        public LispError(string message)
            : this(message, null)
        {
        }

        internal LispError(string message, LispStackFrame stackFrame)
        {
            Message = message;
            StackFrame = stackFrame;
        }

        public override string ToString()
        {
            return $"{Message}: {StackFrame}";
        }
    }

    public class LispSymbol : LispObject
    {
        public bool IsQuoted { get; set; }
        public string Value { get; set; }

        public LispSymbol(string value)
            : this(false, value)
        {
        }

        public LispSymbol(bool isQuoted, string value)
        {
            IsQuoted = isQuoted;
            Value = value;
        }

        public override string ToString()
        {
            return Value;
        }

        public static bool operator ==(LispSymbol a, LispSymbol b)
        {
            return a?.Value == b?.Value;
        }

        public static bool operator !=(LispSymbol a, LispSymbol b)
        {
            return !(a == b);
        }

        public override bool Equals(object obj)
        {
            return obj is LispSymbol && this == (LispSymbol)obj;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }
    }

    public class LispNumber : LispObject
    {
        public double Value { get; set; }

        public LispNumber(double value)
        {
            Value = value;
        }

        public bool IsZero => Value == 0.0;
        public bool IsEven => (Value - (int)Value == 0.0) && (int)Value % 2 == 0;
        public bool IsOdd => (Value - (int)Value == 0.0) && (int)Value % 2 != 0;

        public override string ToString()
        {
            return Value.ToString();
        }

        public static bool operator ==(LispNumber a, LispNumber b)
        {
            return a?.Value == b?.Value;
        }

        public static bool operator !=(LispNumber a, LispNumber b)
        {
            return !(a == b);
        }

        public static LispNumber operator +(LispNumber a, LispNumber b)
        {
            return new LispNumber(a.Value + b.Value);
        }

        public static LispNumber operator -(LispNumber a, LispNumber b)
        {
            return new LispNumber(a.Value - b.Value);
        }

        public static LispNumber operator *(LispNumber a, LispNumber b)
        {
            return new LispNumber(a.Value * b.Value);
        }

        public static LispNumber operator /(LispNumber a, LispNumber b)
        {
            return new LispNumber(a.Value / b.Value);
        }

        public override bool Equals(object obj)
        {
            return obj is LispNumber && this == (LispNumber)obj;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }
    }

    public class LispString : LispObject
    {
        public string Value { get; set; }

        public LispString(string value)
        {
            Value = value;
        }

        public override string ToString()
        {
            return Value;
        }

        public static bool operator ==(LispString a, LispString b)
        {
            return a?.Value == b?.Value;
        }

        public static bool operator !=(LispString a, LispString b)
        {
            return !(a == b);
        }

        public override bool Equals(object obj)
        {
            return obj is LispString && this == (LispString)obj;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }
    }

    public class LispList : LispObject
    {
        public bool IsQuoted { get; set; }
        public List<LispObject> Value { get; set; }

        public LispList(params LispObject[] value)
            : this((IEnumerable<LispObject>)value)
        {
        }

        public LispList(IEnumerable<LispObject> value)
            : this(false, value)
        {
        }

        public LispList(bool isQuoted, params LispObject[] value)
            : this(isQuoted, (IEnumerable<LispObject>)value)
        {
        }

        public LispList(bool isQuoted, IEnumerable<LispObject> value)
        {
            IsQuoted = isQuoted;
            Value = value.ToList();
        }

        public override string ToString()
        {
            return $"{(IsQuoted ? "'" : string.Empty)}({string.Join(" ", Value)})";
        }

        public static bool operator ==(LispList a, LispList b)
        {
            if (a?.Value.Count == b?.Value.Count)
            {
                for (int i = 0; i < a.Value.Count; i++)
                {
                    if (!a.Value[i].Equals(b.Value[i]))
                    {
                        return false;
                    }
                }

                return true;
            }

            return false;
        }

        public static bool operator !=(LispList a, LispList b)
        {
            return !(a == b);
        }

        public override bool Equals(object obj)
        {
            return obj is LispList && this == (LispList)obj;
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }
    }

    public class LispFunction : LispObject
    {
        // TODO: make these read only collections
        public string Name { get; }
        public string[] Arguments { get; }
        public LispObject[] Commands { get; }

        public LispFunction(string name, IEnumerable<string> arguments, IEnumerable<LispObject> commands)
        {
            Name = name;
            Arguments = arguments.ToArray();
            Commands = commands.ToArray();
        }

        public override string ToString()
        {
            return Name;
        }
    }

    public class LispMacro : LispObject
    {
        // TODO: make these read only collections
        public string Name { get; }
        public string[] Arguments { get; }
        public LispObject[] Body { get; }

        public LispMacro(string name, IEnumerable<string> arguments, IEnumerable<LispObject> body)
        {
            Name = name;
            Arguments = arguments.ToArray();
            Body = body.ToArray();
        }

        public override string ToString()
        {
            return Name;
        }
    }
}
