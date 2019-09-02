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
        public string Value { get; set; }

        public LispSymbol(string value)
        {
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
        public List<LispObject> Value { get; set; }

        public LispList(params LispObject[] values)
            : this((IEnumerable<LispObject>)values)
        {
        }

        public LispList(IEnumerable<LispObject> value)
        {
            Value = value.ToList();
        }

        public override string ToString()
        {
            return $"({string.Join(" ", Value)})";
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
        public string[] Arguments { get; }
        public LispObject[] Commands { get; }

        public LispFunction(IEnumerable<string> arguments, IEnumerable<LispObject> commands)
        {
            Arguments = arguments.ToArray();
            Commands = commands.ToArray();
        }

        public override string ToString()
        {
            return "(function)";
        }
    }
}
