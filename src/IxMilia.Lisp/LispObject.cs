using System;
using System.Collections.Generic;
using System.Linq;
using IxMilia.Lisp.Tokens;

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
            var frame = StackFrame == null ? string.Empty : $":\n{StackFrame}";
            return $"{Message}{frame}";
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

    internal class LispForwardListReference : LispObject
    {
        public LispForwardReferenceToken ForwardReference { get; }
        public LispList List { get; }

        public LispForwardListReference(LispForwardReferenceToken forwardReference, LispList list)
        {
            ForwardReference = forwardReference;
            List = list;
        }
    }

    public class LispList : LispObject
    {
        public bool IsQuoted { get; internal set; }
        public virtual LispObject Value { get; }
        public virtual LispObject Next { get; }
        public virtual int Length { get; }
        public virtual bool IsNil { get; }

        public virtual bool IsProperList
        {
            get
            {
                var list = this;
                while (!list.IsNil)
                {
                    if (list.Next is LispList next)
                    {
                        list = next;
                    }
                    else
                    {
                        return false;
                    }
                }

                return true;
            }
        }

        internal LispList()
        {
        }

        public LispList(LispObject value)
            : this(value, LispNilList.Instance, false)
        {
        }

        public LispList(LispObject value, LispObject next)
            : this(value, next, false)
        {
        }

        public LispList(LispObject value, LispObject next, bool isQuoted)
        {
            Value = value;
            Next = next;
            IsQuoted = isQuoted;
            IsNil = false;
            if (next is LispList list)
            {
                Length = list.Length + 1;
            }
            else
            {
                Length = 1; // `Value`; improper tail isn't counted
            }
        }

        public static LispList FromItems(params LispObject[] items)
        {
            return FromEnumerable(items);
        }

        public static LispList FromEnumerable(IEnumerable<LispObject> items)
        {
            LispList list = LispNilList.Instance;
            foreach (var item in items.Reverse())
            {
                list = new LispList(item, list);
            }

            return list;
        }

        public static LispList FromItemsImproper(LispObject first, LispObject second, params LispObject[] rest)
        {
            return FromEnumerableImproper(first, second, rest);
        }

        public static LispList FromEnumerableImproper(LispObject first, LispObject second, IEnumerable<LispObject> rest)
        {
            // guaranteed to contain at least 2 items
            var allItems = new[] { first, second }.Concat(rest).ToList();

            var list = new LispList(allItems[allItems.Count - 2], allItems[allItems.Count - 1]);
            for (int i = allItems.Count - 3; i >= 0; i--)
            {
                list = new LispList(allItems[i], list);
            }

            return list;
        }

        public IList<LispObject> ToList()
        {
            var items = new List<LispObject>();
            var head = this;
            while (!head.IsNil)
            {
                items.Add(head.Value);
                if (head.Next is LispList next)
                {
                    head = next;
                }
                else
                {
                    items.Add(head.Next);
                    break;
                }
            }

            return items;
        }

        public override string ToString()
        {
            return $"{(IsQuoted ? "'" : string.Empty)}({Value}{NextToString()}";
        }

        protected virtual string ToStringTail()
        {
            return $" {Value}{NextToString()}";
        }

        private string NextToString()
        {
            string nextString;
            if (Next is LispList list)
            {
                nextString = list.ToStringTail();
            }
            else
            {
                nextString = $" . {Next})";
            }

            return nextString;
        }

        public static bool operator ==(LispList a, LispList b)
        {
            if ((object)a == null && (object)b == null)
            {
                return true;
            }
            if ((object)a == null || (object)b == null)
            {
                return false;
            }
            if (ReferenceEquals(a, b))
            {
                return true;
            }
            if (a.IsNil && b.IsNil)
            {
                return true;
            }
            if (a.Value.Equals(b.Value))
            {
                return a.Next.Equals(b.Next);
            }
            else
            {
                return false;
            }
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

    internal class LispCircularList : LispList
    {
        private LispObject _value;
        private LispObject _next;
        private int _length;
        private bool _isProperList;

        public override LispObject Value => _value;
        public override LispObject Next => _next;
        public override int Length => _length;
        public override bool IsProperList => _isProperList;

        public LispCircularList()
        {
        }

        public void ApplyForCircularReference(LispList otherList, bool isProperList)
        {
            _value = otherList.Value;
            _next = otherList.Next;
            _length = -Math.Abs(otherList.Length);
            _isProperList = isProperList;
        }

        public override string ToString()
        {
            // TODO: find a better way of displaying this
            return "(...)";
        }

        protected override string ToStringTail()
        {
            // TODO: find a better way of displaying this
            return "...)";
        }
    }

    public class LispNilList : LispList
    {
        public readonly static LispNilList Instance = new LispNilList();

        public override LispObject Value => this;
        public override LispObject Next => this;
        public override int Length => 0;
        public override bool IsNil => true;

        private LispNilList()
        {
        }

        public override string ToString()
        {
            return "()";
        }

        protected override string ToStringTail()
        {
            return ")";
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
