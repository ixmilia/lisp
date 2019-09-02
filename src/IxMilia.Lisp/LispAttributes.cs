using System;

namespace IxMilia.Lisp
{
    public class LispMacroAttribute : Attribute
    {
        public string Name { get; }

        public LispMacroAttribute(string methodName)
        {
            Name = methodName;
        }
    }

    public class LispFunctionAttribute : Attribute
    {
        public string Name { get; }

        public LispFunctionAttribute(string methodName)
        {
            Name = methodName;
        }
    }
}
