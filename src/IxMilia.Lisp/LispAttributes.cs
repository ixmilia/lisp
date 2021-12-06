using System;

namespace IxMilia.Lisp
{
    [AttributeUsage(AttributeTargets.Method, AllowMultiple = true)]
    public class LispSpecialOperatorAttribute : Attribute
    {
        public string Name { get; }

        public LispSpecialOperatorAttribute(string operatorName)
        {
            Name = operatorName;
        }
    }

    [AttributeUsage(AttributeTargets.Method, AllowMultiple = true)]
    public class LispMacroAttribute : Attribute
    {
        public string Name { get; }

        public LispMacroAttribute(string methodName)
        {
            Name = methodName;
        }
    }

    [AttributeUsage(AttributeTargets.Method, AllowMultiple = true)]
    public class LispFunctionAttribute : Attribute
    {
        public string Name { get; }

        public LispFunctionAttribute(string methodName)
        {
            Name = methodName;
        }
    }
}
