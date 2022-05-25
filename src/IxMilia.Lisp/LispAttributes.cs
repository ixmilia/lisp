using System;

namespace IxMilia.Lisp
{
    public abstract class LispInvokableTargetAttribute : Attribute
    {
        public string Signature { get; set; }
        public string Documentation { get; set; }
    }

    [AttributeUsage(AttributeTargets.Method, AllowMultiple = true)]
    public class LispSpecialOperatorAttribute : LispInvokableTargetAttribute
    {
        public string Name { get; }

        public LispSpecialOperatorAttribute(string operatorName)
        {
            Name = operatorName;
        }
    }

    [AttributeUsage(AttributeTargets.Method, AllowMultiple = true)]
    public class LispMacroAttribute : LispInvokableTargetAttribute
    {
        public string Name { get; }

        public LispMacroAttribute(string methodName)
        {
            Name = methodName;
        }
    }

    [AttributeUsage(AttributeTargets.Method, AllowMultiple = true)]
    public class LispFunctionAttribute : LispInvokableTargetAttribute
    {
        public string Name { get; }

        public LispFunctionAttribute(string methodName)
        {
            Name = methodName;
        }
    }
}
