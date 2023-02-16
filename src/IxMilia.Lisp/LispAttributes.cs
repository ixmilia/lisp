using System;

namespace IxMilia.Lisp
{
    public abstract class LispInvokableTargetAttribute : Attribute
    {
        public string Name { get; }
        public string Signature { get; set; }
        public string Documentation { get; set; }

        protected LispInvokableTargetAttribute(string name)
        {
            Name = name;
        }
    }

    [AttributeUsage(AttributeTargets.Method, AllowMultiple = true)]
    public class LispSpecialOperatorAttribute : LispInvokableTargetAttribute
    {
        public LispSpecialOperatorAttribute(string operatorName)
            : base(operatorName)
        {
        }
    }

    [AttributeUsage(AttributeTargets.Method, AllowMultiple = true)]
    public class LispMacroAttribute : LispInvokableTargetAttribute
    {
        public LispMacroAttribute(string methodName)
            : base(methodName)
        {
        }
    }

    [AttributeUsage(AttributeTargets.Method, AllowMultiple = true)]
    public class LispFunctionAttribute : LispInvokableTargetAttribute
    {
        public LispFunctionAttribute(string methodName)
            : base(methodName)
        {
        }
    }
}
