using System;

namespace IxMilia.Lisp
{
    public class LispValueAttribute : Attribute
    {
        public string Name { get; }

        public LispValueAttribute(string methodName)
        {
            Name = methodName;
        }
    }
}
