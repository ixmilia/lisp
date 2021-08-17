using System;
using System.Collections.Generic;

namespace IxMilia.Lisp
{
    public static class LispObjectExtensions
    {
        public static bool IsNil(this LispObject o)
        {
            switch (o)
            {
                case LispNilList _:
                    return true;
                default:
                    return false;
            }
        }

        public static bool IsTLike(this LispObject o)
        {
            switch (o)
            {
                case LispNilList _:
                case null:
                    return false;
                default:
                    return true;
            }
        }

        public static IEnumerable<LispObject> PerformMacroReplacements(this IEnumerable<LispObject> body, IDictionary<string, LispObject> replacements)
        {
            if (replacements is null)
            {
                throw new ArgumentNullException(nameof(replacements));
            }

            foreach (var item in body)
            {
                yield return item.PerformMacroReplacements(replacements);
            }
        }
    }
}
