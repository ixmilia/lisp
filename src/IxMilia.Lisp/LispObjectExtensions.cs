﻿using System;
using System.Collections.Generic;
using System.Linq;

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

        public static LispList PerformMacroReplacements(this IEnumerable<LispObject> body, IDictionary<string, LispObject> replacements)
        {
            if (replacements is null)
            {
                throw new ArgumentNullException(nameof(replacements));
            }

            var itemReplacements = body.Select(item => item.PerformMacroReplacements(replacements)).ToList();
            var bodyList = LispList.FromEnumerable(itemReplacements);
            var result = new LispList(new LispSymbol("PROGN"), bodyList);
            return result;
        }
    }
}
