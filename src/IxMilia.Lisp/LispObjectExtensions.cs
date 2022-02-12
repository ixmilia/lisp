using System;
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

        public static LispList PerformMacroReplacements(this IEnumerable<LispObject> body, LispPackage currentPackage, IDictionary<string, LispObject> replacements)
        {
            if (replacements is null)
            {
                throw new ArgumentNullException(nameof(replacements));
            }

            var itemReplacements = body.Select(item => item.PerformMacroReplacements(currentPackage, replacements)).ToList();
            var bodyList = LispList.FromEnumerable(itemReplacements);
            var result = new LispList(LispSymbol.CreateFromString("COMMON-LISP:PROGN"), bodyList);
            return result;
        }

        public static LispObject GetNarrowestChild(this LispObject obj, LispSourcePosition position)
        {
            if (!obj.SourceLocation.HasValue ||
                !obj.SourceLocation.Value.ContainsPosition(position))
            {
                return null;
            }

            var children = obj.GetChildren().ToList();
            if (children.Count == 0)
            {
                // can't delve any deeper
                return obj;
            }

            // otherwise try to find the child that contains the position and recurse
            foreach (var child in children)
            {
                if (child.SourceLocation.HasValue &&
                    child.SourceLocation.Value.ContainsPosition(position))
                {
                    return child.GetNarrowestChild(position);
                }
            }

            // no child was more specific, just return this
            return obj;
        }

        public static string GetMarkdownDisplay(this LispObject obj, LispRepl repl)
        {
            switch (obj)
            {
                case LispCodeFunction codeFunction:
                    return $@"
``` lisp
; <in module {codeFunction.NameSymbol.PackageName}>
(DEFUN {codeFunction.NameSymbol.ToDisplayString(repl.Host.CurrentPackage)} ({codeFunction.ArgumentCollection}) ...)
```

{codeFunction.Documentation}".Trim();
                case LispFunction function:
                    return $@"
``` lisp
; <native function>
; <in module {function.NameSymbol.PackageName}>
(DEFUN {function.NameSymbol.ToDisplayString(repl.Host.CurrentPackage)} (...) ...)
```

{function.Documentation}".Trim();
                case LispResolvedSymbol symbol:
                    // TODO: don't display current package qualifier
                    return $"`{symbol.Value}`: {repl.GetValue(symbol.Value)}";
                default:
                    return obj.ToString();
            }
        }
    }
}
