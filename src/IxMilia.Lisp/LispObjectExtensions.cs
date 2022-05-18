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

        public static string GetMarkdownDisplay(this LispObject obj, LispHost host)
        {
            switch (obj)
            {
                case LispCodeFunction codeFunction:
                    return $@"
``` lisp
; <in module {codeFunction.NameSymbol.PackageName}>
(DEFUN {codeFunction.NameSymbol.ToDisplayString(host.CurrentPackage)} ({codeFunction.ArgumentCollection}) ...)
```

{codeFunction.Documentation}".Trim();
                case LispFunction function:
                    return $@"
``` lisp
; <native function>
; <in module {function.NameSymbol.PackageName}>
(DEFUN {function.NameSymbol.ToDisplayString(host.CurrentPackage)} (...) ...)
```

{function.Documentation}".Trim();
                case LispCodeMacro codeMacro:
                    return $@"
``` lisp
; <in module {codeMacro.NameSymbol.PackageName}>
(DEFMACRO {codeMacro.NameSymbol.ToDisplayString(host.CurrentPackage)} ({codeMacro.ArgumentCollection}) ...)
```

{codeMacro.Documentation}".Trim();
                case LispMacro macro:
                    return $@"
``` lisp
; <native macro>
; <in module {macro.NameSymbol.PackageName}>
(DEFMACRO {macro.NameSymbol.ToDisplayString(host.CurrentPackage)} (...) ...)
```

{macro.Documentation}".Trim();
                case LispResolvedSymbol symbol:
                    // TODO: don't display current package qualifier
                    return $"`{symbol.Value}`: {host.GetValue(symbol.Value)}";
                default:
                    return obj?.ToString();
            }
        }

        public static IEnumerable<LispToken> GetSemanticTokens(this LispObject obj, LispHost host)
        {
            var tokens = new List<LispToken>();
            obj.AddSemanticTokens(host, tokens);
            return tokens;
        }

        private static void AddSemanticTokens(this LispObject obj, LispHost host, List<LispToken> tokens)
        {
            if (!obj.SourceLocation.HasValue)
            {
                return;
            }

            var start = obj.SourceLocation.Value.Start;
            var end = obj.SourceLocation.Value.End;
            switch (obj)
            {
                case LispList list:
                    {
                        foreach (var child in list.GetChildren())
                        {
                            child.AddSemanticTokens(host, tokens);
                        }
                    }
                    break;
                case LispNumber _:
                    tokens.Add(new LispToken(LispTokenType.Number, start, end));
                    break;
                case LispString _:
                    tokens.Add(new LispToken(LispTokenType.String, start, end));
                    break;
                case LispSymbol s:
                    var resolvedSymbol = s.Resolve(host.CurrentPackage);
                    var resolvedValue = host.RootFrame.GetValue(resolvedSymbol);
                    switch (resolvedValue)
                    {
                        case LispFunction _:
                            tokens.Add(new LispToken(LispTokenType.Function, start, end));
                            break;
                        case LispMacro _:
                            tokens.Add(new LispToken(LispTokenType.Macro, start, end));
                            break;
                    }
                    break;
            }
        }
    }
}
