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

        public static bool IsT(this LispObject o)
        {
            return o is LispResolvedSymbol symbol
                && symbol.PackageName == "COMMON-LISP"
                && symbol.LocalName == "T";
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

        public static bool IsEof(this LispObject o)
        {
            switch (o)
            {
                case LispError error:
                    return error.Message == "EOF";
                default:
                    return false;
            }
        }

        public static LispObject LastItem(this LispList list)
        {
            while (true)
            {
                if (list.IsNil())
                {
                    // went too far?
                    return null;
                }

                if (list.Next.IsNil())
                {
                    // found it
                    return list.Value;
                }

                if (list.Next is LispList nextList)
                {
                    // keep going
                    list = nextList;
                }
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
(DEFUN {codeFunction.NameSymbol.ToDisplayString(host.CurrentPackage)} ({codeFunction.ArgumentCollection.ToDisplayString(host.CurrentPackage)}) ...)
```

{codeFunction.Documentation}".Trim();
                case LispNativeFunction nativeFunction:
                    return $@"
``` lisp
; <native function>
; <in module {nativeFunction.NameSymbol.PackageName}>
(DEFUN {nativeFunction.NameSymbol.ToDisplayString(host.CurrentPackage)} ({nativeFunction.Signature}) ...)
```

{nativeFunction.Documentation}".Trim();
                case LispCodeMacro codeMacro:
                    return $@"
``` lisp
; <in module {codeMacro.NameSymbol.PackageName}>
(DEFMACRO {codeMacro.NameSymbol.ToDisplayString(host.CurrentPackage)} ({codeMacro.ArgumentCollection.ToDisplayString(host.CurrentPackage)}) ...)
```

{codeMacro.Documentation}".Trim();
                case LispNativeMacro nativeMacro:
                    return $@"
``` lisp
; <native macro>
; <in module {nativeMacro.NameSymbol.PackageName}>
(DEFMACRO {nativeMacro.NameSymbol.ToDisplayString(host.CurrentPackage)} ({nativeMacro.Signature}) ...)
```

{nativeMacro.Documentation}".Trim();
                case LispSpecialOperator specialOperator:
                    return $@"
``` lisp
; <special operator>
; <in module {specialOperator.NameSymbol.PackageName}>
(DEFSPECIAL {specialOperator.NameSymbol.ToDisplayString(host.CurrentPackage)} ({specialOperator.Signature}) ...)
```

{specialOperator.Documentation}".Trim();
                case LispResolvedSymbol symbol:
                    // TODO: don't display current package qualifier
                    return $"`{symbol.Value}`: {host.GetValue(symbol.Value)}";
                default:
                    return obj?.ToDisplayString(host.CurrentPackage);
            }
        }

        public static int? GetBreakpointLine(this LispObject obj)
        {
            if (!obj.SourceLocation.HasValue)
            {
                // don't know where we are, obviously can't report a possible breakpoint line
                return null;
            }

            if (obj.Parent is null)
            {
                // if we're a top-level expression, we're already there
                return obj.SourceLocation.Value.Start.Line;
            }

            // navigate to farthest up parent still on the line...
            var candidate = obj;
            while (true)
            {
                if (candidate.Parent is null)
                {
                    // can't go any further
                    break;
                }

                if (candidate.Parent.SourceLocation.HasValue &&
                    candidate.Parent.SourceLocation.Value.FilePath == obj.SourceLocation.Value.FilePath &&
                    candidate.Parent.SourceLocation.Value.Start.Line != obj.SourceLocation.Value.Start.Line)
                {
                    // parent was in the same file, but a different line
                    break;
                }

                // keep going
                candidate = candidate.Parent;
            }

            // ...then find the first sibling on that line...
            var firstChildOnLine = candidate
                .Parent
                ?.GetChildren()
                .FirstOrDefault(c =>
                    c.SourceLocation.HasValue &&
                    c.SourceLocation.Value.FilePath == obj.SourceLocation.Value.FilePath &&
                    c.SourceLocation.Value.Start.Line == obj.SourceLocation.Value.Start.Line);

            // ...and only if we're the same object did we find it...
            if (ReferenceEquals(obj, firstChildOnLine))
            {
                return obj.SourceLocation.Value.Start.Line;
            }

            return null;
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
                case LispUnresolvedSymbol us:
                    {
                        var resolvedSymbol = us.Resolve(host.CurrentPackage);
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
                    }
                    break;
                case LispResolvedSymbol rs:
                    {
                        if (rs.PackageSpan != null)
                        {
                            tokens.Add(new LispToken(LispTokenType.Package, rs.PackageSpan.Item1, rs.PackageSpan.Item2));
                        }

                        if (rs.SymbolSpan != null)
                        {
                            var resolvedValue = host.RootFrame.GetValue(rs);
                            switch (resolvedValue)
                            {
                                case LispFunction _:
                                    tokens.Add(new LispToken(LispTokenType.Function, rs.SymbolSpan.Item1, rs.SymbolSpan.Item2));
                                    break;
                                case LispMacro _:
                                    tokens.Add(new LispToken(LispTokenType.Macro, rs.SymbolSpan.Item1, rs.SymbolSpan.Item2));
                                    break;
                            }
                        }
                        
                    }
                    break;
            }
        }
    }
}
