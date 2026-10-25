#!/usr/bin/env python3
"""Add `#| label:` to every executed {r} chunk that lacks one.

Usage: python3 scripts/add_chunk_labels.py lec9/lec9.qmd [more.qmd ...]

Label = codewindow file name if the chunk sits in one (pnorm.R -> pnorm),
otherwise the nearest preceding slide heading, lowercase-hyphen. The first
chunk after the YAML is labeled `setup`. Duplicates get -2, -3, ...
Existing labels are left alone. Prints what it did.
"""
import re, sys

def slug(text, limit=40):
    text = re.sub(r"\{[^}]*\}", "", text)          # {.smaller} attributes
    text = re.sub(r"`[^`]*`", lambda m: m.group(0).strip("`"), text)
    text = re.sub(r"\$[^$]*\$", "", text)           # inline math
    text = text.lower()
    text = re.sub(r"[^a-z0-9]+", "-", text).strip("-")
    text = text[:limit].strip("-")
    return text or "chunk"

def process(path):
    lines = open(path, encoding="utf-8").read().split("\n")
    out, used = [], set()
    # labels already present
    for l in lines:
        m = re.match(r"^#\|\s*label:\s*(\S+)", l)
        if m: used.add(m.group(1))
    heading = "chunk"
    seen_first = False
    i = 0
    added = []
    in_chunk = False
    while i < len(lines):
        l = lines[i]
        if in_chunk:
            out.append(l)
            if re.match(r"^```\s*$", l): in_chunk = False
            i += 1; continue
        m = re.match(r"^#{1,2} (.+)$", l)
        if m: heading = slug(m.group(1))
        if re.match(r"^```\{r\}\s*$", l):
            # find end of chunk and any existing label
            j = i + 1
            body = []
            while not re.match(r"^```\s*$", lines[j]):
                body.append(lines[j]); j += 1
            has = any(re.match(r"^#\|\s*label:", b) for b in body)
            if not has:
                # codewindow file name within the previous 4 lines
                name = None
                for k in range(max(0, i - 4), i):
                    cm = re.match(r"^([A-Za-z0-9_\-]+)\.(R|r)\s*$", lines[k].strip())
                    if cm: name = slug(cm.group(1))
                if not seen_first and not any(re.match(r"^#{1,2} ", x) for x in lines[:i]):
                    name = "setup"
                elif not seen_first:
                    pass
                base = name or heading
                label, n = base, 2
                while label in used:
                    label = f"{base}-{n}"; n += 1
                used.add(label)
                out.append(l)
                out.append(f"#| label: {label}")
                added.append((i + 1, label))
                seen_first = True
                in_chunk = True
                i += 1; continue
            seen_first = True
            in_chunk = True
        out.append(l)
        if l.startswith("```{r") and l != "```{r}":
            in_chunk = True
        i += 1
    open(path, "w", encoding="utf-8").write("\n".join(out))
    return added

if __name__ == "__main__":
    for p in sys.argv[1:]:
        a = process(p)
        print(f"{p}: {len(a)} labels added")
        for ln, lab in a: print(f"   line {ln}: {lab}")
