# deck-tools (Claude Code mod)

`/render-deck <deck> [<deck> ...]` renders a Quarto deck, then runs `scripts/check_slide_overflow.R`
and prints a short report: render errors/warnings and any slide over 900px. It runs with no Claude
turn, so it uses no tokens.

```
/render-deck lec9
/render-deck lab4/lab4 lab4/lab4_instr
```

`lec9` is shorthand for `lec9/lec9`. Run it from the repo root (it uses the session's working directory).

## Load it

```bash
claude --plugin-dir ./mods/deck-tools
```

Check without a session: `claude -p "/render-deck lab10" --plugin-dir ./mods/deck-tools`
Inspect what it does: `claude plugin validate ./mods/deck-tools`

Tested with Claude Code 2.1.300. Needs `quarto`, `Rscript`, and the `chromote` R package on the PATH.

## Known gaps / notes

- Loading needs `--plugin-dir` each session. A project-scoped auto-load setting was not found in the docs;
  `CLAUDE_CODE_PLUGIN_DIRS` works from the environment or `env` in `~/.claude/settings.json`.
- Quarto colors errors with ANSI codes; the mod strips them before filtering.
- The overflow checker takes ~10s per deck plus render time; the command shows nothing until it finishes.
- It does not yet label-check chunks, check for formulas without an explicit form, or commit.
