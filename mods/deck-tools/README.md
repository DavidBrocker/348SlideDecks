# deck-tools (Claude Code mod)

Three commands: `/render-deck`, `/check-deck`, and `/commit-decks`.

## /render-deck

`/render-deck <deck> [<deck> ...]` renders a Quarto deck, then runs `scripts/check_slide_overflow.R`
and prints a short report: render errors/warnings and any slide over 900px. It runs with no Claude
turn, so it uses no tokens.

```
/render-deck lec9
/render-deck lab4/lab4 lab4/lab4_instr
```

`lec9` is shorthand for `lec9/lec9`. Run it from the repo root (it uses the session's working directory).

## /check-deck

`/check-deck lec9 [lab4/lab4 ...]` runs `scripts/check_deck.R` (base R only) on the `.qmd` and reports, with
line numbers, against the CLAUDE.md standards. It does not render.

- **labels:** executed chunks with no `#| label:`, duplicate labels, labels that are not lowercase-hyphen
- **args:** R calls with unnamed arguments, found by parsing the code (advisory; `c`, `list`, `library`, `print`,
  `cat`, `paste` are allowed positional)
- **math:** a binomial coefficient with no factorial formula in the deck; stats functions (`pnorm`, `qt`, `sd`,
  `t.test`, ...) on a slide with no formula on it or the slide before (advisory)
- **paths:** relative images and `../` files that do not exist

The advisory checks point at places to look. They will have false positives (a `pnorm` slide whose formula is two
slides back) and cannot judge whether a formula is the *right* one.

## /commit-decks

`/commit-decks <folder> [<folder> ...] [--go] [--push] [-m message]` stages only the named lecture/lab
folders (plus their `_freeze/` and `docs/` output and `docs/search.json`) so unrelated work such as
`lab7` or `lec12` is never swept in.

```
/commit-decks lec9 lab4                          # preview: what would be committed, what is left alone
/commit-decks lec9 lab4 --go -m Fix formulas     # commit
/commit-decks lec9 lab4 --go --push -m Fix formulas
```

- Preview is the default. Nothing is staged until `--go`.
- Each path is staged separately (`git add -A -- <path>`), so a missing path cannot abort the rest.
- Stops if anything else is already staged, and asks for `-m` before committing.
- It does not add a Co-Authored-By trailer: commits made this way are yours.
- Tested in a scratch clone with a local remote (preview, missing message, pre-staged file, commit+push).

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
- `$` can't be passed to functions imported from another file and two `session.start` hooks without
  a matcher are rejected, so the whole mod lives in `hooks/register.js`.
- The plugin validator also notes a missing LICENSE and icon; those only matter for submitting to Anthropic's directory.
