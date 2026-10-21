# PSY 348 course site: standing standards

Quarto project for PSY 348 (Statistics for Psychology, Farmingdale State College). Instructor: Dave Brocker.
Slide decks are revealjs (`lec#/lec#.qmd`, `lab#/lab#.qmd`), each with a `*_viewer.qmd` iframe page listed in `_quarto.yml`.
Rendered output goes to `docs/` (tracked, served by GitHub Pages); `_freeze/` is tracked too.

## R code style in slides and labs

- **Name every argument** of every function call: `pnorm(q = 70, mean = 76, sd = 8, lower.tail = TRUE)`, not `pnorm(70, 76, 8)`. Students get stuck on "what do I type"; named arguments show what each value does.
- Store inputs in named objects first (`mu`, `sigma`, `n`, `x`) and reference them, rather than retyping numbers.
- **Chunk naming:** every executed chunk gets a label: `#| label: z-one-score`. Lowercase, hyphen-separated, descriptive of the content (not `chunk-1`). Labels are unique within a file. Figure chunks may use the `fig-` prefix when cross-referenced.
- Wrap executed chunks that students should read in a `codewindow` (`::: {.codewindow .r .codewindow-dark}` with the file name on the first line) and set `#| code-fold: false` on them. Global `code-fold: true` in the YAML so plots and tables offer a "Code" toggle.
- Keep `execute: echo: true, warning: false, message: false` in lab YAML.

## Math and LaTeX: always show the "true" form

Whenever a slide uses a shorthand or compact notation, put the explicit formula next to it, even if the lecture does not dwell on it.

- Example: if a slide uses the binomial coefficient layout $\binom{8}{4}$ (or `choose(n = 8, k = 4)`), also show $\binom{n}{k} = \dfrac{n!}{k!\,(n-k)!}$ and the worked values $\dfrac{8!}{4!\,4!} = 70$. Say what the numerator and denominator are in one plain sentence.
- Same for sigma/s in standard errors, sums, factorials, combinations, summation notation, and any symbol that stands for a longer expression.
- Keep it short: one line of formula plus one line of plain English. It must not turn the slide into a factorial lecture. A callout or a trailing bullet is fine.
- When editing a slide, if a formula appears in only its compact form, add the explicit form.

## Labs: three companion files

Each lab `lab#` should have:

1. `lab#/lab#.qmd`: the student-facing revealjs slides.
2. `lab#/lab#_instr.qmd`: answer key / walkthrough (HTML), with every question answered in R using the style above and the math written out.
3. `lab#/lab#_instr.xlsx`: the same walkthrough in Excel, with an inputs block and live formulas (`STANDARDIZE`, `NORM.DIST`, `NORM.INV`, `SQRT`, ...) and an "Equivalent in R (all arguments named)" column.

`quarto render` renders `lab#.qmd` and `lab#_instr.qmd` together (project render). The `.xlsx` is not produced by Quarto, so rebuild or update it whenever `lab#_instr.qmd` changes. Check the key's numbers against actual R output before publishing; draft keys have contained errors (for example mixing a z-score with a raw-score mean and sd in `pnorm()`).
`lab3` is the exception: its key (`_lab3_key.qmd`) is underscore-prefixed and gitignored because it is distributed only on Brightspace; render it with `quarto render lab3/_lab3_key.qmd`.

## Working conventions

- After finishing a step, render the changed file(s), check the output for errors or stale figures, then commit and push (the user generally wants pushes after finished steps).
- Stage only the files you changed. `lab7/lab7.qmd`, `lec12/lec12.qmd`, `lec4/lec4_viewer.qmd`, and `lab_zscores/` may hold the user's own uncommitted work; do not add them unless asked.
- Underscore-prefixed files are ignored by project render and preview; use `quarto render <file>` for those.
- Commit messages end with the attribution trailer given in the session.

## Related repository: showkitr

R package at `/Users/davebrocker/Desktop/shared_sandbox/showkitr`, remote named `showkitr` (not `origin`): push with `git push showkitr main`. See its `TODO.md` for open work. Keep `"n":` keys quoted in glossary YAML. Run `devtools::document()` after roxygen edits and keep `R CMD check` at 0/0/0.
