# Deck backlog (lectures 8 and up, plus labs)

Generated 2026-10-25 by `Rscript scripts/check_deck.R <deck> ...` (the `/check-deck` command in `mods/deck-tools`).
Re-run it after each pass to update the counts. Lectures 1-6 are deferred until the semester wraps.

| Deck | R chunks | Chunks missing a label | Calls with unnamed args | Slides to review for a nearby formula |
|---|---:|---:|---:|---:|
| lec8/lec8 | 21 | 21 | 136 | 1 |
| lec9/lec9 | 12 | 12 | 68 | 1 |
| lec10/lec10 | 13 | 13 | 80 | 1 |
| lec11/lec11 | 19 | 19 | 144 | 0 |
| lec12/lec12 | 4 | 4 | 26 | 0 |
| lec13fct/lec13fct | 13 | 13 | 79 | 1 |
| lec14/lec14 | 24 | 24 | 139 | 0 |
| lec15c/lec15c | 10 | 10 | 64 | 0 |
| lec16/lec16 | 11 | 11 | 66 | 0 |
| lab1/lab1 | 1 | 1 | 0 | 0 |
| lab3/lab3 | 4 | 4 | 9 | 0 |
| lab4/lab4_instr | 6 | 6 | 2 | 1 |
| lab7/lab7 | 2 | 2 | 24 | 2 |
| lab10/lab10 | 4 | 0 | 10 | 0 |
| **Total** | 144 | 140 | 847 | 7 |

## Suggested order

1. **Chunk labels** (safe and mechanical): add `#| label:` to every chunk, lowercase-hyphen, describing the content. Rendering is unaffected.
2. **Formula review** (the list below): confirm each slide has the explicit formula on it or the slide before; many are false positives.
3. **Named arguments** in existing lecture code: do last, deck by deck, and compare rendered output before and after, since it touches executable code.

Note: `lec12`, `lab7` are the user's in-progress files; do not edit until they are committed.

## Slides to review for a nearby formula (advisory)

**lec8/lec8**
- line 825 "Comparing Two Schools" uses dnorm

**lec9/lec9**
- line 496 "Hypothesis Testing Steps" uses qt

**lec10/lec10**
- line 570 "Assumptions" uses sd, t.test

**lec13fct/lec13fct**
- line 610 "Factorial ANOVA in R" uses aov

**lab4/lab4_instr**
- line 46 "Question 3" uses qnorm

**lab7/lab7**
- line 30 "**Example data from a fictional class:**" uses lm
- line 156 "**Adding More Predictors**" uses lm
