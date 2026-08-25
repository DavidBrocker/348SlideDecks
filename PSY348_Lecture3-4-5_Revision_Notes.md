# PSY 348 — Lecture 3, 4 & 5 Revision Notes
*Organized from Dave's Aug 24, 2026 voice walkthrough. Content is confirmed fine on all three — this is a visual/template pass. Cross-checked against the real files in `348SlideDecks` on Aug 25.*

## Correction after opening the actual project
What you narrated as "Lecture 4" is actually split across two files: **`lec4.qmd`** covers Dispersion (variance/SD) and ends at its own slide 46, and **`lec5.qmd`** — titled "Standard Normal Distribution" — is where the z-scores material you kept talking through actually lives. So the Lec 5 recording you weren't sure you'd gotten to is effectively already here, just filed under `lec5.qmd` rather than a separate pass. I've split Section 3 accordingly.

I also found 8 lines in `lec3.qmd` that look like slide headings (`# Population: strongly skewed...`, etc.) but are actually R comments sitting inside code chunks — they'd inflate a naive slide count. Once filtered out, `lec3.qmd` has **76 real slides**, and your spoken numbers track almost exactly 1 higher than that raw count (confirmed against the Bobo Doll dead-image slide, the CLT/Galton-board pair, and the closing takeaways slide) — i.e. **your "slide N" ≈ the file's Nth-1 heading**. I've applied that shift below. `lec4.qmd` (46 slides) and `lec5.qmd` (30 slides) didn't show that pattern as cleanly, so treat those slide numbers as approximate — several headings repeat a lot ("Calculating Dispersion" appears 15 times, "Z-Scores" almost as often), so the fastest way to confirm any one of these is against the rendered site rather than the raw source order.

## How to use this
Section 1 is the stuff worth fixing **once, globally** — including a few things the theme already supports and you just need to *use*. Sections 2–4 are the per-slide punch lists for Lecture 3, Lecture 4, and Lecture 5. Section 5 is every spot where you explicitly asked "what do you think" — my actual take, not just a restatement.

---

## 1. Global template rules (apply across both lectures, and probably every future one)

- **Every content slide gets a heading *and* a subheading.** Right now it's inconsistent (called out directly on 39/40, but it recurs everywhere).
- **Numbers/formulas render in LaTeX, not backtick/code styling**, once a slide moves past the "look, a number!" introduction (slide 35 is fine keeping the code treatment since that's the *first* time you're teaching students to notice numbers — but by 43/49/57 you're ready to move to LaTeX).
- **Hover-to-reveal for answers**, instead of spawning a new slide just to show a result (43, 49, 57).
- **"Important" callout needs a new color.** Red/gold both currently read as an error or warning, not "pay attention to this definition." Comes up on 37, 56, and the sigma slide in the z-score section.
- **Every GT table gets a title, caption, header, and footer.** This is the single most-repeated note in the whole transcript (14, 21, 27, 33, and generally).
- **Every chart/plot gets a caption or figure label**, from the first one onward, so students build the expectation early.
- **Title Case on every slide heading, no exceptions** — you caught "tendency" and "mode" as lowercase already; worth a full pass rather than fixing them one at a time.
- **End-of-lecture recap/takeaway template.** Slide 77 (Lec 3) is the model you like — reuse it to close every lecture (Lec 4 → slide 45/46), and consider linking terms (mean, median, mode) back to the slide/formula that defined them.
- **Part 1 → Part 2 navigation:** last slide of Part 1 should have a clickable jump to the Part 2 opener.
- **Global click/hover definitions.** Any core term (variance, degrees of freedom, SD) should be clickable/hover-able to a popover with the definition + where it was introduced, rather than re-explaining or hand-waving ("we'll get to that later") each time. This is the fix for the degrees-of-freedom moment on slide 31 too.
- **Section-heading / discussion-question template** (first used as slide 24 in Lec 3, reused as 54 and 25 in Lec 4): a visual cue that tells the class "pause, we're discussing this," with the question centered on the slide.
- **"Continuing example" sub-template**: a distinct visual treatment for slides that are picking up a running example rather than introducing something new.

### Already built in `custom.scss` / the YAML — just needs to be *used* consistently
- `.card` — already styled (border, shadow, header/body/footer). This is your answer for the slide-2 grid, the "each of these could be a card" note on lec4 slide 6, and the patchwork panels — no new CSS needed, just wrap content in `.card` divs.
- `details` / `summary` — already themed (bordered box, colored summary). This is exactly the disclosure mechanism you want for slide 57's unanswered questions — wrap each Q/A in `<details><summary>...</summary>...</details>`, no build needed.
- `lightbox: true` is already set in the lec3 YAML. To get the fuller/centered treatment you described for the mean/median/mode slide, look at Quarto's revealjs lightbox options (`match`/`contain` settings) before writing custom JS — it may already do what you want with a config tweak.
- `menu: {side: right, width: wide}` and `chalkboard: true` are already on. For the Part 1 → Part 2 jump, the cleanest route is a slide ID (`## Heading {#part2}`) plus a plain link (`[Jump to Part 2](#/part2)`) — no plugin required.
- `.callout-tip` has padding defined but **`.callout-important` has no override at all** — it's just inheriting Quarto's default red/orange, which is exactly why it reads as an error. One SCSS block fixes every instance of this across both lectures at once.

### Genuinely new to build
- Term-level popover/hover glossary (variance, degrees of freedom, SD, etc.) — doesn't exist yet. Simplest MVP: native `<abbr title="...">` tooltips; upgrade to a real popover later if you want richer content.
- ggiraph/gganimate treatments for the mean-median-mode shape-shift and the deviation-settling transition — these are currently static ggplot chunks.
- The interactive Galton board (slide ~62/lec3).
- GT table caption/header/footer helper — worth writing as one small R function you call from every table chunk instead of repeating the same `gt() |> tab_header(...)` boilerplate by hand.
- Dark mode toggle — you flagged this yourself as low-priority, after content.

---

## 2. Lecture 3 — `lec3.qmd` — per-slide notes

*76 real slides. "Likely heading" = your spoken number shifted down by 1, per the correction above — spot-checked against several of these directly (the Bobo Doll image, the "coffee drinker" discussion slide, the reused Qualtrics iframe on the Discrete/Continuous slide, the Practice 3 → Measures of Central Tendency part break) and it holds up well. Still worth a glance at the live site before you commit to any one row.*

| Slide | Likely heading in file | Action |
|---|---|---|
| 2 | "Studying Psychology" (1st) | Remove the errant leftover "[Measures of Central Tendency]" bracket text at the bottom. Grid layout (attitudes, brain activity, beliefs, thoughts, etc.) is too cramped — needs a better grid structure. |
| 3 | "Studying Psychology" (2nd, Bobo Doll) | Dead slide — needs content/working image. |
| 12 | "Self-Report:" (Disadvantages) | Good as an intro, but gets long once you're through Acquiescence/Social Desirability/Demand Characteristics — split into its own definition-specific template once it's this dense. |
| 15 | "Observation" | Dead slide, broken images. |
| 18 | "Implicit measures" | Needs a redesign — the fMRI/EEG images on it are currently dead links. |
| 23 | "Nominal (Categories, No Order)" | Flagged for reformatting — no specific direction yet, revisit. |
| 24 | "Raise your hand if you're a coffee drinker…" | This is your "pause and discuss" section-heading template — keep the concept, redo the visual. |
| 32 | "Discrete and Continuous" (has a 2nd, reused Qualtrics iframe) | See Section 5 — you asked for my take, and this confirms it's literally a copy-pasted iframe with no topical connection to Discrete/Continuous. |
| 35 | "Determining Scale of measurement" | First "word problem" slide — establishes that any number gets the backtick/code treatment so it's visually distinct. Keep as-is; this is the *introduction* to the pattern. |
| 39 / 40 | "Why does scale of measurement matter?" / "Takeaway: *How* we measure variables matters…" | 39 has a subheading, 40 doesn't. 40's subheading is oversized. See Section 5 for content suggestion. |
| 41 | "Statistical Terms & Symbols" | Probably cuttable — was "meant to be fun." |
| 42 | "Statistical Symbols" | Keep the current formatting. Add the actual Greek letter name next to the symbol (e.g., label Σ as "capital sigma," not just the bare symbol). |
| 43 | "Practice 1" | Convert bullets to LaTeX. Add hover-to-reveal. Keep the N / x̄ formatting pattern. |
| 45 | "Practice 3" | Last slide of Part 1 → convert to the end-of-part recap template. |
| 46 | "Measures of Central Tendency" (Part 2 opener) | Needs the clickable jump-in from 45 (see the `{#part2}` slide-ID trick in Section 1). |
| 47 / 48 | "Review" x2 | 47 has 2 bullets and a lot of dead space; 48 fills the space but the bullet formatting between the two should be unified. |
| 49 | "Review" | Convert to LaTeX. Decide: keep the "?" as a hover-to-reveal, or show the answer directly. |
| 52 / 53 | "Descriptive Statistics" x2 | Subheading is too large — see Section 5. |
| 54 | "Measures of Central tendency" | Has a duplicate bullet — needs rephrasing. Candidate for the "discussion question, centered text" template. |
| ~56 | "Measures of Central tendency" | ("walkthrough of how to get the values") — swap the "important" callout color (see the SCSS fix in Section 1); use inline LaTeX instead of pushing terms to the side. |
| 57 | "Measures of Central tendency" | Asks 2 questions, never shows the answers. Either carry the answers into 58, or wrap the two questions in `<details>/<summary>` (already themed, just use it) so they expand on click. |
| 59 | "Measures of Central tendency" | Mean/median/mode shape-shift slide — strong candidate for a ggiraph treatment so the shape change is interactive. Also revisit the lightbox: check the `match`/`contain` config before building custom full-screen JS. |
| ~60 (central tendency / mean) | "Measures of Central Tendency" | Plot + details side-by-side works, but space usage could be better. |
| 61 | "Why Do We Assume Normality?" | Plot too small on the left. Remove the gridlines/panel grid. Central Limit Theorem is introduced but not explained — needs real content. |
| 62 | "Central Limit Theorem" | Dead link (was pointing to a Galton board resource). Idea: build an actual interactive Galton board (n increases → converges to normal). |
| 63 | "Measures of Central tendency" | Nice layout, but the plot should take up more space. |
| 70 | "Calculating measures of central Tendency" | "Formula screen" using a tip callout — consider a split layout: formula/callout on one side, an interactive or code-based widget on the other, showing both the arithmetic and the programmatic version. Same idea applies to the next two slides (median, mode). |
| 73 | "Using Excel" | Google Street View iframe — nice touch, make it more interactive if possible. |
| 76 | "Review" (2nd-to-last) | Might be a redundant review slide — see Section 5. |
| 77 | "Key Takeaways Slide" | **Keep as the model.** This becomes the end-of-lecture recap template for every future lecture. Consider linking mean/median/mode back to their formulas. |

**Recurring across the back half of Lecture 3:** Title Case pass on all headings; charts need to "take up more space" as a general rule (63, 70, 71ish).

---

## 3. Lecture 4 — `lec4.qmd` (Dispersion) — per-slide notes

*46 slides, ends at "Problem 2: Comparing dispersion." This file covers the Ch. 3 review through variance/deviation/SD — everything up through the point where your narration shifted into z-scores, which actually lives in `lec5.qmd` (Section 4 below).*

| Slide | Action |
|---|---|
| Global | Content sanity check: verify every place a bullet says "x̄" is actually correct, not a leftover/typo. |
| 4 | Ch. 4 review — good, make slightly more developed. Capitalize "Tendency." |
| 5 | "Dispersion" appears twice — as the term label and inside its own definition. Cut one. |
| 6 | Capitalize "Tendency." Subheading is too long — consider breaking each concept into its own card instead of one dense block. |
| 9 → 10 | Great transition — strong candidate for a ggiraph/gganimate treatment animating deviations settling around the mean. |
| 11 | Introduces "variance" but has no slide title — add one. |
| 12 | Patchwork panels too small/cramped — enlarge or separate. |
| 13 | "Phone usage" text is italicized — switch to bold, or the code/backtick treatment. |
| 14 | Decent slide, but doesn't make clear how the two things differ. GT table has no caption/context — same title/caption/header/footer fix as everywhere. |
| 19 | Introduces the deviation formula — pairs with the "step-by-step walkthrough" idea from Section 1. |
| 21 | Reuses the earlier graph with x − x̄ — same visual improvement as 19. |
| 22 | Good visualization — consider adapting it to match the earlier horizontal balloon/line style for consistency. |
| 25 | Convert to the discussion-question template, then pose questions 1–3 underneath. |
| 26 | Common student friction point (why subtract the mean, square it). Make sure the slide *justifies* this, not just states the formula. |
| 27 | Same GT table caption/title treatment. |
| dispersion slide | Consider adapting the step-by-step walkthrough format. |
| 28/29 | "Now what?" phrasing → change to something like "continuing…". Render the (1+1+0+1) sample-deviation walkthrough in LaTeX. |
| 31 | Introduces "degrees of freedom" then hand-waves it ("don't worry about it now") — you flagged this yourself as pedagogically unfair. Give it a real, lightweight explanation; tie into the global click-to-define mechanism. |
| 32 | Already improved — no action. |
| 33 | Good, wants more description. Apply GT table header/footer/caption treatment. |
| after 33 | Add a subheading marking "continuing example," with its own visual treatment. |
| 37 | Gold highlight hard to see against background. Callout red again reads as an error. Consider a step-by-step LaTeX walkthrough (s² = variance, s = √variance) as a memory cue. |
| 39 | Capitalize "SD." Reuse bold/highlight treatment for formula terms, tied to the global definition-lookup idea. |
| 40 | Apply the same block-quote format used elsewhere. |
| 41 | This side-by-side layout is the one you want replicated elsewhere — but the left-hand side formatting itself needs work. |
| 42 | Code-treatment styling throughout, but italics mixed in inconsistently; font looks different here too — consistency pass needed. |
| 43 | Too many symbols crowded on one screen — needs better delineation/spacing. |
| 44 | Not doing much — consider cutting. |
| 45 | Last slide of Lecture 4 → becomes the end-of-lecture recap template, matching Lec 3's slide 77. |
| 46 | Proposed new recap slide — same template. |

---

## 4. Lecture 5 — `lec5.qmd` ("Standard Normal Distribution") — per-slide notes

*30 real slides (2 R-code-comment lines filtered out, same as lec3). This is where your narration actually was once you said "so 4 Z scores" — the file's own title is "Standard Normal Distribution," and slide 1 opens on exactly that, with the Z-scores content starting around its own slide 9–11. So this doubles as your Lec 5 walkthrough, just recorded as a continuation of the Lecture 4 conversation rather than a separate pass.*

| Slide | Action |
|---|---|
| Intro | Capitalize "SD" here too. Footer hexagon icons — some filled, some not, and the numbering isn't consistent; clean that up. Some background visual interest would help (currently plain); keep the current color scheme as the base. Dark-mode toggle is a nice-to-have, not urgent — revisit after content is locked. |
| Z-score intro | Give this moment more weight — it leads directly into the Z-test, so it deserves a bigger "arrival" treatment (bigger typography/animation cue) rather than blending in. |
| — | The claim that z-scores are "better than raw SD" needs an actual justification/proof on the slide, not just an assertion. |
| — | Capitalize "Z-score" consistently in that opening block of text. Standard deviation capitalization — see Section 5. |
| 2 | Reformat so the plot takes up most of the space. Consider slider interactivity or ggiraph hover-highlighting on points. |
| 5 | Dead picture. Students reference this slide a lot — make sure it stays easy to find/return to. |
| 6 / 7 | A lot is happening on 7 that could move back into 6. Indent the answer bullet on 7; add LaTeX/highlight treatment. |
| — | Building these charts took a lot of trial and error — worth wrapping the reusable pieces into a small internal plotting helper/style kit. |
| 11 | Dead slide. |
| 12 | Dead image — could fold into one of the worked examples instead. |
| 13 | Font looks different — fix to match. |
| 14 | Has the formula plus notes on each term — consider moving those notes inside the callout rather than beside it. Gold still doesn't work on this background. A pink element needs a different color. Use inline LaTeX so it reads across in one line. First time students see lowercase σ — worth a short callout/link explaining it. |
| 16 (Example 2) | Put in the callout format used elsewhere. Same for 17, 18, 19. |
| 20 | Could use more information. |
| 21 | Add header/footer to the table, tighten spacing, add divider lines between points. |
| 21–22 | You like this layout overall — close to the target — but still consider converting to cards. |
| 23 | Easy to read — consider making the table interactive/pageable. |
| 24 | Patchwork panels already have titles — bold them for better visual context. |
| 26 | Bring back the running example ("Esmeralda"?) — ask "what percent scored better than her," show the answer directly below. Highlighting "98% did better" confuses students against "2% did worse" — word the callout carefully so it's unambiguous which direction is which. |
| 27 | Rendering error shows literal "null" as the axis title — fix: rename X/Y axis labels, remove the title, remove "null." Add a second example ("what percentage scored higher than X") with the same highlight treatment as before (e.g., highlighting a point around 16%, one to the right). |
| end | Add the end-of-slide/end-of-lecture recap slide here too. |

---

## 5. Where you asked for my take

**Slide 32 (Qualtrics iframe):** Confirmed in the source — it's the exact same iframe (same `SV_8BmvWRKoN0pX36e` survey) already used earlier on the "What's wrong with this?" self-report slide, pasted a second time into "Discrete and Continuous" with no topical connection to that slide. That matches your instinct exactly. Cut it, unless you make it functional. If you want to keep it, turn it into something they actually interact with (a live poll question tied to *that* slide's topic) so it earns the screen space. Otherwise it's a bigger maintenance/dead-link risk (like 62 and 15/18) for no payoff.

**Slides 39/40 (the empty area after the subheading fix):** Without seeing the actual content there I'd lean toward a short worked micro-example — one line showing the number-symbol pairing in context — rather than more prose. It keeps the slide doing one job (reinforcing the N / x̄ notation) instead of becoming a second definitions block.

**Slides 52/53 (subheading size):** Drop it a size, and make it match your body-text weight rather than competing with the main heading. A subheading that's nearly as prominent as the heading undercuts the visual hierarchy you're building everywhere else (heading → subheading → content).

**Slide 76 (possible redundant review):** If it's covering the same ground as slide 77's recap, cut it — you don't need two "wrapping up" moments back to back, and 77 is the stronger of the two by your own read. If 76 is doing something 77 doesn't (e.g., a practice problem vs. a takeaway list), keep it but relabel it so it's clearly distinct.

**Heading → subheading → bolded-bullet scheme, in general:** I'd keep it, but reserve the bold treatment for the term itself, not the whole line — bold-everything reads as shouting past 3–4 bullets. If a slide needs more depth than that pattern comfortably holds (your "in-depth" worry), that's usually a sign the slide should split rather than the format should loosen.

**Standard deviation capitalization:** Capitalize it — "Standard Deviation" — to match your Z-score/SD consistency rule elsewhere. Mixed-case terms next to each other (capitalized SD, lowercase "standard deviation") reads as inconsistent rather than intentional, and you're already standardizing everything else (Title Case headings, capitalized Tendency, capitalized SD).
