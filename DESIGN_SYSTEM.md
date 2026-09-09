# 348SlideDecks Design System

Reference for the shared `custom.scss` theme used across PSY 348/360/442 revealjs decks. This is the source of truth for design tokens and component conventions — it lives in this repo so it can never drift from the code it describes (a change to `custom.scss` and a change to this doc belong in the same commit).

For the *narrative* of how and why specific decisions were made session-by-session, see the "Teaching" claude.ai project doc (`course_site_design_philosophy.md`). This file is the settled result; that one is the working log.

## Tokens

Defined at the top of `custom.scss`, under `scss:defaults`.

| Token | Value | Use |
|---|---|---|
| `$body-color` | `#2F3E46` | Body/paragraph/list text (dark desaturated green-gray) |
| `$text-muted` | `lighten($body-color, 50%)` | Captions, sub-labels |
| `$body-bg` | `#E9F1EC` | Slide background (sage pastel) |
| `$accent` | `#5C715E` | Headings, links, primary pill/badge fill |
| `$accent2` | `#8FAF9E` | Card/badge borders, secondary accents |
| `$accent3` | `#A7C7B5` | Lighter borders (e.g. `hero-def` card) |
| `$accent4` | `#DCE8E1` | Lightest fill (e.g. `you-callout` background) |

Fonts: **Fira Sans** (body/headings), **Fira Code** (code), **Atkinson Hyperlegible** (fallback — chosen for accessibility), **Material Icons** (icon font, used throughout via `.material-icons`).

Rule of thumb: don't hardcode hex values in new components — use the token so a future palette change is a one-line edit.

## Title slides — two patterns, don't mix them

Most decks use Quarto's YAML `title:` field, which auto-generates `#title-slide` (styled directly, near the top of the rules section: `.title`, `.subtitle`, `.quarto-title-authors`).

Some decks (`lab1.qmd`, `lec2.qmd`) have no YAML title and instead hand-build a manual `##  {#custom-title}` slide, styled via `.cust-title` / `.cust-sub` / `.cust-pic` / `.cust-author`. **This pattern has a known gotcha** — see Known Gotchas below. If you're starting a new deck, prefer the YAML `title:` approach unless you specifically need the custom layout; it avoids the gotcha entirely.

`lec2.qmd`'s custom title also has an optional animated background layer (`#custom-title`, `.title-bg-icons`, `.ghost-nodes`) — six drifting Material Icons + five floating dots, slow and low-opacity by design. Copy this only if a deck wants that same "quiet motion" flourish; it's not a default every custom-title slide needs.

## Core components

**`.card`** — generic white card with hover-lift, used for image+caption groupings. `.card-img` crops to a fixed 260px height (`object-fit: cover`) so mixed-aspect-ratio source images stay uniform in a row.

**`.callout-important`** — Quarto's default callout is restyled from red/orange to the sage accent, since on this site "important" means "definition, pay attention" rather than "warning/error."

**`.blank-tag`** — small corner pill marking a "fill in the blank" slide for the presenter (axis/title-stripped plot for in-class guessing). White text on accent background.

**`.you-callout`** — inline pill for a short personalized aside (e.g. "...and you scored a 67"), lighter weight than `.callout-important`.

**`.stat-card`** — bordered card for a single big number + sublabel (`.stat-num` / `.stat-sub`).

**`.img-placeholder`** — dashed-border stand-in for "find a real image later," so a slide's layout/intent is legible before the asset hunt is done. Once a real photo replaces it, `img` inside is capped at `max-height: 340px` so a full-res photo can't push content off the bottom of the slide.

**`.badge-row` / `.content-badge`** and **`.icon-row` / `.icon-row-item`** — two weights of "icon + short label" layout. `.content-badge` has card chrome (border, shadow, optional heading) for a handful of prominent items; `.icon-row-item` is bare icon + caption for recapping several examples/terms compactly (e.g. under a bullet list).

**`.bio-cluster`** — the "Hi, I'm Dave!" circular-photo-plus-scattered-badges layout (fixed pixel positioning, tuned to one specific slide). Not intended as a general-purpose pattern — treat as a one-off unless another bio slide needs the exact same shape.

**`.marquee-wrap` / `.marquee-track`** — continuous horizontal scroll (e.g. course-eval pull quotes), pauses on hover so a quote can be read.

**`.book-anim`** — small CSS crossfade flourish for a closing/Q&A slide.

## Term + definition (`hero-def` / `term-list`)

The newest component (added 9/4, applied first on `lec3.qmd`'s Operationalization slide). Built specifically with a future site-wide glossary in mind.

**Markup uses pandoc's native definition-list syntax** — already the site's convention (see `lec1.qmd`'s "Statistics / Describing / Analyzing..." slide) — rather than a bespoke div structure. `Term` / `: definition` compiles to plain `<dl><dt><span class="term-pill">Term</span></dt><dd>Definition text</dd></dl>`. That's the most mechanical possible shape to script-extract later: grep every `.term-pill` span and its `dd` sibling, across every deck, no per-slide parsing needed.

**`.hero-def`** — one pivotal term per slide. Pill sits half-overlapping a white bordered card; definition is the card body underneath. The containing section auto-centers the card in the space below the slide heading via `:has()`:

```scss
.reveal .slides section:has(> .hero-def) {
  height: 100%;
  display: flex;
  flex-direction: column;
}
.hero-def { margin: auto; /* ... */ }
```

Markup:

```markdown
::: hero-def
[Operationalization]{.term-pill}
: The process of defining the measurement of a phenomenon that is not
  directly measurable (AKA a latent variable) though its existence is
  implied by other phenomena.
:::
```

**`.term-list`** — several terms on one slide. Lighter weight, no card chrome (a repeated `hero-def` five or six times would be visually loud). Each term/definition pair should be wrapped in its own `::: fragment :::` so they reveal one at a time rather than landing all at once:

```markdown
::: term-list
::: fragment
[Term One]{.term-pill}
: Definition one.
:::
::: fragment
[Term Two]{.term-pill}
: Definition two.
:::
:::
```

**Open item:** `lec1.qmd`'s existing 7-term slide predates this component and doesn't use `::: fragment :::` per pair — a good candidate to retrofit into `.term-list`, not yet done.

**Planned next:** a `hero-example` extension — a `panel-tabset` of worked examples (e.g. Self-Report / Physiological / Behavioral operationalizations of "disgust," Dave's go-to running construct) nested as a natural child of `hero-def`, spawned from the Operationalization slide specifically. Check the existing tabset CSS (already used for `lec2`'s Anscombe plots) before writing new rules, to extend consistently rather than duplicate.

## Known gotchas

**CSS specificity beats source order, every time.** `.reveal .slide p { color: $body-color; }` sits near the top of the file and directly targets any `<p>` on a slide — including the `<p>` pandoc wraps fenced-div content in. A new component that puts colored text in a plain `<p>` inside a div (e.g. `.my-thing p { color: #fff; }`) will silently lose to that rule if its own selector has *lower or equal specificity* (class + tag vs. two classes + tag), no matter where in the file it's written or how late it's added. This bit `.blank-tag` and `.you-callout` once already (fixed by matching specificity: `.reveal .slide .blank-tag p`). **Before shipping any new pill/badge/tag component with its own text color, check what specificity the existing global text rules have and match or exceed it** — don't assume one extra class is enough.

**The `##  {#custom-title}` manual title-slide pattern loses `top` positioning.** Quarto's reveal.css never sets an explicit `top` on `.reveal .slides > section.present` in this site's config. Normally that's invisible because the auto-generated `#title-slide` slide is styled specially — but a manual custom-title slide is just an ordinary `.slide.level2` section like any other, which exposes the gap: the browser resolves the unset `top` as the element's normal-flow "static position," landing the present slide *below* the previous slide's canvas instead of overlapping it at the top (reads as "content pushed off / cut off"). Fixed globally with `.reveal .slides > section.present { top: 0 !important; }` — a no-op on already-correct decks, so this fix never needs revisiting per-deck.

## Dependency checking

`scripts/check_dependencies.R` scans every `.qmd`/`.R` file for R package dependencies — explicit (`library()`/`require()`), namespace-qualified (`pkg::fn()`), and a curated list of "invisible" triggers (e.g. `gt::gtsave(*.png)` silently needs `webshot2` + `chromote`). Run it on a fresh workspace (`Rscript scripts/check_dependencies.R`) before rendering to catch missing packages up front instead of one broken chunk at a time. If a render fails on a package that was never named directly in a `.qmd`, add its trigger pattern to the `implicit_triggers` list in that script so future runs catch it automatically.

## Open items

- Retrofit `lec1.qmd`'s 7-term slide into `.term-list` (with per-pair `::: fragment :::`).
- Build the `hero-example` tabset on `lec3.qmd`'s Operationalization slide (disgust example).
- Missing images still unresolved on lec3/5/6/7/8/11/15c (pre-existing, found during the folder reorg — not a design-system issue but tracked here since it blocks a clean render).
- Extract the glossary: once several decks use `.term-pill`/`dl`/`dd`, a script can walk every deck and build a site-wide glossary page from the markup alone.
