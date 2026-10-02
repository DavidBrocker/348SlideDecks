# Flag slides that run past the bottom of the screen.
#
# Usage (from the repo root, after rendering):
#   Rscript scripts/check_slide_overflow.R lec9/lec9
#   Rscript scripts/check_slide_overflow.R lec10/lec10 lec11/lec11
#
# It opens docs/<deck>.html in headless Chrome at 1600x900, reveals every
# fragment and every "Reveal answer" <details> box (the worst case a presenter
# can hit), and reports slides taller than 900px. The title slide always
# reports ~980px because of its decorative background; that is harmless.
# Requires the chromote package.
library(chromote)

check_deck <- function(deck, root = getwd()) {
  b <- ChromoteSession$new(width = 1600, height = 900)
  on.exit(b$close(), add = TRUE)
  b$Page$navigate(sprintf("file://%s/docs/%s.html", root, deck))
  Sys.sleep(10)
  b$Runtime$evaluate("Reveal.configure({transition:'none', fragments:false})")
  n <- b$Runtime$evaluate("Reveal.getSlides().length")$result$value
  bad <- character()
  for (i in seq_len(n) - 1) {
    b$Runtime$evaluate(sprintf("(function(){var s=Reveal.getSlides()[%d]; var ix=Reveal.getIndices(s); Reveal.slide(ix.h, ix.v); document.querySelectorAll('.fragment').forEach(function(f){f.classList.add('visible','current-fragment')}); document.querySelectorAll('details:not(.code-fold)').forEach(function(d){d.open=true}); })()", i))
    Sys.sleep(0.6)
    r <- b$Runtime$evaluate("(function(){var s=Reveal.getCurrentSlide(); var t=(s.querySelector('h2')||s.querySelector('h1')); var h3=s.querySelector('h3'); return s.scrollHeight+'|'+(t?t.innerText.slice(0,40):'')+' / '+(h3?h3.innerText.slice(0,40):'')})()")$result$value
    parts <- strsplit(r, "\\|")[[1]]
    if (as.numeric(parts[1]) > 905)
      bad <- c(bad, sprintf("  slide %d (%spx): %s", i + 1, parts[1], parts[2]))
  }
  cat(sprintf("== %s: %d slides\n", deck, n))
  cat(if (length(bad)) paste(bad, collapse = "\n") else "  all slides fit", "\n")
}

decks <- commandArgs(trailingOnly = TRUE)
for (d in decks) check_deck(d)
