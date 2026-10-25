# Static checks for a course deck against the standards in CLAUDE.md.
#
# Usage (from the repo root):
#   Rscript scripts/check_deck.R lec9/lec9
#   Rscript scripts/check_deck.R lab4/lab4 lab4/lab4_instr
#
# Needs only base R. Checks (all line numbers refer to the .qmd):
#   1. every executed {r} chunk has a label, labels are lowercase-hyphen, none repeat
#   2. R calls with unnamed arguments (the "name every argument" habit)
#   3. binomial coefficient shown without the factorial formula
#   4. stats functions (pnorm, qt, sd, ...) on a slide with no formula on it or the slide before
#   5. relative image / ../file paths that do not exist
# 2-4 are advisory: they flag places to look, not certain errors.

# Functions where positional arguments are natural and not flagged
ALLOWED_POSITIONAL <- c("c", "list", "library", "require", "requireNamespace",
                        "return", "print", "cat", "paste", "paste0", "message", "stop")

# Functions whose use should sit near a written formula
STATS_FUNCTIONS <- c("pnorm", "qnorm", "dnorm", "pt", "qt", "dt", "sd", "var", "t.test",
                     "aov", "cor", "cor.test", "scale", "choose", "lm", "TukeyHSD", "pf", "qf")

read_chunks <- function(lines) {
  starts <- grep("^```\\{r[ ,}]|^```\\{r\\}$", lines)
  chunks <- list()
  for (s in starts) {
    e <- s + which(grepl("^```\\s*$", lines[(s + 1):length(lines)]))[1]
    if (is.na(e)) next
    body_idx <- (s + 1):(e - 1)
    if (e - s < 2) body_idx <- integer(0)
    body <- lines[body_idx]
    header <- lines[s]
    label <- NA_character_
    m <- regmatches(body, regexec("^#\\|\\s*label:\\s*(\\S+)", body))
    got <- Filter(length, m)
    if (length(got)) label <- got[[1]][2]
    if (is.na(label)) {
      # {r name} or {r name, opts}
      h <- sub("^```\\{r\\s*,?\\s*", "", header)
      h <- sub("\\}.*$", "", h)
      first <- trimws(strsplit(h, ",")[[1]][1])
      if (length(first) && !is.na(first) && nzchar(first) && !grepl("=", first)) label <- first
    }
    is_code <- !grepl("^#\\|", body)
    chunks[[length(chunks) + 1]] <- list(
      line = s, label = label,
      code = body[is_code], code_lines = body_idx[is_code]
    )
  }
  chunks
}

# Unnamed-argument calls in one chunk: data.frame(line, fn, n_unnamed)
unnamed_calls <- function(chunk) {
  code <- chunk$code
  if (!length(code) || !any(nzchar(trimws(code)))) return(NULL)
  pd <- tryCatch(utils::getParseData(parse(text = code, keep.source = TRUE)),
                 error = function(e) NULL)
  if (is.null(pd)) return(NULL)
  fn_ids <- pd$id[pd$token == "SYMBOL_FUNCTION_CALL"]
  out <- list()
  for (id in fn_ids) {
    node <- pd$parent[pd$id == id]
    # climb to the expr that directly holds the "(" token
    repeat {
      kids <- pd[pd$parent == node, ]
      if (any(kids$token == "'('")) break
      up <- pd$parent[pd$id == node]
      if (!length(up) || up == 0) { node <- NA; break }
      node <- up
    }
    if (is.na(node)) next
    kids <- pd[pd$parent == node, ]
    kids <- kids[order(kids$line1, kids$col1), ]
    open_i <- which(kids$token == "'('")[1]
    close_i <- tail(which(kids$token == "')'"), 1)
    if (!length(close_i) || close_i <= open_i + 1) next
    inside <- kids[(open_i + 1):(close_i - 1), ]
    inside <- inside[inside$token != "','" & inside$token != "COMMENT", ]
    n_unnamed <- 0
    i <- 1
    while (i <= nrow(inside)) {
      if (inside$token[i] %in% c("SYMBOL_SUB", "STR_CONST") &&
          i + 1 <= nrow(inside) && inside$token[i + 1] == "EQ_SUB") {
        i <- i + 3   # name, =, value
      } else {
        n_unnamed <- n_unnamed + 1
        i <- i + 1
      }
    }
    fname <- pd$text[pd$id == id]
    if (n_unnamed > 0 && !(fname %in% ALLOWED_POSITIONAL)) {
      ln <- pd$line1[pd$id == id]
      out[[length(out) + 1]] <- data.frame(line = chunk$code_lines[ln], fn = fname,
                                           n = n_unnamed, stringsAsFactors = FALSE)
    }
  }
  if (length(out)) do.call(rbind, out) else NULL
}

check_deck <- function(deck, root = getwd()) {
  path <- file.path(root, paste0(deck, ".qmd"))
  if (!file.exists(path)) return(c(paste0("== ", deck, ": file not found: ", deck, ".qmd")))
  lines <- readLines(path, warn = FALSE)
  chunks <- read_chunks(lines)
  report <- character()
  add <- function(...) report <<- c(report, paste0(...))

  add("== ", deck, ": ", length(lines), " lines, ", length(chunks), " R chunks")

  # 1. labels ---------------------------------------------------------------
  no_label <- Filter(function(ch) is.na(ch$label), chunks)
  labs <- unlist(lapply(chunks, `[[`, "label")); labs <- labs[!is.na(labs)]
  dup <- unique(labs[duplicated(labs)])
  bad_form <- labs[!grepl("^[a-z0-9]+(-[a-z0-9]+)*$", labs)]
  if (!length(no_label) && !length(dup) && !length(bad_form)) {
    add("  [labels] ok")
  } else {
    if (length(no_label)) add("  [labels] missing on ", length(no_label), " chunk(s): line ",
                              paste(vapply(no_label, `[[`, numeric(1), "line"), collapse = ", "))
    if (length(dup)) add("  [labels] duplicated: ", paste(dup, collapse = ", "))
    if (length(bad_form)) add("  [labels] not lowercase-hyphen: ", paste(unique(bad_form), collapse = ", "))
  }

  # 2. unnamed arguments ----------------------------------------------------
  un <- do.call(rbind, lapply(chunks, unnamed_calls))
  if (is.null(un) || !nrow(un)) {
    add("  [args] ok: no unnamed arguments")
  } else {
    add("  [args] ", nrow(un), " call(s) with unnamed arguments (advisory):")
    by_fn <- split(un, un$fn)
    for (f in names(by_fn)[order(-vapply(by_fn, nrow, integer(1)))][1:min(12, length(by_fn))]) {
      d <- by_fn[[f]]
      add("    ", f, "(): line ", paste(utils::head(d$line, 6), collapse = ", "),
          if (nrow(d) > 6) paste0(" ... (", nrow(d), " total)") else "")
    }
    if (length(by_fn) > 12) add("    ... and ", length(by_fn) - 12, " more functions")
  }

  # 3. binomial coefficient without the factorial formula --------------------
  has_binom <- grep("\\\\choose|\\\\binom", lines)
  if (length(has_binom)) {
    has_fact <- any(grepl("!", lines, fixed = TRUE) & grepl("$", lines, fixed = TRUE))
    if (!has_fact) {
      add("  [math] binomial coefficient at line ", paste(has_binom, collapse = ", "),
          " but no n! factorial formula anywhere")
    } else add("  [math] binomial coefficient has a factorial formula in the deck: ok")
  }

  # 4. stats functions on a slide with no formula nearby ----------------------
  in_chunk <- rep(FALSE, length(lines))
  for (ch in chunks) in_chunk[ch$code_lines] <- TRUE
  slide_starts <- which(grepl("^#{1,2} ", lines) & !in_chunk)
  if (length(slide_starts)) {
    bounds <- c(slide_starts, length(lines) + 1)
    has_math <- logical(length(slide_starts))
    uses <- vector("list", length(slide_starts))
    for (k in seq_along(slide_starts)) {
      idx <- bounds[k]:(bounds[k + 1] - 1)
      prose <- lines[idx][!in_chunk[idx]]
      has_math[k] <- any(grepl("\\$[^$ ]|\\$\\$|\\\\\\(", prose))
      code <- lines[idx][in_chunk[idx]]
      uses[[k]] <- STATS_FUNCTIONS[vapply(STATS_FUNCTIONS, function(f)
        any(grepl(paste0("\\b", gsub("\\.", "\\\\.", f), "\\("), code)), logical(1))]
    }
    flagged <- character()
    for (k in seq_along(slide_starts)) {
      if (length(uses[[k]]) && !has_math[k] && !(k > 1 && has_math[k - 1])) {
        flagged <- c(flagged, paste0("line ", slide_starts[k], " \"",
                                     substr(sub("^#+ ", "", lines[slide_starts[k]]), 1, 40),
                                     "\" uses ", paste(uses[[k]], collapse = ", ")))
      }
    }
    if (length(flagged)) {
      add("  [math] stats functions with no formula on this or the previous slide (advisory):")
      for (f in utils::head(flagged, 10)) add("    ", f)
      if (length(flagged) > 10) add("    ... and ", length(flagged) - 10, " more")
    } else add("  [math] ok: stats functions sit near a written formula")
  }

  # 5. relative paths that do not exist ---------------------------------------
  base_dir <- dirname(path)
  refs <- character()
  img <- regmatches(lines, gregexpr("!\\[[^]]*\\]\\(([^)\\s]+)", lines, perl = TRUE))
  for (m in img) for (x in m) refs <- c(refs, sub("^!\\[[^]]*\\]\\(", "", x))
  yml_end <- which(lines == "---")[2]
  if (!is.na(yml_end)) {
    yaml_refs <- regmatches(lines[1:yml_end], gregexpr("\\.\\./[A-Za-z0-9_./-]+", lines[1:yml_end]))
    refs <- c(refs, unlist(yaml_refs))
  }
  refs <- unique(refs[!grepl("^(https?:|data:|#|mailto:)", refs) & nzchar(refs)])
  missing <- refs[!file.exists(file.path(base_dir, sub("[?#].*$", "", refs)))]
  if (length(missing)) add("  [paths] not found: ", paste(missing, collapse = ", ")) else add("  [paths] ok")

  report
}

if (!interactive() && sys.nframe() == 0) {
  decks <- commandArgs(trailingOnly = TRUE)
  if (!length(decks)) stop("usage: Rscript scripts/check_deck.R lec9/lec9 [lab4/lab4 ...]")
  for (d in decks) cat(paste(check_deck(d), collapse = "\n"), "\n\n", sep = "")
}
