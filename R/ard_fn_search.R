# The ARD tab's function search: what a user types (a statistic, an
# abbreviation, a SAS PROC, an R function, in English or Japanese) finds the
# functions that compute it.  The words come from a dictionary
# (inst/ard_search/fn_keywords.csv: `fn`, `lang`, `keyword`, `rank`
# 1 = the function for the word, 2 = related, 3 = near, a note needed;
# `note_ja`, `note_en`; `args`, the arguments the word means -- PROC
# LOGISTIC is method = "glm", ... -- which the form can start from) as well as the functions' labels, names and
# descriptions.  Words are ANDed; a function is found by how well its
# weakest word matched (the whole query, a word, a word's start, inside a
# word, the description), and by a near spelling only when little else is.

.fn_search_cache <- new.env()

# the text both sides are compared in: lower case, half-width letters,
# katakana for hiragana and half-width kana, small kana made large, no long
# vowel mark or middle dot, punctuation as spaces
.fn_norm <- function(x) {
  x <- enc2utf8(as.character(x))
  x[is.na(x)] <- ""
  u <- function(...) intToUtf8(c(...), multiple = TRUE)
  rng <- function(a, b) intToUtf8(seq(a, b))
  # full-width letters, digits and signs; the ideographic space
  x <- chartr(rng(0xFF01, 0xFF5E), rng(0x21, 0x7E), x)
  x <- gsub("\u3000", " ", x, fixed = TRUE)
  # half-width kana, then their voiced marks
  hw <- c(0x30F2, 0x30A1, 0x30A3, 0x30A5, 0x30A7, 0x30A9, 0x30E3, 0x30E5,
          0x30E7, 0x30C3, 0x30FC, 0x30A2, 0x30A4, 0x30A6, 0x30A8, 0x30AA,
          seq(0x30AB, 0x30C1, by = 2), 0x30C4, 0x30C6, 0x30C8,
          0x30CA:0x30CE, seq(0x30CF, 0x30DB, by = 3), 0x30DE:0x30E2,
          0x30E4, 0x30E6, 0x30E8, 0x30E9:0x30ED, 0x30EF, 0x30F3)
  x <- chartr(rng(0xFF66, 0xFF9D), intToUtf8(hw), x)
  x <- gsub("[\uff9e\u309b]", "\u3099", x)
  x <- gsub("[\uff9f\u309c]", "\u309a", x)
  if (any(grepl("[\u3099\u309a]", x))) {
    voiced <- c(seq(0x30AB, 0x30C1, by = 2), 0x30C4, 0x30C6, 0x30C8,
                seq(0x30CF, 0x30DB, by = 3))
    for (k in voiced) x <- gsub(paste0(u(k), "\u3099"), u(k + 1L), x, fixed = TRUE)
    x <- gsub("\u30a6\u3099", "\u30f4", x, fixed = TRUE)
    for (k in seq(0x30CF, 0x30DB, by = 3)) {
      x <- gsub(paste0(u(k), "\u309a"), u(k + 2L), x, fixed = TRUE)
    }
    x <- gsub("[\u3099\u309a]", "", x)
  }
  # hiragana as katakana; small kana large
  x <- chartr(rng(0x3041, 0x3096), rng(0x30A1, 0x30F6), x)
  x <- chartr(intToUtf8(c(0x30A1, 0x30A3, 0x30A5, 0x30A7, 0x30A9, 0x30C3, 0x30E3,
                0x30E5, 0x30E7, 0x30EE, 0x30F5, 0x30F6)),
              intToUtf8(c(0x30A2, 0x30A4, 0x30A6, 0x30A8, 0x30AA, 0x30C4, 0x30E4,
                0x30E6, 0x30E8, 0x30EF, 0x30AB, 0x30B1)), x)
  x <- tolower(x)
  # chi, squared: the ways a chi-square test is written
  x <- gsub("\u03c7", "chi", x, fixed = TRUE)
  x <- gsub("\u00b2", "2", x, fixed = TRUE)
  x <- gsub("(2|\u81ea)\u4e57", "\u4e8c\u4e57", x)
  # the long vowel mark, middle dots and "=" go; the other signs are spaces
  x <- gsub("[\u30fc\u30fb\uff65=]", "", x)
  x <- gsub("[!-/:-@[-`{-~\u3001\u3002\u00d7\u301c]", " ", x)
  trimws(gsub("\\s+", " ", x))
}

.fn_words <- function(x) {
  w <- strsplit(x, " ", fixed = TRUE)
  lapply(w, function(v) v[nzchar(v)])
}

# the dictionary as shipped
.fn_dict <- function() {
  if (is.null(.fn_search_cache$dict)) {
    f <- system.file("ard_search", "fn_keywords.csv", package = "tflplanner")
    d <- utils::read.csv(f, fileEncoding = "UTF-8", stringsAsFactors = FALSE,
                         na.strings = character(), colClasses = "character")
    d$rank <- as.integer(d$rank)
    if (!"args" %in% names(d)) d$args <- ""
    .fn_search_cache$dict <- d
  }
  .fn_search_cache$dict
}

# The dictionary for what the builder lists: a function's words, and the
# same words for the company's keyword that calls it (`continuous` ->
# cards::ard_summary) and for an old name of it.  `methods` is
# .std_ard_methods(); `functions` tfl_ard_functions().
.fn_keywords <- function(methods = NULL, functions = NULL, dict = .fn_dict()) {
  out <- dict
  if (!is.null(methods) && nrow(methods)) {
    for (i in seq_len(nrow(methods))) {
      d <- dict[dict$fn == methods$call[i], , drop = FALSE]
      if (nrow(d)) out <- rbind(out, transform(d, fn = methods$method[i]))
    }
  }
  if (!is.null(functions) && "replaced_by" %in% names(functions)) {
    old <- functions[!is.na(functions$replaced_by) & nzchar(functions$replaced_by), ,
                     drop = FALSE]
    for (i in seq_len(nrow(old))) {
      d <- dict[dict$fn == old$replaced_by[i], , drop = FALSE]
      if (nrow(d)) out <- rbind(out, transform(d, fn = old$call[i]))
    }
  }
  out <- out[!duplicated(out[c("fn", "keyword")]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Every text an entry is found by, one row each: `i` (the entry), `kind`
# ("kw", "label", "name", "desc"), the text as written and normalized,
# `rank`, the note and the arguments (a keyword's)
.fn_targets <- function(entries, keywords, extra = NULL) {
  n <- nrow(entries)
  if (!"args" %in% names(keywords)) keywords$args <- rep("", nrow(keywords))
  one <- function(i, kind, text, rank = 1L, note_ja = "", note_en = "", lang = "",
                  args = "") {
    if (!length(text)) return(NULL)
    data.frame(i = i, kind = kind, text = text, rank = rank,
               note_ja = note_ja, note_en = note_en, lang = lang, args = args,
               stringsAsFactors = FALSE)
  }
  rows <- list()
  for (i in seq_len(n)) {
    v <- entries$value[i]
    cl <- entries$call[i]
    names_i <- unique(c(v, sub("^.*::", "", v), cl, sub("^.*::", "", cl)))
    labels_i <- unique(c(entries$label[i], entries$label_en[i], extra$label[[v]]))
    desc_i <- unique(c(entries$description[i], extra$description[[v]]))
    k <- keywords[keywords$fn == v, , drop = FALSE]
    rows[[length(rows) + 1L]] <- rbind(
      one(i, "kw", k$keyword, k$rank, k$note_ja, k$note_en, k$lang, k$args),
      one(i, "label", labels_i[!is.na(labels_i) & nzchar(labels_i)]),
      one(i, "name", names_i[!is.na(names_i) & nzchar(names_i)]),
      one(i, "desc", desc_i[!is.na(desc_i) & nzchar(desc_i)]))
  }
  t <- do.call(rbind, rows)
  if (is.null(t)) {
    t <- one(integer(), character(), character())
  }
  t$norm <- .fn_norm(t$text)
  t$sq <- gsub(" ", "", t$norm, fixed = TRUE)
  t$words <- .fn_words(t$norm)
  t
}

# how a word of the query matches each target: 1 = a whole word, 2 = a
# word's start, 3 = inside, 3.5 = inside a description, Inf = not.  A short
# English word (two letters) only as a whole word, three letters also as a
# word's start; a description only for longer words.
.fn_word_tier <- function(w, t) {
  ascii <- !grepl("[^ -~]", w)
  n <- nchar(w)
  is_desc <- t$kind == "desc"
  exact <- vapply(t$words, function(x) w %in% x, NA) | t$sq == w
  pre_ok <- if (ascii) n >= 3L else TRUE
  sub_ok <- if (ascii) n >= 4L else TRUE
  pre <- pre_ok & (vapply(t$words, function(x) any(startsWith(x, w)), NA) |
                     startsWith(t$sq, w))
  # inside the text; Japanese, written without spaces, also inside the text
  # with its spaces taken out (English there would find "ttest" in
  # "fisher's exact test")
  inside <- sub_ok & (grepl(w, t$norm, fixed = TRUE) |
                        (!ascii & grepl(w, t$sq, fixed = TRUE)))
  tier <- ifelse(exact, 1, ifelse(pre, 2, ifelse(inside, 3, Inf)))
  desc_ok <- if (ascii) n >= 4L else n >= 2L
  tier[is_desc] <- ifelse(desc_ok & inside[is_desc], 3.5, Inf)
  tier
}

# a near spelling of a word: the closest word of a keyword, label or name
# within 1 edit (4 to 7 letters) or 2 (8 and more), starting with the same
# letter; Japanese only for a word all in katakana
.fn_word_near <- function(w, t) {
  n <- nchar(w)
  kata <- grepl("^[\u30a1-\u30fa]+$", w)
  ascii <- !grepl("[^ -~]", w)
  lim <- if (n >= 8L) 2L else if (n >= 4L) 1L else 0L
  none <- list(d = rep(Inf, nrow(t)), to = rep(NA_character_, nrow(t)))
  if (!lim || !(ascii || kata)) return(none)
  first <- substr(w, 1L, 1L)
  for (r in which(t$kind != "desc")) {
    cand <- unique(c(t$words[[r]], t$sq[r]))
    cand <- cand[substr(cand, 1L, 1L) == first & abs(nchar(cand) - n) <= lim]
    if (kata) cand <- cand[grepl("^[\u30a1-\u30fa]+$", cand)]
    if (!length(cand)) next
    d <- drop(utils::adist(w, cand))
    k <- which.min(d)
    if (d[k] <= lim && d[k] < none$d[r]) {
      none$d[r] <- d[k]
      none$to[r] <- cand[k]
    }
  }
  none
}

# The search: `entries` as .ard_fn_entries() gives them (the screen's
# labels), `keywords` .fn_keywords().  The found entries in order, with
# `tier` (0 = the whole query, 1 to 3.5 as .fn_word_tier(), 4 = a near
# spelling), `rank`, `hit` (the keywords matched, a SAS / R one marked),
# `note` (in `lang`), `args` (the arguments the word means, NA for none),
# `near_from` / `near_to` (a near spelling).  A
# company keyword and the function it calls are one entry (the keyword's),
# unless the function is the one chosen, in the function's category.  NULL
# for an empty query.
.fn_search <- function(q, entries, keywords, lang = "en", current = NA_character_) {
  qn <- .fn_norm(q)
  words <- .fn_words(qn)[[1L]]
  if (!length(words)) return(NULL)
  e <- entries
  # a company keyword and its function: one entry
  company <- !grepl("::", e$value, fixed = TRUE) & grepl("::", e$call, fixed = TRUE) &
    e$value %in% keywords$fn
  merged <- e$value %in% e$call[company] & !company & !e$value %in% current
  extra <- list(label = list(), description = list())
  for (k in which(merged)) {
    to <- e$value[company & e$call == e$value[k]][1L]
    extra$label[[to]] <- c(e$label[k], e$label_en[k])
    extra$description[[to]] <- e$description[k]
    # shown in the function's category
    e$category[e$value == to] <- e$category[k]
  }
  e <- e[!merged, , drop = FALSE]
  rownames(e) <- NULL
  t <- .fn_targets(e, keywords, extra)
  n <- nrow(e)
  qsq <- gsub(" ", "", qn, fixed = TRUE)
  best <- function(tier) {
    # per entry: the best tier, and among those the best rank, and the row
    o <- order(t$i, tier, t$rank)
    b <- o[!duplicated(t$i[o])]
    out <- data.frame(tier = rep(Inf, n), rank = NA_integer_, row = NA_integer_)
    out$tier[t$i[b]] <- tier[b]
    out$rank[t$i[b]] <- t$rank[b]
    out$row[t$i[b]] <- b
    out
  }
  per_word <- lapply(words, function(w) best(.fn_word_tier(w, t)))
  tier <- do.call(pmax, lapply(per_word, `[[`, "tier"))
  rank <- do.call(pmax, lapply(per_word, function(p) ifelse(is.na(p$rank), 1L, p$rank)))
  rows <- lapply(seq_len(n), function(i) vapply(per_word, function(p) p$row[i], 1L))
  # the whole query as one keyword, label or name
  whole <- t$sq == qsq & t$kind != "desc"
  if (any(whole)) {
    b0 <- best(ifelse(whole, 0, Inf))
    k0 <- is.finite(b0$tier)
    tier[k0] <- 0
    rank[k0] <- b0$rank[k0]
    rows[k0] <- as.list(b0$row[k0])
  }
  near_from <- rep(NA_character_, n)
  near_to <- rep(NA_character_, n)
  # a near spelling only when little else is found
  if (sum(is.finite(tier)) < 3L) {
    for (i in which(!is.finite(tier))) {
      ti <- which(t$i == i)
      sub <- t[ti, , drop = FALSE]
      got <- vapply(seq_along(words), function(j) {
        r <- per_word[[j]]$row[i]
        if (is.finite(per_word[[j]]$tier[i])) r else NA_integer_
      }, 1L)
      ok <- TRUE
      for (j in which(is.na(got))) {
        nr <- .fn_word_near(words[j], sub)
        if (!any(is.finite(nr$d))) {
          ok <- FALSE
          break
        }
        k <- which.min(nr$d)
        got[j] <- ti[k]
        near_from[i] <- words[j]
        near_to[i] <- nr$to[k]
      }
      if (ok) {
        tier[i] <- 4
        rank[i] <- max(t$rank[got])
        rows[[i]] <- got
      } else {
        near_from[i] <- near_to[i] <- NA_character_
      }
    }
  }
  found <- which(is.finite(tier))
  note_col <- if (identical(lang, "ja")) "note_ja" else "note_en"
  tag <- c(sas = " (SAS)", r = " (R)")
  hit <- vapply(found, function(i) {
    r <- unique(rows[[i]])
    r <- r[t$kind[r] == "kw"]
    if (!length(r)) return(NA_character_)
    paste(unique(paste0(t$text[r], ifelse(t$lang[r] %in% names(tag),
                                          tag[t$lang[r]], ""))), collapse = ", ")
  }, "")
  note <- vapply(found, function(i) {
    r <- unique(rows[[i]])
    x <- t[[note_col]][r]
    x <- unique(x[t$kind[r] == "kw" & !is.na(x) & nzchar(x)])
    if (length(x)) paste(x, collapse = " / ") else NA_character_
  }, "")
  # the arguments the word matched means (the first keyword that has any)
  args <- vapply(found, function(i) {
    r <- unique(rows[[i]])
    x <- t$args[r][t$kind[r] == "kw"]
    x <- x[!is.na(x) & nzchar(x)]
    if (length(x)) x[1L] else NA_character_
  }, "")
  out <- e[found, , drop = FALSE]
  out$tier <- tier[found]
  out$rank <- rank[found]
  out$hit <- hit
  out$note <- note
  out$args <- args
  out$near_from <- near_from[found]
  out$near_to <- near_to[found]
  out <- out[order(out$tier, out$rank, found), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# The words a function of one's own is found by: the comment lines
# `# tflplanner-keywords: odds ratio, PROC LOGISTIC` (several allowed,
# commas between the words) in the comments right above its definition in
# its file -- plain comments, not roxygen's #', so a package of the
# company's functions keeps them out of its help.  `files` R files; a data
# frame `name`, `keywords` (", " between them; NA for none).
.fn_own_keywords <- function(files) {
  out <- data.frame(name = character(), keywords = character(),
                    stringsAsFactors = FALSE)
  for (f in files[!is.na(files) & file.exists(files)]) {
    x <- readLines(f, warn = FALSE, encoding = "UTF-8")
    defs <- grep("^[A-Za-z.][A-Za-z0-9._]*[[:space:]]*(<-|=)", x)
    for (d in defs) {
      # the comment lines right above it
      k <- d - 1L
      while (k >= 1L && grepl("^[[:space:]]*#", x[k])) k <- k - 1L
      above <- if (k + 1L <= d - 1L) x[(k + 1L):(d - 1L)] else character()
      kw <- grep("^[[:space:]]*#[[:space:]]*tflplanner-keywords:", above, value = TRUE)
      words <- unlist(strsplit(sub("^[^:]*:", "", kw), "[,\u3001\uff0c]"))
      words <- unique(trimws(words))
      words <- words[nzchar(words)]
      out[nrow(out) + 1L, ] <- c(sub("^([A-Za-z.][A-Za-z0-9._]*).*$", "\\1", x[d]),
                                 if (length(words)) paste(words, collapse = ", ") else NA)
    }
  }
  out <- out[!duplicated(out$name), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# the own functions' words as dictionary rows (rank 1, no note)
.fn_own_dict <- function(own) {
  none <- .fn_dict()[0, , drop = FALSE]
  if (is.null(own) || !nrow(own) || !"keywords" %in% names(own)) return(none)
  rows <- lapply(seq_len(nrow(own)), function(i) {
    w <- if (is.na(own$keywords[i])) character() else
      trimws(strsplit(own$keywords[i], ",", fixed = TRUE)[[1L]])
    w <- w[nzchar(w)]
    if (!length(w)) return(NULL)
    data.frame(fn = own$name[i], lang = "", keyword = w, rank = 1L,
               note_ja = "", note_en = "", args = "", stringsAsFactors = FALSE)
  })
  do.call(rbind, c(list(none), rows))
}

# The company's own words (the standards' sheet ard_fn_keywords, the
# dictionary's columns): added to the built-in dictionary, a row without a
# function or a word left out, a rank blank = 1
.fn_company_dict <- function(d) {
  none <- .fn_dict()[0, , drop = FALSE]
  if (is.null(d) || !nrow(d)) return(none)
  # args: a column added later, may be left out
  if (!"args" %in% names(d)) d$args <- NA_character_
  d <- as.data.frame(lapply(d[names(none)], function(v) {
    v <- as.character(v)
    ifelse(is.na(v), "", trimws(v))
  }), stringsAsFactors = FALSE)
  d <- d[nzchar(d$fn) & nzchar(d$keyword), , drop = FALSE]
  r <- suppressWarnings(as.integer(d$rank))
  d$rank <- ifelse(is.na(r) | !r %in% 1:3, 1L, r)
  rownames(d) <- NULL
  d
}
