# Packages only some readers, ARD functions or templates use stay in
# Suggests.  When one is needed and not installed, tflplanner stops and
# names the package to install -- never skips silently (#682).  Every
# place asks .need_pkg(), so the message is the same everywhere.

# Whether a package can be loaded here (one place, so the tests can say a
# package is missing)
.pkg_installed <- function(pkg) {
  vapply(pkg, function(p) requireNamespace(p, quietly = TRUE), NA, USE.NAMES = FALSE)
}

# The message: what needs them, then which to install and how
.need_pkg_message <- function(pkg, what = NULL, lang = tflplanner_language()) {
  pkg <- unique(pkg)
  q <- encodeString(pkg, quote = "\"")
  call <- if (length(pkg) == 1L) q else paste0("c(", paste(q, collapse = ", "), ")")
  names <- if (length(pkg) == 1L) pkg else
    paste(paste(pkg[-length(pkg)], collapse = ", "), pkg[length(pkg)], sep = " & ")
  install <- sprintf(tr(if (length(pkg) == 1L) "Install the %s package: install.packages(%s)" else
    "Install the %s packages: install.packages(%s)", lang), names, call)
  if (is.null(what) || !nzchar(what)) return(install)
  paste(sprintf(tr("%s needs a package that is not installed.", lang), what), install)
}

#' Stop when a package is not installed, naming it
#'
#' @param pkg The packages needed.
#' @param what What needs them ("Reading .xpt files", "The template km"),
#'   in the app's language; `NULL` for none.
#' @return `TRUE`, invisibly, when every one is installed; otherwise an
#'   error of class `tflplanner_need_pkg` (its `packages` the missing ones).
#' @noRd
.need_pkg <- function(pkg, what = NULL) {
  pkg <- unique(as.character(pkg[!is.na(pkg) & nzchar(pkg)]))
  miss <- pkg[!.pkg_installed(pkg)]
  if (!length(miss)) return(invisible(TRUE))
  stop(structure(class = c("tflplanner_need_pkg", "error", "condition"),
                 list(message = .need_pkg_message(miss, what), call = NULL,
                      packages = miss)))
}

# ---- what needs which package ------------------------------------------------

# The templates whose programs call a package of Suggests (tflspec's
# templates do not declare their packages: kept here).  A template made
# from another (`from` in tflspec::tfl_templates(): km_survival from km, a
# company's variant) needs what that one needs.
.template_pkgs <- list(
  km = c("survival", "ggsurvfit"),
  km_table = "survival",
  forest_hr = "survival",
  ancova = "emmeans"
)

# The packages code calls by name (`survival::coxph`, a model's
# `package = "mmrm"`), and cardx's ARD functions that call one
# (cardx::ard_survival_survfit() needs survival)
.code_pkgs <- c("survival", "ggsurvfit", "emmeans", "mmrm")
.ard_fn_pkgs <- c(ard_survival_ = "survival", ard_emmeans_ = "emmeans")

# The packages a piece of code (or a method name) calls
.pkgs_in_code <- function(code) {
  code <- paste(as.character(unlist(code)), collapse = "\n")
  if (!nzchar(code)) return(character())
  hit <- .code_pkgs[vapply(.code_pkgs, function(p) grepl(
    paste0("\\b", p, "::|package\\s*=\\s*[\"']", p, "[\"']"), code, perl = TRUE), NA)]
  fam <- .ard_fn_pkgs[vapply(names(.ard_fn_pkgs), function(f)
    grepl(paste0("\\bcardx::", f), code, perl = TRUE), NA)]
  unique(c(hit, unname(fam)))
}

# The packages an ARD function needs: its own (cards, cardx) and the one
# it calls (survival for ard_survival_*)
.ard_fn_needs <- function(fn) {
  own <- if (grepl("::", fn, fixed = TRUE)) sub("::.*$", "", fn) else character()
  unique(c(own, .pkgs_in_code(fn)))
}

# A template's packages, through the templates it is made from
.template_needs <- function(name, templates = NULL) {
  templates <- templates %||% tryCatch(tflspec::tfl_templates(), error = function(e) NULL)
  out <- character()
  seen <- character()
  while (length(name) == 1L && !is.na(name) && nzchar(name) && !name %in% seen) {
    seen <- c(seen, name)
    out <- c(out, .template_pkgs[[name]])
    k <- if (is.data.frame(templates)) match(name, templates$name) else NA_integer_
    name <- if (is.na(k)) NA_character_ else templates$from[k]
  }
  unique(out)
}

# A report's packages of Suggests: its template's, its file's code (its
# own `ard:`, its R items), its ARD analyses', its designed figure's
.report_needs <- function(p, output_id, templates = NULL) {
  o <- tryCatch(.code_spec(p)$outputs[[output_id]], error = function(e) NULL) %||%
    p$spec$outputs[[output_id]]
  tpl <- if (is.list(o$template)) as.character(o$template$name %||% NA_character_) else NA_character_
  an <- tryCatch(ard_rows(p, "analyses", output_id), error = function(e) NULL)
  fig <- tryCatch({
    d <- fig_design(p, output_id)
    if (is.null(d)) character() else attr(tflspec::tfl_fig_design_code(
      d, output_id, setup = TRUE, save = FALSE), "libs")
  }, error = function(e) character())
  unique(c(.template_needs(tpl, templates),
           if (!is.null(o)) .pkgs_in_code(tryCatch(deparse(unclass(o)), error = function(e) "")),
           if (NROW(an)) .pkgs_in_code(c(an$method, an$args, an$code)),
           intersect(fig, .code_pkgs)))
}

# Before reports run: stop when one needs a package not installed, naming
# the reports and the packages
.need_report_pkgs <- function(p, output_ids) {
  if (!length(output_ids)) return(invisible(TRUE))
  tt <- tryCatch(tflspec::tfl_templates(), error = function(e) NULL)
  needs <- lapply(stats::setNames(output_ids, output_ids), function(id)
    tryCatch(.report_needs(p, id, tt), error = function(e) character()))
  all <- unique(unlist(needs))
  miss <- all[!.pkg_installed(all)]
  if (!length(miss)) return(invisible(TRUE))
  ids <- names(needs)[vapply(needs, function(n) any(n %in% miss), NA)]
  .need_pkg(miss, sprintf(tr(if (length(ids) == 1L) "The report %s" else "The reports %s"),
                          paste(ids, collapse = ", ")))
}

# ---- the ARD tab's functions -------------------------------------------------

# The functions the analyses grid's `method` column offers: cards' and
# cardx's ard_*().  A package not installed: its functions from tflspec's
# catalog, still listed -- choosing one says what to install.
.ard_function_choices <- function(catalog = NULL) {
  fs <- character()
  for (pkg in c("cards", "cardx")) {
    if (.pkg_installed(pkg)) {
      e <- getNamespaceExports(pkg)
      fs <- c(fs, paste0(pkg, "::", sort(e[startsWith(e, "ard_")])))
    } else {
      catalog <- catalog %||% tryCatch(tflspec::tfl_ard_functions(installed = FALSE),
                                       error = function(e) NULL)
      cl <- as.character(catalog$call)
      fs <- c(fs, sort(cl[startsWith(cl, paste0(pkg, "::"))]))
    }
  }
  fs
}

# tflspec's ARD functions as the analysis form lists them: the ones that can
# be called, and those whose package -- or the package they call -- is not
# installed, with `needs` (the missing packages, comma-separated; "" none)
.ard_functions_offered <- function(f = tflspec::tfl_ard_functions(installed = FALSE)) {
  needs <- lapply(f$call, .ard_fn_needs)
  pk <- unique(unlist(needs))
  have <- stats::setNames(.pkg_installed(pk), pk)
  f$needs <- vapply(needs, function(n) paste(n[!have[n]], collapse = ","), "")
  # (one the installed version lacks, its package there: not listed, as before)
  f <- f[f$installed | nzchar(f$needs), , drop = FALSE]
  # (callable once its package is installed: the form offers it)
  f$installed <- TRUE
  rownames(f) <- NULL
  f
}

# Stop when an analysis method -- a function, or a company keyword's
# function -- needs a package that is not installed
.need_ard_methods <- function(methods) {
  methods <- unique(as.character(methods[!is.na(methods) & nzchar(methods)]))
  if (!length(methods)) return(invisible(TRUE))
  keys <- tryCatch(.std_ard_methods(), error = function(e) NULL)
  call <- if (is.null(keys)) methods else
    ifelse(methods %in% keys$method, as.character(keys$call[match(methods, keys$method)]), methods)
  for (i in seq_along(methods)) {
    if (is.na(call[i])) next
    .need_pkg(.ard_fn_needs(call[i]), call[i])
  }
  invisible(TRUE)
}

# The reports a run_batch() call runs: every one, or those `only` names
# (by output id or program), less `exclude`, within the named `batch`
.batch_report_ids <- function(x, only = NULL, exclude = NULL, batch = NULL) {
  ids <- x$outputs$output_id
  prog <- vapply(ids, function(id) tryCatch(as.character(report_info(x, id)$program),
                                            error = function(e) NA_character_), "",
                 USE.NAMES = FALSE)
  on <- function(v) ids %in% v | (!is.na(prog) & prog %in% v)
  keep <- if (length(only)) on(only) else rep(TRUE, length(ids))
  if (length(exclude)) keep <- keep & !on(exclude)
  if (length(batch)) keep <- keep & ids %in% unlist(batch_sets(x)[batch])
  ids[keep]
}
