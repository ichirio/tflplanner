# data-raw/logo.R -- the hex logo, and the favicons made from it.
#
# rtfreporter's logo (rtfreporter/man/figures/logo.png) is the model of the
# family and is kept as it is.  tflspec and tflplanner follow it: the same
# pointy-top hexagon (560 x 620 canvas), the same navy gradient, top gloss,
# bottom vignette, drop shadow, dark border and thin inner highlight, the
# same pale-blue paper card with its shadow, the same large white serif name
# and the same small spaced tagline under it.  Only the card differs, and
# the tagline takes a pale tint of the package's accent colour:
#
#   tflspec     a spec sheet (output_id | analysis | method) and an arrow to
#               a few lines of R code                       accent: green
#   tflplanner  a planning board: TFL IDs with status marks, and a small
#               progress panel                               accent: amber
#
# This script is the same file in tflspec and tflplanner.  Change it in one
# and copy it to the other, so that the two stay alike.
#
# Run it from the package root:  Rscript data-raw/logo.R
#
# It writes
#   man/figures/logo.svg, man/figures/logo.png   the sticker (560 x 620)
#   pkgdown/favicon/*                             the site's favicons
#   data-raw/logo-family.png                      rtfreporter, tflspec and
#                                                 tflplanner side by side
#
# Fonts: Liberation Serif and Liberation Mono (SIL Open Font License 1.1,
# https://github.com/liberationfonts/liberation-fonts).  Liberation Serif is
# the serif rtfreporter's PNG is drawn in.  The SVG names the fonts and the
# PNGs are rendered from it with rsvg, which finds them through fontconfig;
# the script stops if they are not installed (Debian / Ubuntu:
# fonts-liberation2).
#
# rtfreporter's logo for logo-family.png is read from a sibling checkout
# (../rtfreporter) if there is one, else from GitHub.
#
# Needs (developer only, not in DESCRIPTION): rsvg and magick.
# pkgdown::build_favicons() is not used: it sends the logo to the
# RealFaviconGenerator web service.

stopifnot(
  file.exists("DESCRIPTION"),
  requireNamespace("rsvg", quietly = TRUE),
  requireNamespace("magick", quietly = TRUE)
)
pkg <- unname(read.dcf("DESCRIPTION", fields = "Package")[1, 1])

serif <- "Liberation Serif"
mono  <- "Liberation Mono"
for (fam in c(serif, mono)) {
  got <- system2("fc-match", c("-f", "'%{family}'", shQuote(fam)), stdout = TRUE)
  if (!grepl(fam, paste(got, collapse = " "), fixed = TRUE)) {
    stop(fam, " is not installed (fonts-liberation2); the PNGs need it.")
  }
}
font_serif <- sprintf("%s, Times New Roman, serif", serif)
font_mono  <- sprintf("%s, Courier New, monospace", mono)

# -- the family --------------------------------------------------------------

family <- list(
  tflspec    = list(accent = "#3dbb8a", tint = "#cdeedd",
                    tagline = "SPEC \u00b7 CODE"),
  tflplanner = list(accent = "#f2a33a", tint = "#f7e2bf",
                    tagline = "PLAN \u00b7 RUN")
)
rtfreporter_png <- "../rtfreporter/man/figures/logo.png"
rtfreporter_url <- paste0("https://raw.githubusercontent.com/ichirio/",
                          "rtfreporter/main/man/figures/logo.png")

W <- 560; H <- 620
hex   <- "280,28 512,163 512,431 280,566 48,431 48,163"
inner <- "280,38 502,168 502,425 280,556 58,425 58,168"

# Card ink, as on rtfreporter's page.
ink_title  <- "#1f4e79"
ink_head   <- "#1f3a5a"
ink_body   <- "#3a4a5a"
ink_meta   <- "#4a5a6a"
ink_note   <- "#6a7886"
rule       <- "#1a3a5a"
hair       <- "#b9cbdb"

# -- SVG helpers -------------------------------------------------------------

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

txt <- function(x, y, label, size, anchor = "start", fill = ink_body,
                weight = NULL, style = NULL, font = font_serif, extra = "") {
  sprintf('<text x="%s" y="%s" font-family="%s" font-size="%s"%s%s text-anchor="%s" fill="%s"%s>%s</text>',
          x, y, font, size,
          if (is.null(weight)) "" else sprintf(' font-weight="%s"', weight),
          if (is.null(style)) "" else sprintf(' font-style="%s"', style),
          anchor, fill, extra, esc(label))
}

hline <- function(x1, x2, y, col = rule, lwd = 1.5) {
  sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="%s" stroke-width="%s"/>',
          x1, y, x2, y, col, lwd)
}

vline <- function(x, y1, y2, col = hair, lwd = 1) {
  sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="%s" stroke-width="%s"/>',
          x, y1, x, y2, col, lwd)
}

# -- shared frame (rtfreporter's, unchanged) ----------------------------------

defs <- c(
  "<defs>",
  '<linearGradient id="hexbg" x1="50%" y1="0%" x2="50%" y2="100%">',
  '<stop offset="0%" stop-color="#2d6fa8"/>',
  '<stop offset="60%" stop-color="#1a4a78"/>',
  '<stop offset="100%" stop-color="#0e2e4d"/>',
  "</linearGradient>",
  '<radialGradient id="topgloss" cx="50%" cy="0%" r="65%" fx="50%" fy="0%">',
  '<stop offset="0%" stop-color="#ffffff" stop-opacity="0.32"/>',
  '<stop offset="40%" stop-color="#ffffff" stop-opacity="0.10"/>',
  '<stop offset="100%" stop-color="#ffffff" stop-opacity="0"/>',
  "</radialGradient>",
  '<radialGradient id="vignette" cx="50%" cy="100%" r="80%" fx="50%" fy="100%">',
  '<stop offset="0%" stop-color="#000000" stop-opacity="0"/>',
  '<stop offset="80%" stop-color="#000000" stop-opacity="0"/>',
  '<stop offset="100%" stop-color="#000000" stop-opacity="0.30"/>',
  "</radialGradient>",
  '<linearGradient id="paperbg" x1="50%" y1="0%" x2="50%" y2="100%">',
  '<stop offset="0%" stop-color="#f3f8fd"/>',
  '<stop offset="100%" stop-color="#dde9f4"/>',
  "</linearGradient>",
  '<filter id="hex-shadow" x="-12%" y="-8%" width="124%" height="124%">',
  '<feDropShadow dx="0" dy="9" stdDeviation="11" flood-color="#000913" flood-opacity="0.45"/>',
  "</filter>",
  '<filter id="paper-shadow" x="-8%" y="-8%" width="116%" height="120%">',
  '<feDropShadow dx="0" dy="3" stdDeviation="4" flood-color="#0a1c30" flood-opacity="0.55"/>',
  "</filter>",
  '<filter id="text-shadow" x="-10%" y="-50%" width="120%" height="200%">',
  '<feDropShadow dx="0" dy="2" stdDeviation="2" flood-color="#000913" flood-opacity="0.55"/>',
  "</filter>",
  sprintf('<clipPath id="hex-clip"><polygon points="%s"/></clipPath>', hex),
  "</defs>"
)

body <- c(
  sprintf('<g filter="url(#hex-shadow)"><polygon points="%s" fill="url(#hexbg)"/></g>', hex),
  '<g clip-path="url(#hex-clip)">',
  '<rect x="0" y="0" width="560" height="620" fill="url(#topgloss)"/>',
  '<rect x="0" y="0" width="560" height="620" fill="url(#vignette)"/>',
  "</g>"
)

paper <- c(
  paste0('<rect x="92" y="138" width="376" height="252" rx="7" ry="7" ',
         'fill="url(#paperbg)" stroke="#9fb6c8" stroke-width="1.4" ',
         'filter="url(#paper-shadow)"/>'),
  '<line x1="98" y1="141" x2="462" y2="141" stroke="#ffffff" stroke-width="1" opacity="0.8"/>'
)

outline <- c(
  sprintf(paste0('<polygon points="%s" fill="none" stroke="#0d2840" ',
                 'stroke-width="14" stroke-linejoin="round"/>'), hex),
  sprintf(paste0('<polygon points="%s" fill="none" stroke="#ffffff" ',
                 'stroke-width="1.5" stroke-linejoin="round" opacity="0.18"/>'), inner)
)

wordmark <- function(p, tint, tagline) c(
  txt(280, 490, p, 46, "middle", "#ffffff", weight = 700,
      extra = ' letter-spacing="-0.5" filter="url(#text-shadow)"'),
  txt(280, 522, tagline, 13.5, "middle", tint, weight = 500,
      extra = ' letter-spacing="3.5"'),
  txt(280, 555, "i c h i r i o", 11, "middle", "#ffffff", weight = 500,
      extra = ' opacity="0.32" letter-spacing="6"')
)

# -- the cards ----------------------------------------------------------------

# A spec sheet -- a grid with output_id | analysis | method -- and an arrow
# down to the R code it is turned into.
card_tflspec <- function(accent) {
  x1 <- 108; x2 <- 452
  cols <- c(116, 206, 330)          # left edges of the three columns
  rows <- c("T-14-1-1", "Demographics", "summary",
            "T-14-2-1", "Disposition",  "count",
            "T-14-3-1", "AE by SOC",    "count")
  rows <- matrix(rows, ncol = 3, byrow = TRUE)
  grid_top <- 184; head_h <- 20; row_h <- 18
  grid_bot <- grid_top + head_h + nrow(rows) * row_h
  code <- c("adsl |>",
            "  ard_continuous(by = TRT01A,",
            "                 variables = AGE)")
  c(
    txt(x1, 166, "tfl_spec.xlsx", 12, fill = ink_meta),
    txt(x2, 166, "Sheet: ARD", 12, "end", fill = ink_meta),
    # the sheet: a header row in the accent colour, then a light grid
    sprintf('<rect x="%s" y="%s" width="%s" height="%s" fill="%s" opacity="0.22"/>',
            x1, grid_top, x2 - x1, head_h, accent),
    hline(x1, x2, grid_top),
    hline(x1, x2, grid_top + head_h),
    unlist(lapply(seq_len(nrow(rows) - 1), function(i)
      hline(x1, x2, grid_top + head_h + i * row_h, hair, 1))),
    hline(x1, x2, grid_bot),
    vline(cols[2] - 8, grid_top, grid_bot), vline(cols[3] - 8, grid_top, grid_bot),
    txt(cols[1], grid_top + 14, "output_id", 11, fill = ink_head, weight = 600),
    txt(cols[2], grid_top + 14, "analysis",  11, fill = ink_head, weight = 600),
    txt(cols[3], grid_top + 14, "method",    11, fill = ink_head, weight = 600),
    unlist(lapply(seq_len(nrow(rows)), function(i) {
      y <- grid_top + head_h + i * row_h - 5
      c(txt(cols[1], y, rows[i, 1], 10.5),
        txt(cols[2], y, rows[i, 2], 10.5),
        txt(cols[3], y, rows[i, 3], 10.5))
    })),
    # the arrow
    sprintf(paste0('<path d="M 280 %s L 280 %s M 271 %s L 280 %s L 289 %s" ',
                   'fill="none" stroke="%s" stroke-width="3" ',
                   'stroke-linecap="round" stroke-linejoin="round"/>'),
            grid_bot + 6, grid_bot + 24, grid_bot + 16, grid_bot + 25,
            grid_bot + 16, accent),
    # the R code, on a slightly darker panel with an accent bar
    sprintf('<rect x="%s" y="%s" width="%s" height="%s" rx="4" fill="#ccdcea"/>',
            x1, grid_bot + 32, x2 - x1, 66),
    sprintf('<rect x="%s" y="%s" width="3" height="%s" fill="%s"/>',
            x1, grid_bot + 32, 66, accent),
    unlist(lapply(seq_along(code), function(i)
      txt(x1 + 14, grid_bot + 32 + 18 * i, code[i], 11, fill = ink_title,
          font = font_mono, extra = ' xml:space="preserve"'))),
    txt(x1, 378, "Program written by tfl_ard_code().", 9.5,
        fill = ink_note, style = "italic")
  )
}

# A planning board: the study's TFL IDs with their status marks, and a small
# panel with the progress.
card_tflplanner <- function(accent) {
  x1 <- 108; x2 <- 452
  done <- "#3d8f5f"
  items <- data.frame(
    id     = c("T-14-1-1", "T-14-1-2", "T-14-3-1", "L-16-2-1", "F-14-2-1"),
    title  = c("Demographics", "Disposition", "AE by SOC", "AE listing",
               "KM plot"),
    status = c("done", "done", "run", "run", "todo")
  )
  mark <- function(x, y, s) switch(s,
    done = c(sprintf('<circle cx="%s" cy="%s" r="6.5" fill="%s"/>', x, y, done),
             sprintf(paste0('<path d="M %s %s L %s %s L %s %s" fill="none" ',
                            'stroke="#ffffff" stroke-width="1.8" ',
                            'stroke-linecap="round" stroke-linejoin="round"/>'),
                     x - 3.2, y + 0.2, x - 0.8, y + 2.6, x + 3.4, y - 2.6)),
    run  = sprintf('<circle cx="%s" cy="%s" r="6.5" fill="%s"/>', x, y, accent),
    todo = sprintf('<circle cx="%s" cy="%s" r="5.8" fill="none" stroke="%s" stroke-width="1.4"/>',
                   x, y, ink_note)
  )
  list_x2 <- 334; top <- 196; row_h <- 26
  px <- 346; pw <- x2 - px
  c(
    txt(x1, 166, "Study XYZ-001", 12, fill = ink_meta),
    txt(x2, 166, "TFL plan", 12, "end", fill = ink_meta),
    # the list
    hline(x1, list_x2, top - 4),
    txt(x1 + 8, top + 10, "Output", 11, fill = ink_head, weight = 600),
    txt(list_x2 - 4, top + 10, "Status", 11, "end", fill = ink_head, weight = 600),
    hline(x1, list_x2, top + 18),
    unlist(lapply(seq_len(nrow(items)), function(i) {
      y <- top + 18 + row_h * i - 8
      c(txt(x1 + 8, y, items$id[i], 10.5, fill = ink_head, weight = 600),
        txt(x1 + 72, y, items$title[i], 10.5),
        mark(list_x2 - 18, y - 3.5, items$status[i]),
        if (i < nrow(items)) hline(x1, list_x2, y + 9, hair, 1))
    })),
    hline(x1, list_x2, top + 18 + row_h * nrow(items) + 4),
    # the panel
    sprintf(paste0('<rect x="%s" y="%s" width="%s" height="%s" rx="5" ',
                   'fill="#ffffff" fill-opacity="0.55" stroke="#9fb6c8" stroke-width="1"/>'),
            px, top - 4, pw, 156),
    sprintf('<rect x="%s" y="%s" width="%s" height="3" rx="1.5" fill="%s"/>',
            px + 8, top + 4, pw - 16, accent),
    txt(px + pw / 2, top + 24, "Progress", 11, "middle", ink_head, weight = 600),
    txt(px + pw / 2, top + 62, "2 / 5", 26, "middle", ink_title, weight = 700),
    txt(px + pw / 2, top + 78, "outputs done", 9.5, "middle", ink_note),
    # a stacked bar: done | running | to do
    sprintf('<rect x="%s" y="%s" width="%s" height="8" rx="4" fill="#c4d4e2"/>',
            px + 12, top + 92, pw - 24),
    sprintf('<rect x="%s" y="%s" width="%s" height="8" rx="4" fill="%s"/>',
            px + 12, top + 92, (pw - 24) * 4 / 5, accent),
    sprintf('<rect x="%s" y="%s" width="%s" height="8" rx="4" fill="%s"/>',
            px + 12, top + 92, (pw - 24) * 2 / 5, done),
    mark(px + 18, top + 120, "done"), txt(px + 30, top + 124, "2", 10.5),
    mark(px + 50, top + 120, "run"),  txt(px + 62, top + 124, "2", 10.5),
    mark(px + 82, top + 120, "todo"), txt(px + 94, top + 124, "1", 10.5),
    txt(x1, 378, "Last official run: runs/2026-10-01_0930_all/", 9.5,
        fill = ink_note, style = "italic")
  )
}

# -- the sticker --------------------------------------------------------------

# `name = FALSE` gives the favicon: the hexagon and its card, no text.
sticker <- function(p, name = TRUE) {
  fm <- family[[p]]
  card <- get(paste0("card_", p))(fm$accent)
  c('<?xml version="1.0" encoding="UTF-8"?>',
    sprintf(paste0('<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" ',
                   'viewBox="0 0 %d %d">'), W, H, W, H),
    sprintf("<title>%s</title>", p),
    defs, body,
    '<g clip-path="url(#hex-clip)">', paper, card, "</g>",
    if (name) wordmark(p, fm$tint, fm$tagline),
    outline,
    "</svg>")
}

# Without the time stamps magick writes, so that a re-run changes nothing.
save <- function(img, file, ...) magick::image_write(magick::image_strip(img), file, ...)

render <- function(svg, file, width, height = width * H / W) {
  tmp <- tempfile(fileext = ".svg")
  writeLines(svg, tmp, useBytes = TRUE)
  rsvg::rsvg_png(tmp, file, width = round(width), height = round(height))
}

# A square image of the sticker, for the favicons: the hexagon only (the
# canvas below it holds the drop shadow), centred.
square <- function(svg, size, bg = "none") {
  tmp <- tempfile(fileext = ".png")
  render(svg, tmp, W * 2)
  img <- magick::image_read(tmp)
  img <- magick::image_crop(img, sprintf("%dx%d+%d+%d", 2 * 478, 2 * 552, 2 * 41, 2 * 21))
  img <- magick::image_resize(img, sprintf("%dx%d", size, size))
  magick::image_extent(magick::image_background(img, bg), sprintf("%dx%d", size, size),
                       gravity = "center", color = bg)
}

# -- write ---------------------------------------------------------------------

stopifnot(pkg %in% names(family))
dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("pkgdown/favicon", recursive = TRUE, showWarnings = FALSE)

svg <- sticker(pkg)
writeLines(svg, "man/figures/logo.svg", useBytes = TRUE)
render(svg, "man/figures/logo.png", W)

icon <- sticker(pkg, name = FALSE)
writeLines(icon, "pkgdown/favicon/favicon.svg", useBytes = TRUE)
save(square(icon, 96), "pkgdown/favicon/favicon-96x96.png")
save(square(icon, 180, "#ffffff"), "pkgdown/favicon/apple-touch-icon.png")
for (n in c(192, 512)) {
  save(square(icon, n, "#ffffff"),
       sprintf("pkgdown/favicon/web-app-manifest-%dx%d.png", n, n))
}
save(magick::image_join(lapply(c(16, 32, 48), function(n) square(icon, n))),
     "pkgdown/favicon/favicon.ico", format = "ico")
writeLines(c(
  "{",
  sprintf('  "name": "%s",', pkg),
  sprintf('  "short_name": "%s",', pkg),
  '  "icons": [',
  '    {', '      "src": "web-app-manifest-192x192.png",', '      "sizes": "192x192",',
  '      "type": "image/png",', '      "purpose": "maskable"', '    },',
  '    {', '      "src": "web-app-manifest-512x512.png",', '      "sizes": "512x512",',
  '      "type": "image/png",', '      "purpose": "maskable"', '    }',
  '  ],',
  '  "theme_color": "#ffffff",',
  '  "background_color": "#ffffff",',
  '  "display": "standalone"',
  "}"), "pkgdown/favicon/site.webmanifest")

# The family, side by side, 300 px high each: rtfreporter's committed logo,
# then tflspec and tflplanner as drawn here.
rtf <- if (file.exists(rtfreporter_png)) rtfreporter_png else {
  tmp <- tempfile(fileext = ".png")
  utils::download.file(rtfreporter_url, tmp, mode = "wb", quiet = TRUE)
  tmp
}
stickers <- c(
  list(magick::image_read(rtf)),
  lapply(names(family), function(p) {
    tmp <- tempfile(fileext = ".png")
    render(sticker(p), tmp, W)
    magick::image_read(tmp)
  })
)
stickers <- lapply(stickers, function(img)
  magick::image_border(magick::image_resize(img, "x300"), "none", "12x12"))
save(magick::image_append(do.call(c, stickers)), "data-raw/logo-family.png")
