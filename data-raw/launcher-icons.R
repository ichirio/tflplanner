# The launcher's icons: inst/launcher/tflplanner.ico (Windows), .icns (macOS)
# and .png (Linux), made from the package's hex logo.  add_shortcut() copies
# them from inst/launcher as they are.
#
#   Rscript data-raw/launcher-icons.R      (from the package root)
#
# Run data-raw/logo.R first if the logo itself changed: the large sizes are
# rendered from man/figures/logo.svg.
#
#   48 px and up   the logo as it is: hexagon, card and name
#   16, 24, 32 px  a simplified mark: the hexagon and its light border only
#                  (no card, no text), so it stays clean at taskbar size
#
#   tflplanner.ico    16, 24, 32, 48, 64, 128 and 256 px
#   tflplanner.png    256 px (the large logo)
#   tflplanner.icns   `ic08` (256 px) and `ic07` (128 px), PNG elements
#   data-raw/launcher-icon-preview.png   the 16, 32, 48 and 256 px renders
#                                        side by side (small ones enlarged)
#
# Fonts: the SVG names Liberation Serif / Mono, found by rsvg through
# fontconfig, as in data-raw/logo.R (Debian / Ubuntu: fonts-liberation2).
#
# Needs (developer only, not in DESCRIPTION): rsvg and magick.

stopifnot(
  file.exists("DESCRIPTION"),
  file.exists("man/figures/logo.svg"),
  requireNamespace("rsvg", quietly = TRUE),
  requireNamespace("magick", quietly = TRUE)
)
for (fam in c("Liberation Serif", "Liberation Mono")) {
  got <- system2("fc-match", c("-f", "'%{family}'", shQuote(fam)), stdout = TRUE)
  if (!grepl(fam, paste(got, collapse = " "), fixed = TRUE)) {
    stop(fam, " is not installed (fonts-liberation2); the icons need it.")
  }
}

out <- file.path("inst", "launcher")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
sizes <- c(16, 24, 32, 48, 64, 128, 256)
small <- sizes < 48

# -- the sources ----------------------------------------------------------------

# The hexagon, its gradient and gloss, its dark border and a light border
# (the logo's own inner highlight, drawn stronger so that it shows at 16 px).
# The colours are the logo's (data-raw/logo.R, family$tflplanner).
hex   <- "280,28 512,163 512,431 280,566 48,431 48,163"
inner <- "280,66 476,180 476,414 280,528 84,414 84,180"
mark_svg <- c(
  '<?xml version="1.0" encoding="UTF-8"?>',
  '<svg xmlns="http://www.w3.org/2000/svg" width="560" height="620" viewBox="0 0 560 620">',
  "<defs>",
  '<linearGradient id="hexbg" x1="50%" y1="0%" x2="50%" y2="100%">',
  '<stop offset="0%" stop-color="#a8424f"/>',
  '<stop offset="60%" stop-color="#7a2734"/>',
  '<stop offset="100%" stop-color="#4d1520"/>',
  "</linearGradient>",
  '<radialGradient id="topgloss" cx="50%" cy="0%" r="65%" fx="50%" fy="0%">',
  '<stop offset="0%" stop-color="#ffffff" stop-opacity="0.28"/>',
  '<stop offset="100%" stop-color="#ffffff" stop-opacity="0"/>',
  "</radialGradient>",
  sprintf('<clipPath id="hex-clip"><polygon points="%s"/></clipPath>', hex),
  "</defs>",
  sprintf('<polygon points="%s" fill="url(#hexbg)"/>', hex),
  '<g clip-path="url(#hex-clip)"><rect width="560" height="620" fill="url(#topgloss)"/></g>',
  sprintf('<polygon points="%s" fill="none" stroke="#3e1019" stroke-width="30" stroke-linejoin="round"/>', hex),
  sprintf('<polygon points="%s" fill="none" stroke="#f7e2bf" stroke-width="36" stroke-linejoin="round"/>', inner),
  "</svg>")

render <- function(svg_lines, px) {
  tmp <- tempfile(fileext = ".svg")
  writeLines(svg_lines, tmp, useBytes = TRUE)
  png <- tempfile(fileext = ".png")
  rsvg::rsvg_png(tmp, png, width = 4 * 560, height = 4 * 620)
  img <- magick::image_read(png)
  # the hexagon only (the canvas below the logo holds the drop shadow),
  # fitted into a transparent square
  img <- magick::image_crop(img, sprintf("%dx%d+%d+%d", 4 * 478, 4 * 552, 4 * 41, 4 * 21))
  img <- magick::image_resize(img, sprintf("%dx%d", px, px), filter = "Lanczos")
  magick::image_extent(magick::image_background(img, "none"),
                       sprintf("%dx%d", px, px), gravity = "center", color = "none")
}

logo_svg <- readLines("man/figures/logo.svg", warn = FALSE)
icon <- function(px) render(if (px < 48) mark_svg else logo_svg, px)
imgs <- lapply(sizes, icon)
names(imgs) <- sizes

# Without the time stamps magick writes, so that a re-run changes nothing.
strip <- magick::image_strip
png_raw <- function(px) magick::image_write(strip(imgs[[as.character(px)]]), format = "png")

# -- .png (Linux) ----------------------------------------------------------------

magick::image_write(strip(imgs[["256"]]), file.path(out, "tflplanner.png"), format = "png")

# -- .ico (Windows): all sizes; ImageMagick stores 256 px as PNG ------------------

magick::image_write(strip(do.call(c, unname(imgs))), file.path(out, "tflplanner.ico"),
                    format = "ico")

# -- .icns (macOS): `ic08` (256 px) first, then `ic07` (128 px) -------------------

u32be <- function(x) writeBin(as.integer(x), raw(), size = 4, endian = "big")
element <- function(type, px) {
  p <- png_raw(px)
  c(charToRaw(type), u32be(8 + length(p)), p)
}
els <- c(element("ic08", 256), element("ic07", 128))
writeBin(c(charToRaw("icns"), u32be(8 + length(els)), els),
         file.path(out, "tflplanner.icns"))

# -- the preview -----------------------------------------------------------------

# 16, 32, 48 and 256 px on a light and a dark tile; the small ones are shown
# at actual size (left of each pair) and enlarged x4 without smoothing.
tile <- function(img, bg, scale) {
  px <- magick::image_info(img)$width
  if (scale > 1) img <- magick::image_scale(img, sprintf("%dx%d", px * scale, px * scale))
  magick::image_extent(
    magick::image_composite(magick::image_blank(280, 310, bg), img, gravity = "north", offset = "+0+12"),
    "280x310", color = bg, gravity = "north")
}
label <- function(img, text, bg) {
  magick::image_annotate(img, text, gravity = "south", size = 16, font = "Liberation Sans",
                         color = if (bg == "#ffffff") "#333333" else "#dddddd",
                         location = "+0+8")
}
cells <- function(bg) {
  px  <- c(16, 32, 48, 256)
  scl <- c(8, 4, 4, 1)
  do.call(c, Map(function(p, s) {
    label(tile(imgs[[as.character(p)]], bg, s),
          sprintf("%d px%s", p, if (s > 1) sprintf(" (x%d)", s) else ""), bg)
  }, px, scl))
}
prev <- magick::image_append(c(magick::image_append(cells("#ffffff")),
                               magick::image_append(cells("#202428"))), stack = TRUE)
magick::image_write(strip(prev), "data-raw/launcher-icon-preview.png", format = "png")

info <- magick::image_info(magick::image_read(file.path(out, "tflplanner.ico")))
print(info[, c("format", "width", "height")])
stopifnot(identical(sort(info$width), as.integer(sizes)))
cat("wrote", out, "\n")
