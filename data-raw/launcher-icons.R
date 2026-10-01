# The launcher's icons: inst/launcher/tflplanner.png (Linux), .ico (Windows)
# and .icns (macOS), drawn here with base graphics.  Both containers carry
# the PNG itself (an .ico entry and an .icns `ic08` entry may be PNG), so no
# image library is needed.
#
#   Rscript data-raw/launcher-icons.R

out <- file.path("inst", "launcher")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
png_file <- file.path(out, "tflplanner.png")

grDevices::png(png_file, width = 256, height = 256, bg = "transparent",
               type = if (capabilities("cairo")) "cairo" else getOption("bitmapType"))
graphics::par(mar = c(0, 0, 0, 0))
graphics::plot.new()
graphics::plot.window(c(0, 1), c(0, 1), xaxs = "i", yaxs = "i")
# a rounded square in the docs' blue
r <- 0.16
th <- seq(0, pi / 2, length.out = 20)
corner <- function(cx, cy, a0) cbind(cx + r * cos(th + a0), cy + r * sin(th + a0))
pts <- rbind(corner(1 - r - .02, 1 - r - .02, 0), corner(r + .02, 1 - r - .02, pi / 2),
             corner(r + .02, r + .02, pi), corner(1 - r - .02, r + .02, 3 * pi / 2))
graphics::polygon(pts, col = "#1f4e79", border = NA)
# three table rules and the name
graphics::segments(0.18, c(0.70, 0.56, 0.30), 0.82, c(0.70, 0.56, 0.30),
                   col = "white", lwd = c(6, 3, 6))
graphics::text(0.5, 0.43, "TFL", col = "white", font = 2, cex = 4.2)
grDevices::dev.off()

png <- readBin(png_file, "raw", file.size(png_file))
u32be <- function(x) writeBin(as.integer(x), raw(), size = 4, endian = "big")
u32le <- function(x) writeBin(as.integer(x), raw(), size = 4, endian = "little")
u16le <- function(x) writeBin(as.integer(x), raw(), size = 2, endian = "little")

# .ico: ICONDIR + one ICONDIRENTRY (0 = 256 px) + the PNG
ico <- c(u16le(0), u16le(1), u16le(1),
         as.raw(0), as.raw(0), as.raw(0), as.raw(0),
         u16le(1), u16le(32), u32le(length(png)), u32le(22), png)
writeBin(ico, file.path(out, "tflplanner.ico"))

# .icns: header + one `ic08` (256x256 PNG) element
el <- c(charToRaw("ic08"), u32be(8 + length(png)), png)
icns <- c(charToRaw("icns"), u32be(8 + length(el)), el)
writeBin(icns, file.path(out, "tflplanner.icns"))
cat("wrote", out, "\n")
