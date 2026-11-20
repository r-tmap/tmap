# Build the tmap cheatsheet PDF from tmap_cheatsheet.html with headless Chrome
# Run from the package root: Rscript cheatsheet/build.R
# Output:
#   pkgdown/assets/tmap_cheatsheet.pdf (served at https://r-tmap.github.io/tmap/tmap_cheatsheet.pdf)
#   man/figures/cheatsheet.png (preview of page 1, used in the README)
chrome = Sys.getenv("CHROME", "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
html = normalizePath("cheatsheet/tmap_cheatsheet.html")
dir.create("pkgdown/assets", showWarnings = FALSE, recursive = TRUE)
pdf = file.path(normalizePath("pkgdown/assets"), "tmap_cheatsheet.pdf")
system2(chrome, c("--headless", "--disable-gpu", "--no-pdf-header-footer",
	paste0("--print-to-pdf=", shQuote(pdf)), shQuote(paste0("file://", html))))

preview = magick::image_read_pdf(pdf, pages = 1, density = 100)
preview = magick::image_border(magick::image_scale(preview, "800"), "gray80", "1x1")
magick::image_write(preview, "man/figures/cheatsheet.png")
