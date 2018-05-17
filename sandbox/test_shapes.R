library(grid)

data(NLD_prov)


###################### test of symbol shapes (matched by variable value) ###################################

# create pngs (in random order)
files <- tempfile(letters[1:12], fileext = ".png")
names(files) <- sample(NLD_prov$name)
for (i in 1:length(files)) {
	png(files[i], width = 64, height=64)
	grid.text(names(files)[i])
	dev.off()
}

# create grobs (in the same order as the pngs)
grobs <- lapply(names(files), function(name) {
	textGrob(name)
})
names(grobs) <- names(files)

# create vector of symbol numbers (in the same order as the pngs)
syms <- 1:12
names(syms) <- names(files)


# draw symbols (joined by name variable)
tm_shape(NLD_prov) +
	tm_symbols(shape="name", shapes = syms)

# draw pngs (joined by name variable)
tm_shape(NLD_prov) +
	tm_symbols(shape="name", shapes = tmap_icons(files))

# use grobs (joined by name variable)
tm_shape(NLD_prov) +
	tm_symbols(shape="name", shapes = grobs)


###################### small multiples ###################################

## generate a second set of symbols (png, grob and symbol numbers)

# create pngs (in random order)
files2 <- tempfile(LETTERS[1:12], fileext = ".png")
names(files2) <- sample(NLD_prov$name)
for (i in 1:length(files2)) {
	png(files2[i], width = 64, height=64)
	grid.text(names(files2)[i], gp=gpar(col="blue"))
	dev.off()
}

# create grobs (in the same order as the pngs)
grobs2 <- lapply(names(files2), function(name) {
	textGrob(name, gp=gpar(col="blue"))
})
names(grobs2) <- names(files2)

# create vector of symbol numbers (in the same order as the pngs)
syms2 <- 13:24
names(syms2) <- names(files2)

tm_shape(NLD_prov) +
	tm_symbols(shape=c("name", "name"), shapes = list(grobs, grobs2))

tm_shape(NLD_prov) +
	tm_symbols(shape=c("name", "name"), shapes = list(tmap_icons(files), tmap_icons(files2)))

tm_shape(NLD_prov) +
	tm_symbols(shape=c("name", "name"), shapes = list(syms, syms2))

	
