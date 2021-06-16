library(pals)
library(rcartocolor)
library(grid)

tmap_pal_name_compress = function(x) tolower(gsub("[-, \\,, (, ), \\ ]",  "", x))


hcl = lapply(c("qualitative", "sequential", "diverging", "divergingx"), hcl.pals)
# merge diverging and divergingx
hcl[[3]] = c(hcl[[3]], hcl[[4]])
hcl[[4]] = NULL

hcl_labels = unlist(hcl)
hcl_names = tmap_pal_name_compress(hcl_labels)
hcl_n = rep(Inf, length(hcl_labels))
hcl_type = unlist(mapply(rep, c("cat", "seq", "div"), sapply(hcl, length), USE.NAMES = FALSE, SIMPLIFY = TRUE))
hcl_series = rep("hcl", length(hcl_labels))
hcl_package = rep("grDevices", length(hcl_labels))
hcl_fun = rep("hcl.colors", length(hcl_labels))
hcl_fun_type_arg = rep("palette", length(hcl_labels))

palette_hex = grDevices:::.palette_colors_hex
palette_labels = names(palette_hex)
palette_names = tmap_pal_name_compress(palette_labels)
palette_n = unname(sapply(palette_hex, length, USE.NAMES = FALSE))
palette_type = rep("cat", length(palette_hex))
palette_series = rep("palette", length(palette_hex))
palette_package = rep("grDevices", length(palette_hex))
palette_fun = rep("palette.colors", length(palette_hex))
palette_fun_type_arg = rep("palette", length(palette_hex))

pals_syspals = pals:::syspals
pals_syspals = pals_syspals[names(pals_syspals) %in% ls(asNamespace("pals"))]

pals_functions = ls(asNamespace("pals")) 

pals_names = names(pals_syspals)
pals_labels = pals_names
pals_n = unname(sapply(pals_names, function(b) {
	length(pals_syspals[[b]])
}))
pals_series = ifelse(substr(pals_names, 1, 6) == "brewer", "brewer",
			  ifelse(substr(pals_names, 1, 5) == "ocean", "ocean",
			  ifelse(substr(pals_names, 1, 6) == "kovesi", "kovesi",
			  ifelse(pals_names %in% c("magma", "inferno", "plasma", "viridis"), "viridis", "misc"))))
pals_type = ifelse(pals_names %in% c("alphabet", "alphabet2", "cols25", "glasbey", "kelly", "polychrome", "stepped", "stepped2", "stepped3", "okabe", "tableau20", "tol", "tol.groundcover", "watlington", 
									 "brewer.set1", "brewer.set2", "brewer.set3", "brewer.pastel1",
									 "brewer.pastel2", "brewer.dark2", "brewer.paired"), "cat",
				   ifelse(pals_names %in% c("coolwarm", "cubehelix", "brewer.brbg", "brewer.piyg", "brewer.prgn", "brewer.puor", "brewer.rdbu", "brewer.rdgy", "brewer.rdylbu", "brewer.rdylgn", "brewer.spectral", "ocean.balance", "ocean.delta", "ocean.curl", pals_names[substr(pals_names, 1, 16) == "kovesi.diverging"]), "div", ifelse(pals_names[substr(pals_names, 1, 13) == "kovesi.cyclic"], "cyc", "seq")))

pals_package = rep("pals", length(pals_names))
pals_fun = pals_names
pals_fun_type_arg = rep("", length(pals_names))

biv_names = c(
	"arc.bluepink",
	"brewer.qualbin",
	"brewer.divbin",
	"brewer.divseq",
	"brewer.qualseq",
	"brewer.divdiv",
	"brewer.seqseq1",
	"brewer.seqseq2",
	"census.blueyellow",
	"tolochko.redblue",
	"stevens.pinkgreen",
	"stevens.bluered",
	"stevens.pinkblue",
	"stevens.greenblue",
	"stevens.purplegold",
	"vsup.viridis",
	"vsup.redblue")
biv_n = c(9, 6, 6, rep(9, 12), 32, 32)
biv_labes = biv_names
biv_series = rep("bivariate", length(biv_names))
biv_package  = rep("pals", length(biv_names))
biv_fun = biv_names
biv_fun_type_arg = rep("", length(biv_names))
biv_type = rep("biv", lngth(biv_names))


carto_names = metacartocolors$Name
carto_labels = carto_names
carto_n = metacartocolors$Max_n
carto_series = rep("carto", length(carto_names))
carto_package = rep("rcartocolor", length(carto_names))
carto_fun = rep("carto_pal", length(carto_names))
carto_fun_type_arg = rep("name", length(carto_names))
carto_type = ifelse(metacartocolors$Type == "diverging", "div", ifelse(metacartocolors$Type == "qualitative", "cat", "seq"))




gn = function(x, name) get(paste(x, name, sep = "_"))

.tmap_pals = do.call(rbind, lapply(c("hcl", "palette", "pals", "carto"), function(x) {
	data.frame(name = gn(x, "names"),
			   label = gn(x, "labels"),
			   package = gn(x, "package"),
			   type = gn(x, "type"),
			   series = gn(x, "series"),
			   maxn = gn(x, "n"),
			   fun = gn(x, "fun"),
			   fun_type_args = gn(x, "fun_type_arg"))
}))


.tmap_pals$fullname = paste(.tmap_pals$series, .tmap_pals$name, sep = "__")


View(.tmap_pals)



tmap_get_palette = function(fullname, n) {
	id = match(fullname, .tmap_pals$fullname)
	if (is.na(id)) stop("cannot find palette ", fullname)
	
	x = as.list(.tmap_pals[id, ])
	fun = getExportedValue(x$package, x$fun)
	
	if (x$fun_type_args != "") {
		args = structure(list(x$name), names = x$fun_type_args)
	} else (
		args = list()
	)
	
	if (n > x$maxn) {
		n2 = x$maxn
	} else{
		n2 = n
	}
	
	args$n = n2
	
	pal = do.call(fun, args)
	if (n != n2) pal = colorRampPalette(pal)(n)
	pal
}
















# 
# 
# tmap.pal.info <- local({
# 	br <- RColorBrewer::brewer.pal.info[, 1:2]
# 	br$origin <- factor("brewer", levels = c("brewer", "viridis"))
# 	
# 	vr <- data.frame(maxcolors = rep.int(0, 5), category = factor("seq", levels = c("div", "qual", "seq")), origin = factor("viridis", levels = c("brewer", "viridis")), row.names = c("viridis", "magma", "plasma", "inferno", "cividis"))
# 	
# 	rbind(br, vr)
# })


get_default_contrast <- function(type, m) {
	if (type=="seq") {
		default_contrast_seq(m)
	} else {
		default_contrast_div(m)
	}
}


plot_tmap_pals_height_in = function(n, cex_line) {
	lines_per_inch = 0.2
	lines_per_inch * (1 + n * 1.33) * cex_line
}

plot_tmap_pals <- function(m, contrast=NULL, stretch=NULL, cex =.9, cex_line = .8, print.hex = FALSE, col.blind="normal") {
	#m=8 
	
	pal_nm = .tmap_pals$name
	pal_labels = .tmap_pals$label
	
	n <- length(pal_nm)
	
	
	cols = lapply(1:nrow(.tmap_pals), function(i) {
		tmap_get_palette(.tmap_pals$fullname[i], m)
	})
	
	
	
	grid.newpage()
	label_width <- convertWidth(stringWidth("Acbdefghijk"), "in", valueOnly = TRUE) * cex
	lH <- convertHeight(unit(1, "lines"), "in", valueOnly=TRUE) * cex_line
	
	vp <- viewport(layout = grid.layout(nrow = 2*n + 1, ncol=m+2,
										widths = unit(c(label_width, rep(1, m), label_width/4), c("in", rep("null", m), "in")),
										heights = unit(c(lH, rep(c(lH, .33*lH), length.out=2*n+1), 1), c(rep("in", 2*n), "null"))))
	pushViewport(vp)
	
	lapply(1L:m, function(j) {
		pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1+j))
		grid.text(j, gp=gpar(cex=cex))
		popViewport()
	})
	
	lapply(1L:n, function(i) {
		pushViewport(viewport(layout.pos.row = i*2, layout.pos.col = 1))
		#if (cb[i]) grid.rect(gp=gpar(col=NA, fill="yellow"))
		grid.text(pal_labels[i], x = .1, just = "left", gp=gpar(cex=cex, col="#999999"))
		#if (!cb[i]) grid.lines(x=c(.1,.9), y=c(.5, .5), gp=gpar(col="#AAAAAA"))
		popViewport()
		
		pal = cols[[i]]
		ids = numeric(0)
		
		# if (origin == "brewer") {
		# 	if (type=="qual") {
		# 		pal <- get_brewer_pal(pal_nm[i], n=m, stretch=stretch, plot=FALSE)
		# 		ids <- which(pal==pal[1])[-1]
		# 	} else {
		# 		pal <- get_brewer_pal(pal_nm[i], n=m, contrast=contrast, plot=FALSE)
		# 		ids <- numeric(0)
		# 	}
		# } else {
		# 	pal <- viridis(m, option = pal_nm[i], begin = contrast[1], end = contrast[2])
		# 	ids <- numeric(0)
		# }
		
		if (col.blind != "normal") pal <- dichromat::dichromat(pal, type=col.blind)
		
		
		fontcol <- ifelse(is_light(pal), "black", "white")
		fontwidth <- convertWidth(stringWidth("#FFFFFFAA"), unitTo = "npc", valueOnly = TRUE)
		fontsize <- min(cex, (1/fontwidth) / m)
		
		
		lapply(1L:m, function(j) {
			pushViewport(viewport(layout.pos.row = i*2, layout.pos.col = 1+j))
			grid.rect(gp=gpar(fill=pal[j]))
			if (print.hex) grid.text(pal[j], gp=gpar(cex=fontsize, col=fontcol[j]))
			if (j %in% ids) {
				grid.circle(x=0, y=.5, r=.25, gp=gpar(fill="white", lwd=1))
			}
			popViewport()
		})
	})
	popViewport()
}

is_light <- function(col) {
	colrgb <- col2rgb(col)
	apply(colrgb * c(.299, .587, .114), MARGIN=2, sum) >= 128
}

n = nrow(.tmap_pals)

pfile = tempfile(fileext = ".png")
dpi = 72
dz = dev.size()

png(pfile, height = plot_tmap_pals_height_in(n, cex_line = 0.8) * dpi, width = dz[1] * dpi, res = dpi)
plot_tmap_pals(m = 15)
dev.off()

temp <- tempfile(fileext = ".html")
writeLines(as.character(htmltools::img(src=knitr::image_uri(pfile))), temp)
getOption("viewer")(temp)


