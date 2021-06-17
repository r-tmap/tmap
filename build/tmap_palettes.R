tmap_get_palette = function(fullname, n = NA) {
	id = match(fullname, .tmap_pals$fullname)
	if (is.na(id)) stop("cannot find palette ", fullname)
	
	x = as.list(.tmap_pals[id, ])
	fun = getExportedValue(x$package, x$fun)
	
	if (x$fun_type_args != "") {
		args = structure(list(x$name), names = x$fun_type_args)
	} else (
		args = list()
	)
	
	if (is.na(n)) n = x$maxn
	
	if (n > x$maxn) {
		n2 = x$maxn
	} else{
		n2 = n
	}
	
	args$n = n2
	
	pal = do.call(fun, args)
	if (n != n2) {
		if (x$type == "cat") {
			pal = rep(pal, length.out = n)
		} else pal = colorRampPalette(pal)(n)	
	} 
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

tmap_show_palettes = function(type = c("cat", "seq", "div", "cyc", "biv"),
							  series = c("palette", "hcl", "brewer", "viridis", "kovesi", "ocean", "carto", "bivariate", "misc"),
							  
							  n = 9) {
	pals = .tmap_pals[.tmap_pals$type %in% type & .tmap_pals$series %in% series, ]
	
	
	pals$typename = c("Categorical", "Sequential", "Diverging", "Cyclic", "Bivariate")[match(pals$type, c("cat", "seq", "div", "cyc", "biv"))]
	
	inch_per_line = 0.2
	inch_per_word = 0.85
	
	dz = dev.size()
	
	table(pals$type, pals$series)
	
	types = intersect(type, pals$type)
	
	pals$lines_per_pal = ifelse(is.infinite(pals$maxn), 1, 
					ifelse(pals$type == "biv", floor(sqrt(pals$maxn)), ceiling(pals$maxn / n)))
	
	
	
	ppt = lapply(types, function(tp) {
		pals[pals$type == tp, ]	
	})
	
	
	hs = lapply(ppt, function(p) {
		list(rowin  = c(0.5, 1.5, 0.5, 1, unlist(lapply(p$lines_per_pal, function(l) c(l, 0.2))), 0.5),
			 rowids = seq(5, by = 2, length.out = nrow(p)),
			 row_idnum = 4,
			 row_type = 2)
		# space, type name, space, indices, {pal, space}, space
	})
	
	rowid_start = c(0, head(cumsum(sapply(hs, function(h) {
		length(h$rowin)
	})), -1))

	rowid_idnum = mapply(function(h, s) {
		h$row_idnum + s
	}, hs, rowid_start, SIMPLIFY = TRUE)
	
	rowid_type = mapply(function(h, s) {
		h$row_type + s
	}, hs, rowid_start, SIMPLIFY = TRUE)
		
	rowids = mapply(function(h, s) {
		h$rowids + s
	}, hs, rowid_start, SIMPLIFY = FALSE)
	
	
	
	
	
	rowin = unlist(lapply(hs, function(h) h$rowin)) * inch_per_line
	nrows = length(rowin)
	
	colin = c(.5* inch_per_word, inch_per_word, inch_per_word, rep(dz[2] - 3 * inch_per_word, n),.5 * inch_per_word)
	ncols = length(colin)
	
	pfile = tempfile(fileext = ".png")
	dpi = 72
	dz = dev.size()
	
	png(pfile, height = sum(rowin) * dpi, width = dz[1] * dpi, res = dpi)
	grid.newpage()
	
	vp = viewport(layout = grid.layout(nrow = nrows, ncol = ncols))
										#widths = unit(colin, "in"),
										#heights = unit(rowin, "in")))
	pushViewport(vp)
	
	mapply(function(p, ids, idnum, idtype) {
		 # p = ppt[[2]]
		 # ids = rowids[[2]]
		 # idnum = rowid_idnum[2]
		 # idtype = rowid_type[2]

		pushViewport(viewport(layout.pos.row = idtype, layout.pos.col = 3:(n + 2)))
		#grid.rect(gp=gpar(fill="yellow"))
		grid.text(p$typename[1], gp=gpar(cex = 1.5, col = "grey30"))
		upViewport()
		for (i in 1:n) {
			pushViewport(viewport(layout.pos.row = idnum, layout.pos.col = i + 2))
			grid.text(i, gp=gpar(cex = 0.75, col = "grey50"))
			upViewport()
		}
		for (k in 1:nrow(p)) {
			cols = tmap_get_palette(p$fullname[k], n = ifelse(is.infinite(p$maxn[k]), n, p$maxn[k]))
			
			if (p$type[k] == "biv") {
				nk = p$maxn[k] / p$lines_per_pal[k]
			} else {
				if (length(cols) %% n != 0) cols = c(cols, rep(NA, (n - (length(cols) %% n))))
				nk = 9
			}
			
			cm = matrix(cols, ncol = nk, byrow = TRUE)
			
			for (i in 1:nk) {
				pushViewport(viewport(layout.pos.row = ids[k], layout.pos.col = i + 2))
				#vp2 = viewport(layout = grid.layout(nrow = nrow(cm), ncol = 1))
				#pushViewport(vp2)
				
				h = 1 / nrow(cm)
				y = seq(0, 1, length.out = nrow(cm) * 2 + 1)[seq(2, by = 2, length.out = nrow(cm))]
				
				coli = ifelse(is.na(cm[, i]), NA, "grey70")
				
				grid.rect(y = y, height = h, gp = gpar(col = coli, fill = cm[, i]))
				upViewport()
			}
			
		}
		
	}, ppt, rowids, rowid_idnum, rowid_type, SIMPLIFY = FALSE)
	
	
	dev.off()
	
	temp <- tempfile(fileext = ".html")
	writeLines(as.character(htmltools::img(src=knitr::image_uri(pfile))), temp)
	getOption("viewer")(temp)
	
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

png(pfile, height = plot_tmap_pals_height_in(n, cex_line = 2) * dpi, width = dz[1] * dpi, res = dpi)
plot_tmap_pals(m = 15, cex_line = 2)
dev.off()

temp <- tempfile(fileext = ".html")
writeLines(as.character(htmltools::img(src=knitr::image_uri(pfile))), temp)
getOption("viewer")(temp)


