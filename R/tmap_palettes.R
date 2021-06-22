tmapPalId = function(name, check_rev = TRUE) {
	if (check_rev) {
		isrev = (substr(name, 1, 1) == "-")
		if (isrev) name = substr(name, 2, nchar(name))
	}
	
	id = match(name, .tmap_pals$fullname)
	if (is.na(id)) {
		id = match(tmap_pal_name_compress(name), .tmap_pals$name)
	}
	#if (is.na(id)) stop("Palette \"", name, "\" could not be found")
	id
}

tmap_get_palette = function(name, n = NA) {
	if (name %in% unique(.tmap_pals$type)) {
		name = tmap_options()$aes.palette[[name]]
	} 
	
	
	
	isrev = (substr(name, 1, 1) == "-")
	if (isrev) name = substr(name, 2, nchar(name))
	
	id =  tmapPalId(name, check_rev = FALSE)
	
	x = as.list(.tmap_pals[id, ])
	fun = getExportedValue(x$package, x$fun)
	
	if (x$fun_type_args != "") {
		args = structure(list(x$name), names = x$fun_type_args)
	} else (
		args = list()
	)
	
	if (is.na(n)) {
		n = if (is.infinite(x$maxn)) 9 else x$maxn
	}
	
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
	
	if (isrev) pal = rev(pal)

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

tmap_pal_name_compress = function(x) tolower(gsub("[-, \\,, (, ), \\ ]",  "", x))

tmap_pals = local({
	fnm = split(.tmap_pals$label, list(.tmap_pals$package, .tmap_pals$series)) 
	nm = split(.tmap_pals$name, list(.tmap_pals$package, .tmap_pals$series)) 
	
	fnm = fnm[vapply(fnm, FUN = function(x) length(x) > 0, FUN.VALUE = logical(1))]
	nm = nm[vapply(nm, FUN = function(x) length(x) > 0, FUN.VALUE = logical(1))]
	
	mapply(function(fnmi, nmi) {
		names(fnmi) = nmi
		as.list(fnmi)
	}, fnm, nm)
	
})



tmapGetPalette = function(name, n) {
	isrev = (substr(name, 1, 1) == "-")
	if (isrev) name = substr(name, 2, nchar(name))
	
	id = match(name, .tmap_pals$fullname)
	if (is.na(id)) {
		id = match(tmap_pal_name_compress(name), .tmap_pals$name)
	}
	if (is.na(id)) stop("Palette \"", name, "\" could not be found")
	pal = tmap_get_palette(.tmap_pals$fullname[id], n)
	if (isrev) pal = rev(pal)
	pal
}


tmap_show_palettes = function(type = c("cat", "seq", "div", "cyc", "biv"),
							  series = c("palette", "hcl", "brewer", "viridis", "kovesi", "ocean", "carto", "bivariate", "misc"),
							  n = 9,
							  color_vision_deficiency = c("none", "deutan", "protan", "tritan")) {
	pals = .tmap_pals[.tmap_pals$type %in% type & .tmap_pals$series %in% series, ]
	
	
	color_vision_deficiency = match.arg(color_vision_deficiency)
	
	pals$typename = c("Categorical", "Sequential", "Diverging", "Cyclic", "Bivariate")[match(pals$type, c("cat", "seq", "div", "cyc", "biv"))]
	
	inch_per_line = 0.2
	inch_per_word = 2
	
	dz = dev.size()
	
	table(pals$type, pals$series)
	
	types = intersect(type, pals$type)
	
	pals$lines_per_pal = ifelse(is.infinite(pals$maxn), 1, 
					ifelse(pals$type == "biv", floor(sqrt(pals$maxn)), ceiling(pals$maxn / n)))
	
	
	
	ppt = lapply(types, function(tp) {
		pals[pals$type == tp, ]	
	})
	
	
	sim_colors = if (color_vision_deficiency == "deutan") {
		colorspace::deutan
	} else if (color_vision_deficiency == "protan") {
		colorspace::protan
	} else if (color_vision_deficiency == "tritan") {
		colorspace::tritan
	} else {
		function(x) x
	}
	
	hs = lapply(ppt, function(p) {
		list(rowin  = c(0.5, 1.5, 0.5, 1, unlist(lapply(p$lines_per_pal, function(l) c(l, 0.5))), 0.5),
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
	
	colin = c(.25* inch_per_word, 0.3 * inch_per_word, 1.3 * inch_per_word, rep((dz[1] - 2.1 * inch_per_word) / n, n),.25 * inch_per_word)
	ncols = length(colin)
	
	pfile = tempfile(fileext = ".png")
	dpi = 92
	dz = dev.size()
	
	png(pfile, height = sum(rowin) * dpi, width = dz[1] * dpi, res = dpi)
	grid::grid.newpage()
	#grid.rect(gp=grid::gpar(fill="yellow"))
	vp = grid::viewport(layout = grid::grid.layout(nrow = nrows, ncol = ncols,
										widths = grid::unit(colin, "in"),
										heights = grid::unit(rowin, "in")))
	grid::pushViewport(vp)
	
	mapply(function(p, ids, idnum, idtype) {
		 # p = ppt[[2]]
		 # ids = rowids[[2]]
		 # idnum = rowid_idnum[2]
		 # idtype = rowid_type[2]

		grid::pushViewport(grid::viewport(layout.pos.row = idtype, layout.pos.col = 3:(n + 2)))
		#grid.rect(gp=gpar(fill="yellow"))
		grid::grid.text(p$typename[1], gp=grid::gpar(cex = 1.5, col = "grey30"))
		grid::upViewport()
		for (i in 1:n) {
			grid::pushViewport(grid::viewport(layout.pos.row = idnum, layout.pos.col = i + 3))
			grid::grid.text(i, gp=grid::gpar(cex = 0.75, col = "grey50"))
			grid::upViewport()
		}
		for (k in 1:nrow(p)) {
			cols = sim_colors(tmap_get_palette(p$fullname[k], n = ifelse(is.infinite(p$maxn[k]), n, p$maxn[k])))
			
			if (p$type[k] == "biv") {
				nk = p$maxn[k] / p$lines_per_pal[k]
			} else {
				if (length(cols) %% n != 0) cols = c(cols, rep(NA, (n - (length(cols) %% n))))
				nk = n
			}
			
			cm = matrix(cols, ncol = nk, byrow = TRUE)
			
			grid::pushViewport(grid::viewport(layout.pos.row = ids[k], layout.pos.col = 2))
			grid::grid.text(p$series[k], gp=grid::gpar(cex = 1, col = "grey30"), just = "right", x = 0.95)
			grid::upViewport()
			
			grid::pushViewport(grid::viewport(layout.pos.row = ids[k], layout.pos.col = 3))
			grid::grid.text(p$label[k], gp=grid::gpar(cex = 1, col = "grey30"), just = "right", x = 0.95)
			grid::upViewport()
			
			
			for (i in 1:nk) {
				grid::pushViewport(grid::viewport(layout.pos.row = ids[k], layout.pos.col = i + 3))
				#vp2 = viewport(layout = grid.layout(nrow = nrow(cm), ncol = 1))
				#pushViewport(vp2)
				
				h = 1 / nrow(cm)
				y = rev(seq(0, 1, length.out = nrow(cm) * 2 + 1)[seq(2, by = 2, length.out = nrow(cm))])
				
				coli = ifelse(is.na(cm[, i]), NA, "grey70")
				
				grid::grid.rect(y = y, height = h, gp = grid::gpar(col = coli, fill = cm[, i]))
				grid::upViewport()
			}
			
		}
		
	}, ppt, rowids, rowid_idnum, rowid_type, SIMPLIFY = FALSE)
	
	
	dev.off()
	
	temp <- tempfile(fileext = ".html")
	writeLines(as.character(htmltools::img(src=knitr::image_uri(pfile))), temp)
	getOption("viewer")(temp)
	
}
