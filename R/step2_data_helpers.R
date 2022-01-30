# function to update the levels fl per facet dimension
update_fl = function(k, lev = NULL, m = NULL) {
	fl = get("fl", envir = .TMAP)
	# print("test")
	# print(fl)
	fk = fl[[k]]
	n =  if (is.character(fk)) length(fk) else fk
	
	if (!missing(lev)) {
		m = length(lev)
		if (m == 1L && n == 1L) {
			fk = lev
		} else if (m > 1L && n > 1L && m != n) {
			stop("Inconsistent number of facets in the ", k, " dimension.", call. = FALSE)
		} else if (m > n) {
			fk = lev
		}
	} else if (!missing(m)) {
		if (m > 1L && n > 1L && m != n) {
			stop("Inconsistent number of facets in the ", k, " dimension.", call. = FALSE)
		} else if (m > n) {
			fk = m
		}
	}
	fl[[k]] = fk
	assign("fl", fl, envir = .TMAP)
}



preprocess_meta_step2 = function(o) {
	within(o, {
		
		pc = list(sepia.intensity=color.sepia.intensity, saturation=color.saturation, deficiency.sim=color.deficiency.sim)
		color.sepia.intensity = NULL
		color.saturation = NULL
		color.deficiency.sim = NULL
		
		title.size = title.size * scale
		legend.title.size = legend.title.size * scale
		legend.text.size = legend.text.size * scale
		
		panel.label.size = panel.label.size * scale
		
		space.color = ifelse(is.null(space.color), bg.color, space.color[1])
		earth.boundary.color = ifelse(is.null(earth.boundary.color), attr.color, earth.boundary.color[1])
		legend.text.color =  ifelse(is.null(legend.text.color), attr.color, legend.text.color[1])
		legend.title.color = ifelse(is.null(legend.title.color), attr.color, legend.title.color[1])
		title.color = ifelse(is.null(title.color), attr.color, title.color[1])
		
		legend.inside.box = if (!is.logical(legend.frame)) TRUE else legend.frame
		if (identical(title.bg.color, TRUE)) title.bg.color = bg.color
		
		if (identical(frame, TRUE)) frame = attr.color else if (identical(frame, FALSE)) frame = NA 
		
		if (is.logical(legend.frame)) if (identical(legend.frame, TRUE)) legend.frame = attr.color else legend.frame = NA 
		# 		
		# 		between.margin.in <- convertHeight(unit(between.margin, "lines") * scale, "inch", valueOnly=TRUE)
		# 		
		# 		between.margin.y <-convertHeight(unit(between.margin.in, "inch"), "npc", valueOnly=TRUE) * gf$nrow
		# 		between.margin.x <-convertWidth(unit(between.margin.in, "inch"), "npc", valueOnly=TRUE) * gf$ncol
		# 		
		
		outer.margins <- rep(outer.margins, length.out = 4)

		inner.margins.extra = rep(inner.margins.extra, length.out = 4)
		
		if (is.list(inner.margins)) {
			inner.margins = lapply(inner.margins, function(im) {
				rep(im, length.out = 4) + inner.margins.extra
			})
		} else {
			rep(inner.margins, length.out = 4) + inner.margins.extra
		}
		inner.margins.extra = NULL
		
		attr.color.light = is_light(attr.color)
		
		title.color = do.call("process_color", c(list(col=title.color), pc))
		main.title.color = do.call("process_color", c(list(col=main.title.color), pc))
		legend.text.color = do.call("process_color", c(list(col=legend.text.color), pc))
		legend.title.color = do.call("process_color", c(list(col=legend.title.color), pc))
		if (!is.na(frame)) frame = do.call("process_color", c(list(col=frame), pc))
		if (!is.na(legend.frame)) legend.frame = do.call("process_color", c(list(col=legend.frame), pc))
		
		panel.label.color = do.call("process_color", c(list(col=panel.label.color), pc))
		panel.label.bg.color = do.call("process_color", c(list(col=panel.label.bg.color), pc))
		
		earth.boundary.color = do.call("process_color", c(list(col=earth.boundary.color), pc))
		
		bg.color = do.call("process_color", c(list(col=bg.color), pc))
		
		if (!is.null(outer.bg.color)) outer.bg.color = do.call("process_color", c(list(col=outer.bg.color), pc))
		
		if (is.na(legend.bg.color)) legend.bg.color = !is.na(legend.frame)
		if (!is.na(legend.bg.color)) {
			legend.bg.color = if (identical(legend.bg.color, FALSE)) {
				NA
			} else if (identical(legend.bg.color, TRUE)) {
				bg.color
			} else {
				do.call("process_color", c(list(col=legend.bg.color, alpha=legend.bg.alpha), pc))				}
		} 
		if (!is.na(title.bg.color)) title.bg.color = do.call("process_color", c(list(col=title.bg.color, alpha=title.bg.alpha), pc))
		if (!is.na(earth.boundary.color)) earth.boundary.color = do.call("process_color", c(list(col=earth.boundary.color), pc))
		space.color = do.call("process_color", c(list(col=space.color), pc))
		
		earth.bounds = if (is.logical(earth.boundary)) {
			c(-180, -90, 180, 90)
		} else {
			as.vector(bb(earth.boundary))
		}
		earth.boundary = !identical(earth.boundary, FALSE)
		
		earth.boundary.lwd = earth.boundary.lwd * scale
		frame.lwd = frame.lwd * scale
		
		# set font face and family
		
		if (is.null(legend.title.fontface)) legend.title.fontface = fontface
		if (is.null(legend.title.fontfamily)) legend.title.fontfamily = fontfamily
		
		if (is.null(legend.text.fontface)) legend.text.fontface = fontface
		if (is.null(legend.text.fontfamily)) legend.text.fontfamily = fontfamily
		
		if (is.null(title.fontface)) title.fontface = fontface
		if (is.null(title.fontfamily)) title.fontfamily = fontfamily
		
		if (is.null(main.title.fontface)) main.title.fontface = fontface
		if (is.null(main.title.fontfamily)) main.title.fontfamily = fontfamily
		
		if (is.null(panel.label.fontface)) panel.label.fontface = fontface
		if (is.null(panel.label.fontfamily)) panel.label.fontfamily = fontfamily
		
		
	})
}


get_tmf = function(tmfs) {
	# Get tmf object: start with the one that is called, and add 'calls'
	
	nf = length(tmfs)
	
	gns = vapply(tmfs, "[[", FUN.VALUE = integer(3), "gn", USE.NAMES = FALSE)
	
	grpid = apply(gns, which.max, MARGIN = 1)
	
	gn = apply(gns, max, MARGIN = 1)
	
	for (i in seq_len(3L)) if (any(gns[i, ] > 1L & gns[i, ] != gn[i])) stop("Number of facets inconsistent over groups", call. = FALSE)
	
	gls = lapply(tmfs, "[[", "gl")
	
	gl = lapply(seq_len(3L), function(i) gls[[grpid[i]]][[i]])
	
	
	#gnl = lapply(tmfs, "[[", FUN.VALUE = integer(3), "gn", USE.NAMES = FALSE)
	
	# find first tmf that has been called
	fid = which(vapply(tmfs, function(tmf){
		"calls" %in% names(tmf)
	}, FUN.VALUE = logical(1)))[1]
	
	if (is.na(fid)) fid = 1L
	
	tmf = tmfs[[fid]]
	
	if (fid < nf) {
		for (i in (fid+1):nf) {
			args = tmfs[[i]]$calls
			tmf[args] = tmfs[[i]][args]
		}
	}
	tmf$fl = gl
	tmf$fn = gn
	tmf$n = prod(gn)
	
	if (tmf$type == "wrapstack") {
		tmf$type = if (tmf$n > 3) "wrap" else "stack"
	}
	
	tmf
}

cbind_dts = function(dts, plot.order) {
	if (!length(dts)) return(list())
	
	bypass_ord = plot.order$aes == "NULL"
	
	
	id = which.max(vapply(dts, ncol, FUN.VALUE = integer(1))) # data.table with the most group-by columns (others are joined)
	
	dt = dts[[id]]
	
	dev = getOption("tmap.devel.mode")

	if (length(dts) > 1L) {
		for (i in setdiff(seq_along(dts), id)) {
			dti = dts[[i]]
			
			id_cols = ncol(dti) - 1L - as.integer(!bypass_ord) #minus one aes and one ord
			id_nams = names(dti)[seq.int(id_cols)]
			
			#dt = dt[dti, on = names(dti)[1L:(ncol(dti)-2L)]]
			dt = dt[dti, on = id_nams]
		}
	}
	

	
	ord_cols = which(subStr(names(dt), -5) == "__ord")
	#m = as.matrix(dt[, ord_cols, with = FALSE])
	
	
	#dt[, ord_cols, with = FALSE][, ]
	
	if (!bypass_ord) {
		dt_rep = function(old, new) {
			dt[, (ord_cols) := replace(.SD, .SD == old, new), .SDcols = ord_cols]
		}
		

		if (plot.order$na.order == "mix") dt_rep(0L, 1L)
		if (plot.order$null.order == "mix") dt_rep(-1L, 1L)
		
		if (!plot.order$null.below.na) {
			if (plot.order$na.order == "top") dt_rep(0L, 2147483646L) else dt_rep(0L, -2L)
			if (plot.order$null.order == "top") dt_rep(-1L, 2147483647L)
		} else {
			if (plot.order$na.order == "top") dt_rep(0L, 2147483647L)
			if (plot.order$null.order == "top") dt_rep(-1L, 2147483646L)
		}

		dt[, ord__ := do.call(pmin, .SD), .SDcols = ord_cols]
		dt[ord__ > 0L, ord__ := do.call(pmax, .SD), .SDcols = ord_cols]
		dt[, (ord_cols) := NULL]
	}

	
	dt
}


subStr = function(x, k) {
	if (k > 0L) {
		substr(x, 1L, k)	
	} else {
		n = nchar(x)
		substr(x, n + k + 1L, n)	
	}
}

