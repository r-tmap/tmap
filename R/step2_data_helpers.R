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


step2_data_grp_prepare = function(tmf, dt) {
	### Specify 'by' variables
	if (tmf$is.wrap) {
		# facet wrap: only use by1
		by1 = tmf$by
		by2 = NULL
		by3 = NULL
		
		# By default, facets are created over the aes variables ("VARS__"). If wrap is specified in tm_facets, limit number of variables to 1.
		limitvars = (by1 != "VARS__")
		limitvars_warn = "Multiple variables have been specified in a layer function. However, since the 'by' argument of tm_facets_wrap has been specified, only the first variable is used"
	} else {
		# facet grid
		by1 = tmf$rows
		by2 = tmf$columns
		by3 = tmf$pages
		
		## Try to assign VARS__ to one dimension. If not possible, limit number of variables to 1.
		limitvars = FALSE
		if (!identical(by1, "VARS__") && !identical(by2, "VARS__") && !identical(by3, "VARS__")) {
			if (is.null(by1)) {
				by1 = "VARS__"
			} else if (is.null(by2)) {
				by2 = "VARS__"
			} else if (is.null(by3)) {
				by3 = "VARS__"
			} else {
				limitvars = TRUE
			}
		}
		limitvars_warn = "Multiple variables have been specified in a layer function. However, since the 'by' argument of tm_facets_wrap has been specified, only the first variable is used"
	}
	
	
	# v is variable by-dimension, b are the group-by by-dimensions
	v = which(c(by1, by2, by3) == "VARS__")
	b = setdiff(which(!vapply(list(by1, by2, by3), is.null, FUN.VALUE = logical(1))), v)
	
	#n = length(v) + length(b)
	
	by123 = paste0("by", 1L:3L) 
	by123__ = paste0("by", 1L:3L, "__")
	
	var__ = by123__[v]
	by__ = by123__[b]
	
	# create byx__ columns for which_by_spec
	if (length(b)) {
		for (w in b) {
			byvar = by123[w]
			byname = by123__[w]
			if (is.factor(dt[[get(byvar)]])) {
				if (tmf$drop.empty.facets) {
					dt[, (byname) := droplevels(get(get(..byvar)))] 
				} else {
					dt[, (byname) := get(get(..byvar))] 	
				}
			} else {
				dt[, (byname) := factor(get(get(..byvar)))]
			}
			update_fl(k = w, lev = levels(dt[[byname]]))
			dt[, (byname) := as.integer(get(..byname))]
		}
	}
	
	
	list(v = v, b = b, by123__ = by123__, var__ = var__, by__ = by__, limitvars = limitvars, limitvars_warn = limitvars_warn, drop.units = tmf$drop.units)
	
}

preprocess_meta_step2 = function(meta) {
	within(meta, {
		
		pc = list(sepia.intensity=sepia.intensity, saturation=saturation)
		sepia.intensity = NULL
		saturation = NULL
		
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
		
		if (is.list(inner.margins)) {
			inner.margins = lapply(inner.margins, rep, length.out = 4)
		}
		
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
	tmf
}

cbind_dts = function(dts) {
	if (!length(dts)) return(list())
	id = which.max(vapply(dts, ncol, FUN.VALUE = integer(1)))
	
	dt = dts[[id]]
	
	if (length(dts) > 1L) {
		for (i in setdiff(seq_along(dts), id)) {
			dti = dts[[i]]
			dt = dt[dti, on = names(dti)[1L:(ncol(dti)-1)]]
		}
	}
	dt
}
