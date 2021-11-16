preprocess_meta = function(o, legs) {
	within(o, {
		nby = get_nby(fl)
		isdef = sapply(fl, is.character)
		n = prod(nby)
		if (is.na(panel.type)) panel.type = ifelse((n == 1 && is.na(panel.labels[[1]]) && !isdef[1]) || (is.wrap && !isdef[1]) || (!is.wrap && !isdef[1] && !isdef[2]), "none", 
										    ifelse(is.wrap || (n == 1), "wrap", "xtab"))
		
		inner.margins = get_option_class(inner.margins, class = main_class)

		# legend.present.auto:
		#   find out whether there are legends for all facets, per row, per col
		#   use them to automatically determine meta.margins (in preprocess_meta)
		# # legend.present.fix
		#	find legend boxes that are assigned to outer margins
		if (nrow(legs) == 0) {
			legend.present.auto = c(all = FALSE, per_row = FALSE, per_col = FALSE)
			legend.present.fix = rep(FALSE, 4)
		} else {
			if (is.wrap) {
				#o$legend.present.auto = c(all = any(is.na(legs$by1__) & legs$class == "auto"), per_row = any(!is.na(legs$by1__) & legs$class == "auto"), per_col = FALSE)
				legend.present.auto = c(all = any(legs$class == "auto"), per_row = FALSE, per_col = FALSE)
			} else {
				legend.present.auto = c(all = any(is.na(legs$by1__) & is.na(legs$by2__) & legs$class == "auto"), per_row = any(!is.na(legs$by1__) & is.na(legs$by2__) & legs$class == "auto"), per_col = any(is.na(legs$by1__) & !is.na(legs$by2__) & legs$class == "auto"))
			}
			legend.present.fix = c(any(legs$class == "out" & legs$v == "bottom"), 
								   any(legs$class == "out" & legs$h == "left"),
								   any(legs$class == "out" & legs$v == "top"),
								   any(legs$class == "out" & legs$h == "right"))
		}
	})
}

process_meta = function(o, d) {
	within(o, {
		# sasp shape aspect ratio (NA if free coordinates)
		diff_asp = any(d$asp != d$asp[1])
		sasp = ifelse(diff_asp, NA, d$asp[1])
		
		# dasp device aspect ratio
		devsize = dev.size()
		dasp = devsize[1] / devsize[2]
		
		# needed for spnc viewport (to retain aspect ratio)
		if (dasp > 1) {
			cw = dasp
			ch = 1
		} else {
			ch = 1/dasp
			cw = 1
		}
		
		lineH = grid::convertHeight(grid::unit(1, "lines"), unitTo = "npc", valueOnly = TRUE) * scale
		lineW = grid::convertWidth(grid::unit(1, "lines"), unitTo = "npc", valueOnly = TRUE) * scale
		
		bufferH = lineH / 2
		bufferW = lineW / 2
		
		# calculate space for margins, panels, etc
		
		meta.automatic = is.na(meta.margins[1])
		
		#one.row = (!is.na(o$nrows) && o$nrows == 1)
		#one.col = (!is.na(o$ncols) && o$ncols == 1)
		
		if (meta.automatic) meta.margins = c(0, 0, 0, 0) else meta.margins = rep(meta.margins, length.out = 4)
		
		meta.buffers = sign(meta.margins) * c(bufferH, bufferW, bufferH, bufferW) # outside and inside
		
		panel.xtab.size = if (panel.type == "xtab") {
			c(ifelse("bottom" %in% panel.xtab.pos, panel.label.height * lineH, 0),
			  ifelse("left" %in% panel.xtab.pos, panel.label.height * lineW, 0),
			  ifelse("top" %in% panel.xtab.pos, panel.label.height * lineH, 0),
			  ifelse("right" %in% panel.xtab.pos, panel.label.height * lineW, 0))
		} else c(0, 0, 0, 0)
		
		panel.wrap.size = if (panel.type == "wrap") {
			c(ifelse(panel.wrap.pos == "bottom", panel.label.height * lineH, 0),
			  ifelse(panel.wrap.pos == "left", panel.label.height * lineW, 0),
			  ifelse(panel.wrap.pos == "top", panel.label.height * lineH, 0),
			  ifelse(panel.wrap.pos == "right", panel.label.height * lineW, 0))
		} else c(0, 0, 0, 0)
		
		xylab.margins = rep(0, 4)
		if (xlab.show) xylab.margins[ifelse(xlab.pos == "bottom", 1, 3)] = xylab.height * lineH
		if (ylab.show) xylab.margins[ifelse(xlab.pos == "left", 2, 4)] = xylab.height * lineW
		
		
		grid.buffers = if (grid.show) {
			as.integer(grid.label.pos == c("bottom", "left", "top", "right")) * c(bufferH, bufferW, bufferH, bufferW)
		} else {
			rep(0, 4)
		}
		
		grid.margins = if (grid.show) {
			as.integer(grid.label.pos == c("bottom", "left", "top", "right")) * grid.mark.height * c(lineH, lineW, lineH, lineW)
		} else {
			rep(0, 4)
		}
		between.marginH = between.margin * lineH
		between.marginW = between.margin * lineW
		
		fixedMargins  =  outer.margins + meta.buffers * 2 + meta.margins + xylab.margins + panel.xtab.size + grid.buffers + grid.margins
		
		
		
		# preferred aspect ratio (just for this function): if asp is defined (not 0 or NA), use that, otherwise use sasp (shape asp) if available (if not; 1)
		pasp = if (is.na(sasp)) {
			if (!is.na(asp) && asp > 0) {
				asp
			} else {
				1
			}
		} else {
			if (!is.na(asp) && asp > 0) {
				asp
			} else {
				sasp
			}
		}
		
		masp = ((1 - sum(fixedMargins[c(2, 4)])) / (1 - sum(fixedMargins[c(1, 3)]))) * dasp
		
		# Aspect ratios:
		# sasp: shape
		# asp: user specified
		# pasp: prefered (asp or if not specified, sasp)
		# masp: multiples (facets) area
		
		# determine where to place automatic legends (i.e. legends with local legend.position = NA and with legend.position = tm_lp_auto() enabled)
		# this is also neede to find out which margins are taken from meta.auto.margins
		
		legend.position.sides = legend.position
		legend.position.all = legend.position
		
		## find position for all-facet legend
		
		if (legend.present.auto[1]) {
			if (!legend.present.auto[2] & !legend.present.auto[3]) {
				# only 'all facets' legends (either bottom or right)
				if ((n == 1 && pasp > masp) || (n > 1 && masp > pasp) || (identical(nrows, 1) || (!is.na(ncols) && ncols >= n))) { # || one.row
					legend.position.all = list(h = "center", v = legend.position$v)
				} else {
					legend.position.all = list(h = legend.position$h, v = "center")
				}
			} else if (legend.present.auto[2] & !legend.present.auto[3]) {
				# central goes center bottom 
				legend.position.all = list(h = "center", v = legend.position$v)
			} else if (!legend.present.auto[2] & legend.present.auto[3]) {
				# central goes right center 
				legend.position.all = list(h = legend.position$h, v = "center")
			}
		}
		
		margins.used.all = c(legend.position.all$v == "bottom",
							 legend.position.all$h == "left",
							 legend.position.all$v == "top",
							 legend.position.all$h == "right") * legend.present.auto[1]
		
		margins.used.sides = c(bottom = legend.position.sides$v == "bottom",
							   left = legend.position.sides$h == "left",
							   top = legend.position.sides$v == "top",
							   right = legend.position.sides$h == "right") * legend.present.auto[c(3,2,3,2)]
		
		
		margins.used =  margins.used.all | margins.used.sides | legend.present.fix
		
		if (meta.automatic && any(margins.used)) {
			meta.auto.margins = rep(meta.auto.margins, length.out = 4)
			if (all(!margins.used[c(1,3)]) && n == 1) {
				# auto adjust left/right
				meta.margins[margins.used] =  pmax(meta.auto.margins[margins.used], (1 - pasp/masp - 2*bufferW)) / sum(margins.used)
			} else if (all(!margins.used[c(2,4)]) && n == 1) {
				# auto adjust top/right
				meta.margins[margins.used] =  pmax(meta.auto.margins[margins.used], (1 - masp/pasp - 2*bufferH)) / sum(margins.used) } 
			else {
				meta.margins[margins.used] = meta.auto.margins[margins.used]
			}
			
			# redo calculations
			meta.buffers = sign(meta.margins) * c(bufferH, bufferW, bufferH, bufferW) # outside and inside
			fixedMargins  =  outer.margins + meta.buffers * 2 + meta.margins + xylab.margins + panel.xtab.size + grid.buffers + grid.margins
		}
		
		
		
		
		# determine number of rows and cols
		if (!is.wrap) {
			nrows = nby[1]
			ncols = nby[2]
		} else {
			
			if (is.na(nrows) && !is.na(ncols)) {
				nrows = ceiling((nby[1] / ncols))
			} else if (!is.na(nrows) && is.na(ncols)) {
				ncols = ceiling((nby[1] / nrows))
			} else if (is.na(nrows) && is.na(ncols)) {
				
				# loop through col row combinations to find best nrow/ncol
				# b needed to compare landscape vs portrait. E.g if prefered asp is 2, 1 is equally good as 4
				ncols = which.min(vapply(1L:n, function(nc) {
					nr = ceiling(n / nc)
					
					# calculate available width and height. They can be negative, at this stage this is avoided my taking at least a small number
					width = max(1e-9, ((1 - sum(fixedMargins[c(2, 4)])) - (nc * sum(panel.wrap.size[c(2,4)])) - (nc - 1) * between.marginW) / nc)
					height = max(1e-9, ((1 - sum(fixedMargins[c(1, 3)])) - (nr * sum(panel.wrap.size[c(1,3)])) - (nr - 1) * between.marginH) / nr)
					
					a = (width / height) * dasp
					b = ifelse(a<pasp, pasp/a, a/pasp)
					b
				}, FUN.VALUE = numeric(1)))
				
				
				nrows = ceiling(n / ncols)
			}
		}
		
		#overall scale down factor for facets
		width_forn = max(1e-9, ((1 - sum(fixedMargins[c(2, 4)])) - (ncols * sum(panel.wrap.size[c(2,4)])) - (ncols - 1) * between.marginW) / ncols)
		width_for1 = max(1e-9, ((1 - sum(fixedMargins[c(2, 4)])) - (sum(panel.wrap.size[c(2,4)]))))

		height_forn = max(1e-9, ((1 - sum(fixedMargins[c(1, 3)])) - (nrows * sum(panel.wrap.size[c(1,3)])) - (nrows - 1) * between.marginH) / nrows)
		height_for1 = max(1e-9, ((1 - sum(fixedMargins[c(1, 3)])) - (sum(panel.wrap.size[c(1,3)]))))
		
		scale_down = (1 / sqrt((width_for1 * height_for1) / (width_forn * height_forn))) ^ (1 / scale.factor)
		
		title.size = title.size * scale
		legend.title.size = legend.title.size * scale
		legend.text.size = legend.text.size * scale

		panel.label.size = panel.label.size * scale
		
		
		# update panel labels
		if (is.na(panel.labels[1])) {
			panel.labels = fl[1:2]
		} else {
			if (!is.list(panel.labels)) panel.labels = list(panel.labels, "")
			panel.labels = mapply(FUN = function(p, f) {
				if (length(p) != length(f)) warning("the number of supplied panel labels does not correspond to the number of panels", call. = FALSE)
				rep_len(p, length(f))
			}, panel.labels, fl[1:2], SIMPLIFY = FALSE)
		}
		
		
		

		npages = ceiling(n / (nrows * ncols))	
		
		legend.position = NA
	})
	
}

