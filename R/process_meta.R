prepreprocess_meta = function(o, vp) {
	
	within(o, {
		vp = vp
		if (is.null(vp)) {
			devsize = dev.size()
		} else {
			if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
			devsize = c(grid::convertWidth(grid::unit(1, "npc"), unitTo = "inch", valueOnly = TRUE),
						grid::convertHeight(grid::unit(1, "npc"), unitTo = "inch", valueOnly = TRUE))
		}
		
		# dasp device aspect ratio
		dasp = devsize[1] / devsize[2]
		
		# needed for spnc viewport (to retain aspect ratio)
		if (dasp > 1) {
			cw = dasp
			ch = 1
		} else {
			ch = 1/dasp
			cw = 1
		}
		
		
		lin = par("cin")[2]# * scale

		lineH = lin / devsize[2] * scale
		lineW = lin / devsize[1] * scale
		
		# not needed?
		#nlinesH = 1/lineH
		#nlinesW = 1/lineW
		
	})
}
preprocess_meta = function(o, cdt) {
	within(o, {
		nby = fn #get_nby(fl)
		isdef = !sapply(fl, is.null)
		n = prod(nby)
		
		
		if (is.na(panel.type)) panel.type = ifelse((n == 1 && is.na(panel.labels[[1]])) || ((type %in% c("wrap", "stack")) && !isdef[1]) || (!(type %in% c("wrap", "stack")) && !isdef[1] && !isdef[2]), "none", 
										    ifelse((type %in% c("wrap", "stack")) || (n == 1), "wrap", "xtab"))
		
		inner.margins = get_option_class(inner.margins, class = main_class)

		# legend.present.auto:
		#   find out whether there are legends for all facets, per row, per col
		#   use them to automatically determine meta.margins (in preprocess_meta)
		# # legend.present.fix
		#	find legend boxes that are assigned to outer margins
		
		if (nrow(cdt) == 0) {
			legend.present.auto = c(all = FALSE, per_row = FALSE, per_col = FALSE, per_facet = FALSE)
			legend.present.fix = rep(FALSE, 4)
		} else {
			if (type %in% c("wrap", "stack")) {
				#o$legend.present.auto = c(all = any(is.na(cdt$by1__) & cdt$class == "autoout"), per_row = any(!is.na(cdt$by1__) & cdt$class == "autoout"), per_col = FALSE)
				legend.present.auto = c(all = any(cdt$class == "autoout" & is.na(cdt$by1__)), 
										per_row = FALSE, per_col = FALSE, 
										per_facet = any(cdt$class == "autoout" & !is.na(cdt$by1__)))
			} else {
				legend.present.auto = c(all = any(is.na(cdt$by1__) & is.na(cdt$by2__) & cdt$class == "autoout"), 
										per_row = any(!is.na(cdt$by1__) & is.na(cdt$by2__) & cdt$class == "autoout"), 
										per_col = any(is.na(cdt$by1__) & !is.na(cdt$by2__) & cdt$class == "autoout"),
										per_facet = any(!is.na(cdt$by1__) & !is.na(cdt$by2__) & cdt$class == "autoout"))
			}
			legend.present.fix = c(any(cdt$class == "out" & cdt$cell.v == "bottom"), 
								   any(cdt$class == "out" & cdt$cell.h == "left"),
								   any(cdt$class == "out" & cdt$cell.v == "top"),
								   any(cdt$class == "out" & cdt$cell.h == "right"))
		}
		
		
		# in case there are per-facet legends but no no marginal legends, and nrows or ncols equals 1, place them outside (to do this, set them to all-facet here, change legend.position.all below accordingly, and finally determine legend position in step4_plot)
		if (legend.present.auto[4] && (!any(legend.present.auto[2:3])) && (type == "stack")) {
			per_facet_wrap_outside = TRUE
			legend.present.auto[1] = TRUE
			legend.present.auto[4] = FALSE
		} else {
			per_facet_wrap_outside = FALSE
		}
		
		
	})
}

process_meta = function(o, d, cdt) {
	within(o, {
		# sasp shape aspect ratio (NA if free coordinates)
		diff_asp = any(d$asp != d$asp[1])
		sasp = ifelse(diff_asp, NA, d$asp[1])
		
		
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
		
		# determine where to place automatic legends (i.e. legends with local legend.position = NA and with legend.position = tm_pos_auto_out() enabled)
		# this is also neede to find out which margins are taken from meta.auto.margins
		
		legend.position.sides = legend.position
		legend.position.all = legend.position
		
	#	legsG = cdt[, leg]
		
		# determine orientation of stacked maps
		# it also implies where legends will be drawn: horizontal orientation=legends bottom or top, vertical orientation=legends left or right
		# !!! this also applies for single maps
		mx_width = (1 - sum(fixedMargins[c(1, 3)])) * devsize[1]
		mx_height = (1 - sum(fixedMargins[c(2, 4)])) * devsize[2]
		
		# cdt[, scale := pmax(legW/mx_width, legH/mx_height, 1)]
		# cdt[, ":="(legW_sc = legW/scale, legH_sc = legH/scale)]
		
		if (type == "stack") {
			if (is.na(orientation)) {
				legs_auto = cdt[class=="autoout"]
				
				if (nrow(legs_auto) && n == 1) {
					
					legWmax = min(max(legs_auto$legW) / devsize[1], max(meta.auto.margins[c(2,4)])) 
					legHmax = min(max(legs_auto$legH) / devsize[2], max(meta.auto.margins[c(1,3)]))
					

					av_width = mx_width - legWmax * devsize[1]
					av_height = mx_height - legHmax * devsize[2]
					
					shp_height_hor = if ((av_width / mx_height) < pasp) av_width / pasp else mx_height
					shp_height_ver = if ((mx_width / av_height) < pasp) mx_width / pasp else av_height
					
					orientation = if (shp_height_hor >= shp_height_ver) "vertical" else "horizontal"
				} else {
					orientation = if ((n == 1 && (pasp > masp)) || (n > 1 && (pasp < masp))) "horizontal" else "vertical"
				}
			}
		}
		

		## find position for all-facet legend
		if (legend.present.auto[1]) {
			if (!legend.present.auto[2] & !legend.present.auto[3]) {
				# only 'all facets' outside legends (either bottom or right)
				# was: n > 1 && masp > pasp
				if ((type != "stack" && n == 1 && pasp > masp) || (type != "stack" && n > 1 && masp < 1) || (type == "stack" && orientation == "horizontal")) {
					legend.position.all = list(cell.h = "center", cell.v = legend.position$cell.v)
				} else {
					legend.position.all = list(cell.h = legend.position$cell.h, cell.v = "center")
				}
			} else if (legend.present.auto[2] & !legend.present.auto[3]) {
				# central goes center bottom 
				legend.position.all = list(cell.h = "center", cell.v = legend.position$cell.v)
			} else if (!legend.present.auto[2] & legend.present.auto[3]) {
				# central goes right center 
				legend.position.all = list(cell.h = legend.position$cell.h, cell.v = "center")
			}
		}
		
		margins.used.all = c(legend.position.all$cell.v == "bottom",
							 legend.position.all$cell.h == "left",
							 legend.position.all$cell.v == "top",
							 legend.position.all$cell.h == "right") * legend.present.auto[1]
		
		margins.used.sides = c(bottom = legend.position.sides$cell.v == "bottom",
							   left = legend.position.sides$cell.h == "left",
							   top = legend.position.sides$cell.v == "top",
							   right = legend.position.sides$cell.h == "right") * legend.present.auto[c(3,2,3,2)]
		
		
		margins.used =  margins.used.all | margins.used.sides | legend.present.fix
		
		# tm_shape(World) + tm_polygons(fill = "HPI", lwd = "life_exp")
		
		
		if (nrow(cdt)) {
			meta.auto.margins = pmin(meta.auto.margins, 
									 c(max(cdt$legH[cdt$cell.v == "bottom" & cdt$class %in% c("autoout", "out")], 0) / o$devsize[2],
									   max(cdt$legW[cdt$cell.h == "left" & cdt$class %in% c("autoout", "out")], 0) / o$devsize[1],
									   max(cdt$legH[cdt$cell.v == "top" & cdt$class %in% c("autoout", "out")], 0) / o$devsize[2],
									   max(cdt$legW[cdt$cell.h == "right" & cdt$class %in% c("autoout", "out")], 0) / o$devsize[1]))
			#meta.auto.margins = c(0.13,0,0,0)
		}


		
		if (meta.automatic && any(margins.used)) {
			meta.auto.margins = rep(meta.auto.margins, length.out = 4)
			if (all(!margins.used[c(1,3)]) && n == 1) {
				# auto adjust left/right
				meta.margins[c(2,4)] =  local({
					xtra = max(0, (1 - pasp/masp - 2*bufferW) - sum(meta.auto.margins[margins.used]))
					tmp = rep(0, 4)
					tmp[margins.used] = meta.auto.margins[margins.used]
					tmp[c(2,4)] + (xtra / 2)
				})
			} else if (all(!margins.used[c(2,4)]) && n == 1) {
				# auto adjust top/bottom
				meta.margins[c(1,3)] =  local({
					xtra = max(0, (1 - masp/pasp - 2*bufferH) - sum(meta.auto.margins[margins.used]))
					# divide extra vertical space between used margins
					tmp = rep(0, 4)
					tmp[margins.used] = meta.auto.margins[margins.used]
					tmp[c(1,3)] + (xtra / 2)
				})
			} else {
				meta.margins[margins.used] = meta.auto.margins[margins.used]
			}
			
			# redo calculations
			meta.buffers = sign(meta.margins) * c(bufferH, bufferW, bufferH, bufferW) # outside and inside
			fixedMargins  =  outer.margins + meta.buffers * 2 + meta.margins + xylab.margins + panel.xtab.size + grid.buffers + grid.margins
		}
		
		
		
		
		# determine number of rows and cols
		if (type == "grid") {
			nrows = nby[1]
			ncols = nby[2] 
		} else if (type == "stack") {
			if (orientation == "horizontal") {
				nrows = 1
				ncols = n
			} else {
				nrows = n
				ncols = 1
			}
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
		# 
		# title.size = title.size * scale
		# legend.title.size = legend.title.size * scale
		# legend.text.size = legend.text.size * scale
		# 
		# panel.label.size = panel.label.size * scale
		
		
		# update panel labels
		if (is.na(panel.labels[1])) {
			panel.labels = fl[1:2]
		} else {
			if (!is.list(panel.labels)) panel.labels = list(panel.labels, "")
			panel.labels = mapply(FUN = function(p, f) {
				if (length(p[p!=""]) != length(f)) warning("the number of supplied panel labels does not correspond to the number of panels", call. = FALSE)
				rep_len(p, length(f))
			}, panel.labels, fl[1:2], SIMPLIFY = FALSE)
		}
		
		
		

		npages = ceiling(n / (nrows * ncols))	
		
		legend.position = NA
	})
	
}

