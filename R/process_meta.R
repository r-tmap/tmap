prepreprocess_meta = function(o, vp) {

	within(o, {
		vp = vp
		if (is.null(vp)) {
			devsize = graphics::par("fin") #dev.size() MAY NOT BE EQUAL IN RSTUDIO: https://github.com/rstudio/rstudio/issues/10723
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


		lin = graphics::par("cin")[2]# * scale

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

		if (is.na(panel.type)) panel.type = if (identical(panel.show, FALSE)) {
			"none"
		} else if (identical(panel.show, TRUE)) {
			# force panel labels
			if (type %in% c("wrap", "stack", "page") || (n == 1)) {
				"wrap"
			} else {
				"xtab"
			}
		} else if ((n == 1) && is.na(panel.labels[[1]])) {
			"none"
		} else if (!(type %in% c("wrap", "stack")) && !isdef[1] && !isdef[2]) {
			"none"
		} else if ((type %in% c("wrap", "stack", "page")) || (n == 1)) {
			"wrap"
		} else {
			"xtab"
		}

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
			if (type %in% c("wrap", "stack", "page")) {
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
		if (legend.present.auto[4] && (!any(legend.present.auto[2:3]))) {

			if (type == "stack") {
				legend.present.auto[1] = TRUE
				legend.present.auto[4] = FALSE
				set_to_stack_message = FALSE
			} else {
				set_to_stack_message = TRUE
			}
		} else {
			set_to_stack_message = FALSE
		}

	})
}

process_meta = function(o, d, cdt, aux) {

	if (o$legend.only) {
		return(within(o, {
			meta.buffers = c(0, 0, 0, 0)
			meta.margins = c(0, 0, 0, 0)
			xylab.margins = rep(0, 4)
			panel.xtab.size = c(0, 0, 0, 0)
			panel.xtab.margin = rep(0, 4)
			panel.wrap.margin = rep(0, 4)
			grid.buffers = c(0, 0, 0, 0)
			grid.margins = c(0, 0, 0, 0)
			panel.wrap.size = c(0, 0, 0, 0)
			fixedMargins  =  outer.margins + meta.buffers * 2 + meta.margins
			nrows = 1L
			ncols = 1L

			between_marginH = between_margin * lineH
			between_marginW = between_margin * lineW


			#overall scale down factor for facets
			width_forn = max(1e-9, ((1 - sum(fixedMargins[c(2, 4)])) - (ncols * sum(panel.wrap.size[c(2,4)])) - (ncols - 1) * between_marginW) / ncols)
			width_for1 = max(1e-9, ((1 - sum(fixedMargins[c(2, 4)])) - (sum(panel.wrap.size[c(2,4)]))))

			height_forn = max(1e-9, ((1 - sum(fixedMargins[c(1, 3)])) - (nrows * sum(panel.wrap.size[c(1,3)])) - (nrows - 1) * between_marginH) / nrows)
			height_for1 = max(1e-9, ((1 - sum(fixedMargins[c(1, 3)])) - (sum(panel.wrap.size[c(1,3)]))))

			scale_down = (1 / sqrt((width_for1 * height_for1) / (width_forn * height_forn))) ^ (1 / scale.factor)

		}))
	}


	gs = tmap_graphics_name()

	# add tm_grid values to o
	gid = which(vapply(aux, FUN = inherits, "tm_grid", FUN.VALUE = logical(1)))[1]
	if (!is.na(gid)) {
		a = aux[[gid]]$args
		a$zindex = NULL
		a$group = NULL
		o[paste0("grid.", names(a))] = a
	}

	# add credits into to o (for view mode in order to reset default attribution text)
	cid = which(vapply(cdt$comp, FUN = inherits, "tm_credits", FUN.VALUE = logical(1)))[1]
	o$credits.defined = (!is.na(cid))

	bbx = d$bbox[[1]]
	within(o, {
		# sasp shape aspect ratio (NA if free coordinates)
		diff_asp = any(d$asp != d$asp[1])
		sasp = ifelse(diff_asp, NA, d$asp[1])

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


		if (gs == "Grid") {

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

			panel.margin = get_option_class(panel.margin, panel.type, spatial_class = FALSE)

			panel.xtab.margin = if (panel.type == "xtab") {
				c(ifelse("bottom" %in% panel.xtab.pos, panel.margin * lineH, 0),
				  ifelse("left" %in% panel.xtab.pos, panel.margin * lineW, 0),
				  ifelse("top" %in% panel.xtab.pos, panel.margin * lineH, 0),
				  ifelse("right" %in% panel.xtab.pos, panel.margin * lineW, 0))
			} else c(0, 0, 0, 0)

			panel.wrap.margin =	if (panel.type == "wrap") {
				c(ifelse(panel.wrap.pos == "bottom", panel.margin * lineH, 0),
				  ifelse(panel.wrap.pos == "left", panel.margin * lineW, 0),
				  ifelse(panel.wrap.pos == "top", panel.margin * lineH, 0),
				  ifelse(panel.wrap.pos == "right", panel.margin * lineW, 0))
			} else c(0, 0, 0, 0)


			panel.wrap.size = if (panel.type == "wrap") {
				c(ifelse(panel.wrap.pos == "bottom", panel.label.height * lineH, 0),
				  ifelse(panel.wrap.pos == "left", panel.label.height * lineW, 0),
				  ifelse(panel.wrap.pos == "top", panel.label.height * lineH, 0),
				  ifelse(panel.wrap.pos == "right", panel.label.height * lineW, 0))
			} else c(0, 0, 0, 0)

			xylab.margins = rep(0, 4)
			if (xlab.show) {
				#if (is.na(xlab.fontface)) xlab.fontface = text.fontface
				xylab.margins[ifelse(xlab.side == "bottom", 1, 3)] = if (xlab.rotation %in% c(0, 180)) {
					(number_text_lines(xlab.text) + xlab.space)  * lineH
				} else {
					(text_width_inch(xlab.text, space = FALSE) / lin) * lineH
				}
			}
			if (ylab.show) {
				#if (is.na(ylab.fontface)) ylab.fontface = text.fontface
				xylab.margins[ifelse(ylab.side == "left", 2, 4)] = if (ylab.rotation %in% c(90, 270)) {
					(number_text_lines(ylab.text) + ylab.space)  * lineW
				} else {
					(text_width_inch(ylab.text, space = FALSE) / lin) * lineW
				}
			}


			grid.buffers = if (grid.show) {
				as.integer(c("bottom", "left", "top", "right") %in% grid.labels.pos) * c(bufferH, bufferW, bufferH, bufferW)
			} else {
				rep(0, 4)
			}



			grid.labels.show = rep(grid.labels.show, length.out = 2) # also happens in tmapGridGridPrep
			if (grid.show && any(grid.labels.show) && !grid.labels.inside_frame) {
				proj = sf::st_crs(bbx)
				if (!is.na(o$grid.crs)) {
					bbx_orig <- bbx
					bbx <- suppressWarnings(bb(bbx, current.projection = proj, projection = o$grid.crs))
				}

				lineHin <- convertHeight(unit(grid.labels.size, "lines"), "inch", valueOnly=TRUE)

				if (grid.labels.show[1]) {
					gridx = pretty30(bbx[c(1,3)], n = 5, longlat = !is.na(o$grid.crs) && sf::st_is_longlat(proj))
					xbbstringWin <- max(convertWidth(stringWidth(do.call("fancy_breaks", c(list(vec=gridx, intervals = FALSE), grid.labels.format))), "inch", valueOnly = TRUE)) * grid.labels.size
					xgridHin <- ifelse(!is.na(grid.labels.space.x), grid.labels.space.x * lineHin, ifelse(grid.labels.rot[1] %in% c(0, 180), 1.375 * lineHin, xbbstringWin + lineHin * .75) + grid.labels.margin.x * lineHin)

				} else {
					xgridHin = 0
				}

				if (grid.labels.show[2]) {
					gridy = pretty30(bbx[c(2,4)], n = 5, longlat = !is.na(o$grid.crs) && sf::st_is_longlat(proj))
					ybbstringWin = max(
						convertWidth(
							stringWidth(do.call("fancy_breaks", c(
								list(vec=gridy, intervals=FALSE), grid.labels.format))), "inch", valueOnly = TRUE)
						)

					ybbstringWin = ybbstringWin * grid.labels.size
					ygridWin = ifelse(!is.na(grid.labels.space.y), grid.labels.space.y * lineHin, ifelse(grid.labels.rot[2] %in% c(0, 180), ybbstringWin + lineHin * .75, 1.375 * lineHin) + grid.labels.margin.y * lineHin)
				} else {
					ygridWin = 0
				}

				marks_new = c(xgridHin, ygridWin, xgridHin, ygridWin) / lin
				grid.margins = as.integer(c("bottom", "left", "top", "right") %in% grid.labels.pos) * marks_new * c(lineH, lineW, lineH, lineW)
			} else {
				grid.margins = rep(0, 4)
			}

			between_marginH = between_margin * lineH
			between_marginW = between_margin * lineW

			fixedMargins = outer.margins + meta.buffers * 2 + meta.margins + xylab.margins + panel.xtab.size + grid.buffers + grid.margins
		} else {
			grid.buffers = rep(0, 4)
			grid.labels.show = c(FALSE, FALSE)
			grid.margins = rep(0, 4)
			fixedMargins = rep(0, 4)
			panel.wrap.size = rep(0, 4)
			between_marginH = 0
			between_marginW = 0
		}


		masp = ((1 - sum(fixedMargins[c(2, 4)])) / (1 - sum(fixedMargins[c(1, 3)]))) * dasp

		# Aspect ratios:
		# sasp: shape
		# asp: user specified
		# pasp: prefered (asp or if not specified, sasp)
		# masp: multiples (facets) area

		# determine where to place automatic legends (i.e. legends with local legend.position = NA and with legend.position = tm_pos_auto_out() enabled)
		# this is also neede to find out which margins are taken from meta.auto_margins

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


		if (gs == "Grid") {
			if (type %in% c("stack", "page")) {
				if (is.na(orientation)) {
					if (nrow(cdt)) {
						legs_auto = cdt[class=="autoout"]
					} else {
						legs_auto = cdt
					}


					if (type == "page" || (nrow(legs_auto) && n == 1)) {

						legWmax = min(max(legs_auto$legW) / devsize[1], max(meta.auto_margins[c(2,4)]))
						legHmax = min(max(legs_auto$legH) / devsize[2], max(meta.auto_margins[c(1,3)]))


						av_width = mx_width - legWmax * devsize[1]
						av_height = mx_height - legHmax * devsize[2]

						shp_height_hor = if ((av_width / mx_height) < pasp) av_width / pasp else mx_height
						shp_height_ver = if ((mx_width / av_height) < pasp) mx_width / pasp else av_height

						orientation = if (shp_height_hor >= shp_height_ver) "vertical" else "horizontal"
					} else {
						orientation = if ((!is.na(nrows) && nrows == 1) || (!is.na(ncols) && ncols == n)) {
							"horizontal"
						} else if ((!is.na(nrows) && nrows == n) || (!is.na(ncols) && ncols == 1)) {
							"vertical"
						} else if ((n == 1 && (pasp > masp)) || (n > 1 && (pasp < masp))) "horizontal" else "vertical"
					}
				}
			}
		} else {
			orientation = if ((n == 1 && (pasp > masp)) || (n > 1 && (pasp < masp))) "horizontal" else "vertical"
		}

		if (gs == "Grid") {
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
				cdt2 = data.table::copy(cdt[cdt$class %in% c("autoout", "out"),])

				# CODE COPIED FROM STEP4_plot L157
				# TO DO: fix this
				if (o$type != "grid" && o$n > 1) {
					#if (o$nrows == 1 && o$ncols == 1)
					if (identical(orientation, "horizontal")) {
						# -use by2 and not by1 when they form a row
						cdt2[, by2__ := by1__]
						cdt2[, by1__ := NA]
					}
				}


				stacks = o$legend.stack

				cdt2[is.na(by1__) & is.na(by2__) & class == "autoout", ':='(cell.h = legend.position.all$cell.h, cell.v = legend.position.all$cell.v)]
				cdt2[!is.na(by1__) & is.na(by2__) & class == "autoout", ':='(cell.h = legend.position.sides$cell.h, cell.v = "by")]
				cdt2[is.na(by1__) & !is.na(by2__) & class == "autoout", ':='(cell.h = "by", cell.v = legend.position.sides$cell.v)]

				cdt2[is.na(by1__) & is.na(by2__) & class == "autoout", ':='(stack = ifelse(stack_auto, ifelse(cell.h == "center", stacks["all_col"], ifelse(cell.v == "center", stacks["all_row"], stacks["all"])), stack))]
				cdt2[!is.na(by1__) & is.na(by2__) & class == "autoout", ':='(stack = ifelse(stack_auto, stacks["per_row"], stack))]
				cdt2[is.na(by1__) & !is.na(by2__) & class == "autoout", ':='(stack = ifelse(stack_auto, stacks["per_col"], stack))]


				cdt2[class == "autoout", class := "out"]



				if (nrow(cdt2) == 0) {
					meta.auto_margins = c(0, 0, 0, 0)
				} else {
					if (type == "stack") {
						# workaround: stacking mode is determined later (step4 L156), because it requires ncols and nrows
						# for stack, this is already known, so therefore we can better estimate the meta width and height

						cdt2[is.na(by1__), by1__:=1]

						meta.auto_margins = pmin(meta.auto_margins, do.call(pmax, lapply(unique(cdt2$by1__), function(b1) {
							cdt2b = cdt2[by1__==b1, ]

							cdt2b[stack_auto == TRUE, stack:= ifelse(n==1, ifelse(cell.h %in% c("left", "right"), o$legend.stack["all_row"], o$legend.stack["all_col"]), ifelse(orientation == "vertical", o$legend.stack["per_row"], o$legend.stack["per_col"]))]

							c(sum(sum(c(0,cdt2b[cell.v == "bottom" & stack == "vertical", legH,by = c("cell.h", "cell.v")]$legH)),
								  max(c(0,cdt2b[cell.v == "bottom" & stack == "horizontal", legH,by = c("cell.h", "cell.v")]$legH))) / o$devsize[2],
							  sum(sum(c(0,cdt2b[cell.h == "left" & stack == "horizontal", legW,by = c("cell.h", "cell.v")]$legW)),
							  	max(c(0,cdt2b[cell.h == "left" & stack == "vertical", legW,by = c("cell.h", "cell.v")]$legW))) / o$devsize[1],
							  sum(sum(c(0,cdt2b[cell.v == "top" & stack == "vertical", legH,by = c("cell.h", "cell.v")]$legH)),
							  	max(c(0,cdt2b[cell.v == "top" & stack == "horizontal", legH,by = c("cell.h", "cell.v")]$legH))) / o$devsize[2],
							  sum(sum(c(0,cdt2b[cell.h == "right" & stack == "horizontal", legW,by = c("cell.h", "cell.v")]$legW)),
							  	max(c(0,cdt2b[cell.h == "right" & stack == "vertical", legW,by = c("cell.h", "cell.v")]$legW))) / o$devsize[1])
						})))
					} else {
						meta.auto_margins = pmin(meta.auto_margins,
												 c(max(cdt$legH[cdt$cell.v == "bottom" & cdt$class %in% c("autoout", "out")], 0) / o$devsize[2],
												   max(cdt$legW[cdt$cell.h == "left" & cdt$class %in% c("autoout", "out")], 0) / o$devsize[1],
												   max(cdt$legH[cdt$cell.v == "top" & cdt$class %in% c("autoout", "out")], 0) / o$devsize[2],
												   max(cdt$legW[cdt$cell.h == "right" & cdt$class %in% c("autoout", "out")], 0) / o$devsize[1]))
					}

					# add margins (compensate for legend frames)
					# the final calculations of these margins are computed in tmapGridComp (this is just to compute the meta.auto_margins)
					# those calculations are take the component.offset into account

					sel_tb = c(3,1)[meta.auto_margins[c(3,1)]!=0]
					sel_lr = c(2,4)[meta.auto_margins[c(2,4)]!=0]
					if (length(sel_tb)) meta.auto_margins[sel_tb] = meta.auto_margins[sel_tb] + 2 * (o$frame.lwd * o$scale / 144) / o$devsize[2]
					if (length(sel_lr)) meta.auto_margins[sel_lr] = meta.auto_margins[sel_lr] + 2 * (o$frame.lwd * o$scale / 144) / o$devsize[1]
				}
			}

			if (meta.automatic && any(margins.used)) {
				meta.auto_margins = rep(meta.auto_margins, length.out = 4)
				meta.margins[margins.used] = meta.auto_margins[margins.used]

				# redo calculations
				meta.buffers = sign(meta.margins) * c(bufferH, bufferW, bufferH, bufferW) # outside and inside
				fixedMargins  =  outer.margins + meta.buffers * 2 + meta.margins + xylab.margins + panel.xtab.size + grid.buffers + grid.margins
			}
		} else {
			meta.buffers = c(0, 0, 0, 0)
			meta.margins = c(0, 0, 0, 0)
		}


		# determine number of rows and cols
		if (type == "grid") {
			nrows = nby[1]
			ncols = nby[2]
		} else if (type == "page") {
			if (is.na(nrows)) nrows = 1
			if (is.na(ncols)) ncols = 1
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
					width = max(1e-9, ((1 - sum(fixedMargins[c(2, 4)])) - (nc * sum(panel.wrap.size[c(2,4)])) - (nc - 1) * between_marginW) / nc)
					height = max(1e-9, ((1 - sum(fixedMargins[c(1, 3)])) - (nr * sum(panel.wrap.size[c(1,3)])) - (nr - 1) * between_marginH) / nr)

					a = (width / height) * dasp
					b = ifelse(a<pasp, pasp/a, a/pasp)
					b
				}, FUN.VALUE = numeric(1)))


				nrows = ceiling(n / ncols)
			}
			if ((nrows == 1 || ncols == 1) && set_to_stack_message) message_wrapstack(nrows == 1)
		}

		#overall scale down factor for facets
		width_forn = max(1e-9, ((1 - sum(fixedMargins[c(2, 4)])) - (ncols * sum(panel.wrap.size[c(2,4)])) - (ncols - 1) * between_marginW) / ncols)
		width_for1 = max(1e-9, ((1 - sum(fixedMargins[c(2, 4)])) - (sum(panel.wrap.size[c(2,4)]))))

		height_forn = max(1e-9, ((1 - sum(fixedMargins[c(1, 3)])) - (nrows * sum(panel.wrap.size[c(1,3)])) - (nrows - 1) * between_marginH) / nrows)
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
				if (is.null(f)) {
					if (length(p) > 1) warning("the number of supplied panel labels is", length(p), "but only one is supported because no facets are defined", call. = FALSE)
					p[1]
				} else {
					if (length(p[p!=""]) != length(f)) warning("the number of supplied panel labels does not correspond to the number of panels", call. = FALSE)
					rep_len(p, length(f))
				}
			}, panel.labels, fl[1:2], SIMPLIFY = FALSE)
		}




		npages = ceiling(n / (nrows * ncols))

		legend.position = NA

		if (!is.logical(set_bounds)) if (length(set_bounds) !=4 || !is.numeric(set_bounds)) stop("Incorrect set_bounds argument", call.=FALSE)


		if (!is.na(set_view[1])) {
			if (!is.numeric(set_view)) stop("set_view is not numeric")
			if (!length(set_view) %in% c(1, 3)) stop("set_view does not have length 1 or 3")
		}
		if (!is.na(set_zoom_limits[1])) {
			if (!is.numeric(set_zoom_limits)) stop("set_zoom_limits is not numeric")
			if (!length(set_zoom_limits)==2) stop("set_zoom_limits does not have length 2")
			if (set_zoom_limits[1] >= set_zoom_limits[2]) stop("incorrect set_zoom_limits")
		} else {
			set_zoom_limits <- c(NA, NA)
		}
		if (!is.na(set_view[1]) && !is.na(set_zoom_limits[1])) {
			if (set_view[length(set_view)] < set_zoom_limits[1]) {
				if (show.warnings) warning("default zoom smaller than minimum zoom, now it is set to the minimum zoom")
				set_view[length(set_view)] <- set_zoom_limits[1]
			}
			if (set_view[length(set_view)] > set_zoom_limits[2]) {
				if (show.warnings) warning("default zoom larger than maximum zoom, now it is set to the maximum zoom")
				set_view[length(set_view)] <- set_zoom_limits[2]
			}
		}


	})

}

