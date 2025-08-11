

grid_nonoverlap = function(x, s) {
	n = length(x)
	x = c(-x[1], x, 1 + (1 - x[n]))

	m_left = x[2:(n+1)] - x[1:n]
	m_right = x[3:(n+2)] - x[2:(n+1)]
	m = pmin(m_left, m_right)

	m > s
}

pretty30 = function(x, n, longlat) {
	p = pretty(x, n)
	if (!longlat) return(p)
	step = p[2] - p[1]
	if (step > 50) {
		x = p %/% 60 * 60
		seq(min(x), max(x), by = 60)
	} else if (step > 10) {
		x = p %/% 30 * 30
		seq(min(x), max(x), by = 30)
	} else {
		p
	}
}

#' @export
#' @keywords internal
#' @rdname tmapGridLeaflet
tmapGridAuxPrepare.tm_aux_graticules = function(a, bs, id, o) {
	tmapGridAuxPrepare.tm_aux_grid(a, bs, id, o)
}

#' @export
#' @keywords internal
#' @rdname tmapGridLeaflet
tmapGridAuxPrepare.tm_aux_grid = function(a, bs, id, o) {
	g = get("g", envir = .TMAP_GRID)




	#alpha <- labels.inside_frame <- labels.rot <- NULL
	a2 = within(a, {
		if (labels.inside_frame && !identical(labels.pos, c("left", "bottom"))) cli::cli_warn("label.pos not implemented yet for {.code labels.inside_frame==TRUE} (only {.val left} and {.val bottom})")
		show <- TRUE
		if (is.na(col)) col <- ifelse(o$attr.color.light, darker(o$attr.color, .5), lighter(o$attr.color, .5))
		if (is.na(labels.col)) labels.col <- ifelse(o$attr.color.light, darker(o$attr.color, .2), lighter(o$attr.color, .2))
		if (!is.numeric(labels.rot) || length(labels.rot) != 2) stop("labels.rot should be a numeric vector of length two")
		col <- do.call("process_color", c(list(col=col, alpha=alpha), o$pc))
		labels.col <- do.call("process_color", c(list(col=labels.col), o$pc))
		lwd <- lwd * o$scale
		is.projected <- !(crs=="longlat" || tryCatch(sf::st_is_longlat(crs), error = function(e) TRUE))

		crs <- sf::st_crs(crs)

		if (!labels.inside_frame && any(o$outer.margins[1:2]==0)) stop("When grid labels are plotted outside the frame, outer.margins (the bottom and the left) should be greater than 0. When using tmap_save, notice that outer.margins are set to 0 by default, unless set to NA.")
		if (!"scientific" %in% names(labels.format)) labels.format$scientific <- FALSE
		if (!"digits" %in% names(labels.format)) labels.format$digits <- NA

		labels.show <- rep(labels.show, length.out = 2)
		ticks <- rep(ticks, length.out = 2)

		add.labels = labels.inside_frame & labels.show


	})

	o$scale.extra = 1

	a3s = lapply(bs, function(bbx) {
		crs_bb = sf::st_crs(bbx)
		within(a2, {
			if (!is.na(crs)) {
				bbx_orig <- bbx
				bbx <- suppressWarnings(bb(bbx, current.projection = crs_bb, projection = crs))
			}
			sasp = get_asp_ratio(bbx)

			## automatically determine number of grid lines
			if (is.na(n.x) && !is.na(n.y)) {
				n.x <- n.y * sasp
			} else if (!is.na(n.x) && is.na(n.y)) {
				n.y <- n.x / sasp
			} else if (is.na(n.x) && is.na(n.y)) {
				n.lines <- 15 / (o$scale / o$scale.extra)
				n.x <- round(sasp * (n.lines/(1+sasp)))
				n.y <- round(n.lines / (1+sasp))
			}

			## find natural breaks
			custom.x = !is.na(x[1])
			custom.y = !is.na(y[1])

			if (!custom.x) x <- pretty30(bbx[c(1,3)], n=n.x, longlat = !is.na(crs) && sf::st_is_longlat(crs_bb))
			if (!custom.y) y <- pretty30(bbx[c(2,4)], n=n.y, longlat = !is.na(crs) && sf::st_is_longlat(crs_bb))

			## copy x and y
			x.orig <- x
			y.orig <- y

			## crop
			x <- x[x > bbx[1] & x < bbx[3]]
			y <- y[y > bbx[2] & y < bbx[4]]

			## project grid lines
			if (!is.na(crs)) {
				## add extra grid lines to make sure the warped grid is full
				if (custom.x) {
					x2 <- x.orig
				} else {
					gnx2 <- floor(length(x))
					if (gnx2==1) {
						x2 <- x
					} else if (gnx2>1) {
						x2 <- c(rev(seq(x[1], by=-diff(x[1:2]), length.out = gnx2)),
								x[-c(1, length(x))],
								seq(x[length(x)], by=diff(x[1:2]), length.out = gnx2))
					} else x2 <- NA
				}
				if (custom.y) {
					y2 <- y.orig
				} else {
					gny2 <- floor(length(y))
					if (gny2==1) {
						y2 <- y
					} else if (gny2>1) {
						y2 <- c(rev(seq(y[1], by=-diff(y[1:2]), length.out = gny2)),
								y[-c(1, length(y))],
								seq(y[length(y)], by=diff(y[1:2]), length.out = gny2))
					} else y2 <- NA
				}
				if (!is.projected) {
					# x2[abs(x2-180)<1e-9] <- 180
					# x2[abs(x2- -180)<1e-9] <- -180
					# y2[abs(y2-90)<1e-9] <- 90
					# y2[abs(y2- -90)<1e-9] <- -90
					x2 <- x2[x2>=-180 & x2<=180]
					y2 <- y2[y2>=-90 & y2<=90]
				}
				gnx2 <- gny2 <- NULL

				## determine limits
				x2.min <- min(min(x2), bbx[1], na.rm=TRUE)
				x2.max <- max(max(x2), bbx[3], na.rm=TRUE)
				y2.min <- min(min(y2), bbx[2], na.rm=TRUE)
				y2.max <- max(max(y2), bbx[4], na.rm=TRUE)

				lnsSel <- c(length(x2) && !is.na(x2[1]),
							length(y2) && !is.na(y2[1]))

				if (lnsSel[1]) {
					lnsX = sf::st_sfc(lapply(x2, function(x) {
						sf::st_linestring(matrix(c(rep(x,ndiscr), seq(y2.min, y2.max, length.out=ndiscr)), ncol=2))
					}), crs = crs)


					lnsX_proj_res <- transform_ortho(lnsX, crs = crs_bb, tmapID = 1:length(x2))

					# keeping longest grid line, however some CRS's may have MULTILINESTRING grid lines
					lnsX_proj = to_longest_linestring(lnsX_proj_res$shp)

					lnsX_proj_emp = sf::st_is_empty(lnsX_proj)
					lnsX_proj <- lnsX_proj[!lnsX_proj_emp]

					lnsX_emp = rep(TRUE, length(x2))
					lnsX_emp[lnsX_proj_res$tmapID] <- lnsX_proj_emp

					x2 <- x2[!lnsX_emp]

					xco <- sf::st_coordinates(lnsX_proj)
					# co.x.lns
					co.x <- lapply(unique(xco[,3]), function(i) {
						lco <- xco[xco[,3]==i, 1:2,drop = FALSE]
						lco[, 1] <- (lco[, 1]-bbx_orig[1]) / (bbx_orig[3] - bbx_orig[1])
						lco[, 2] <- (lco[, 2]-bbx_orig[2]) / (bbx_orig[4] - bbx_orig[2])
						lco
					})
					lnsX <- NULL
					lnsX_proj <- NULL
					lnsX_emp <- NULL
					lnsX_proj_res <- NULL

					sel.x <- which(x2 %in% x)
				} else {
					co.x <- numeric(0)
				}

				if (lnsSel[2]) {
					lnsY = sf::st_sfc(lapply(y2, function(y) {
						sf::st_linestring(matrix(c(seq(x2.min, x2.max, length.out=ndiscr), rep(y,ndiscr)), ncol=2))
					}), crs = crs)
					lnsY_proj_res <- transform_ortho(lnsY, crs = crs_bb, tmapID = 1:length(y2))

					lnsY_proj = to_longest_linestring(lnsY_proj_res$shp)

					lnsY_proj_emp = sf::st_is_empty(lnsY_proj)
					lnsY_proj <- lnsY_proj[!lnsY_proj_emp]

					lnsY_emp = rep(TRUE, length(y2))
					lnsY_emp[lnsY_proj_res$tmapID] <- lnsY_proj_emp

					y2 <- y2[!lnsY_emp]

					yco <- sf::st_coordinates(lnsY_proj)
					co.y <- lapply(unique(yco[,3]), function(i) {
						lco <- yco[yco[,3]==i, 1:2,drop=FALSE]
						lco[, 1] <- (lco[, 1]-bbx_orig[1]) / (bbx_orig[3] - bbx_orig[1])
						lco[, 2] <- (lco[, 2]-bbx_orig[2]) / (bbx_orig[4] - bbx_orig[2])
						lco
					})
					lnsY <- NULL
					lnsY_proj <- NULL
					lnsY_emp <- NULL
					lnsY_proj_res <- NULL

					sel.y <- which(y2 %in% y)
				}	else {
					co.y <- numeric(0)
				}


				labels.show <- labels.show & lnsSel # update needed for plot_n
			} else {
				# normalize coordinates
				co.x <- (x-bbx[1]) / (bbx[3] - bbx[1])
				co.y <- (y-bbx[2]) / (bbx[4] - bbx[2])
			}

			## format grid labels


			labels.x <- local({
				if (labels.cardinal) {
					xneg <- x < 0
					xpos <- x > 0

					xlab <- do.call("fancy_breaks", c(list(vec=abs(x), intervals=FALSE), o$labels.format)) #format(x, big.mark = ",")

					xlab[xpos] <- paste0(xlab[xpos], "\u00B0E")
					xlab[xneg] <- paste0(xlab[xneg], "\u00B0W")
					xlab
				} else {
					do.call("fancy_breaks", c(list(vec=x, intervals=FALSE), o$labels.format)) #format(x, big.mark = ",")
				}
			})


			labels.y <- local({
				if (labels.cardinal) {
					yneg <- y < 0
					ypos <- y > 0

					ylab <- do.call("fancy_breaks", c(list(vec=abs(y), intervals=FALSE), o$labels.format))

					ylab[ypos] <- paste0(ylab[ypos], "\u00B0N")
					ylab[yneg] <- paste0(ylab[yneg], "\u00B0S")
					ylab
				} else {
					do.call("fancy_breaks", c(list(vec=y, intervals=FALSE), o$labels.format))
				}
			})

			if (!is.na(crs)) {
				# remove lines with one coordinate-row, and update related list items
				cox_sel = vapply(co.x, nrow, FUN.VALUE = integer(1)) >= 2
				co.x = co.x[cox_sel]
				x2 = x2[cox_sel]
				sel.x = which(x2 %in% x)

				coy_sel = vapply(co.y, nrow, FUN.VALUE = integer(1)) >= 2
				co.y = co.y[coy_sel]
				y2 = y2[coy_sel]
				sel.y = which(y2 %in% y)
			}

		})

	})

	g$grid_comp_per_bbx = a3s

	assign("g", g, envir = .TMAP_GRID)

	return("grid")
}

#' @export
#' @keywords internal
#' @rdname tmapGridLeaflet
tmapGridAuxPlot.tm_aux_basemap = function(a, bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	tmapGridAuxPlot.tm_aux_tiles(a, bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o)
}

#' @export
#' @keywords internal
#' @rdname tmapGridLeaflet
tmapGridAuxPlot.tm_aux_tiles = function(a, bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	g = get("g", envir = .TMAP_GRID)

	# id is on aux level
	id2 = which(names(g$bmaps_dts) == as.character(id))

	dt = g$bmaps_dts[[id2]][[bi]]
	shpTM = g$bmaps_shpTHs[[id2]][[bi]]
	gp = list()

	class(a) = "tm_data_raster"

	a2 = structure(list(interpolate = TRUE), class = "tm_data_raster")
	if (!is.null(dt)) tmapGridDataPlot(a2, shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o)
}


tmapGridGridXLab = function(bi, bbx, facet_row, facet_col, facet_page, o) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)
	gt = gts[[facet_page]]

	is_top = o$grid.labels.pos[2] == "top"


	rc_text = frc(facet_row, facet_col)

	rowid = g$rows_facet_ids[facet_row] + ifelse(is_top, -3, 3)
	colid = g$cols_facet_ids[facet_col]

	H = g$rowsIn[rowid]
	W = g$colsIn[colid]


	a = g$grid_comp_per_bbx[[bi]]



	labelsx <- a$labels.x

	# find coordinates for projected grid labels
	if (!is.na(a$crs)) {
		glabelsx <- get_gridline_labels(lco=a$co.x[a$sel.x], xax = as.integer(is_top))
		cogridx <- glabelsx$cogrid
		idsx <- glabelsx$ids
		labelsx <- labelsx[idsx]
	} else {
		cogridx <- a$co.x
	}

	cex <- a$labels.size*o$scale

	# remove overlapping grid labels
	widths = text_width_inch(labelsx) * cex / W
	selx2 = grid_nonoverlap(cogridx, widths)
	if (!any(selx2)) return(NULL)
	labelsx2 = labelsx[selx2]
	cogridx2 = cogridx[selx2]


	spacerX <- grid::convertHeight(unit(.5, "lines"), unitTo="inch", valueOnly=TRUE) * cex / H
	marginX <- grid::convertHeight(unit(a$labels.margin.x, "lines"), unitTo="inch", valueOnly=TRUE) * cex / H

	just <- ifelse(a$labels.rot[1] == 90, "right",
				   ifelse(a$labels.rot[1] == 270, "left",
				   	   ifelse(a$labels.rot[1] == 180, "bottom", "top")))

	if (a$ticks[1]) {
		if (is_top) {
			ticks <- grid::polylineGrob(x=rep(cogridx2, each = 2), y = rep(c(0, spacerX*.5+marginX), length(cogridx2)), id = rep(1:length(cogridx2), each = 2), gp=grid::gpar(col=a$col, lwd=a$lwd))
		} else {
			ticks <- grid::polylineGrob(x=rep(cogridx2, each = 2), y = rep(c(1-spacerX*.5-marginX,1), length(cogridx2)), id = rep(1:length(cogridx2), each = 2), gp=grid::gpar(col=a$col, lwd=a$lwd))
		}
	} else {
		ticks <- NULL
	}

	if (is_top) {
		labels <- grid::textGrob(labelsx2, y=1-spacerX/2, x=cogridx2, just=just, rot=a$labels.rot[1], gp=grid::gpar(col=a$labels.col, cex=cex, fontface=a$labels.fontface, fontfamily=a$labels.fontfamily))
	} else {
		labels <- grid::textGrob(labelsx2, y=1-spacerX-marginX, x=cogridx2, just=just, rot=a$labels.rot[1], gp=grid::gpar(col=a$labels.col, cex=cex, fontface=a$labels.fontface, fontfamily=a$labels.fontfamily))
	}

	res = grid::gTree(children = gList(ticks, labels), name = "gridTicksLabelsX")

	#res = gTree(children = gList(rectGrob(gp=gpar(fill="red"))), name = "gridTicksLabelsX")

	gt = add_to_gt(gt, res, row = rowid, col = colid)

	gts[[facet_page]] = gt

	assign("gts", gts, envir = .TMAP_GRID)
	NULL
}


tmapGridGridYLab = function(bi, bbx, facet_row, facet_col, facet_page, o) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)
	gt = gts[[facet_page]]

	is_left = o$grid.labels.pos[1] == "left"

	rc_text = frc(facet_row, facet_col)

	rowid = g$rows_facet_ids[facet_row]
	colid = g$cols_facet_ids[facet_col] + ifelse(is_left, -3, 3)

	H = g$rowsIn[rowid]
	W = g$colsIn[colid]


	a = g$grid_comp_per_bbx[[bi]]


	labelsy <- a$labels.y

	# find coordinates for projected grid labels
	if (!is.na(a$crs)) {
		glabelsy <- get_gridline_labels(lco=a$co.y[a$sel.y], yax = 0)
		cogridy <- glabelsy$cogrid
		idsy <- glabelsy$ids
		labelsy <- labelsy[idsy]
	} else {
		cogridy <- a$co.y
	}

	cex <- a$labels.size*o$scale

	# remove overlapping grid labels
	heights = text_height_npc(labelsy) * cex
	sely2 = grid_nonoverlap(cogridy, heights)
	if (!any(sely2)) return(NULL)
	labelsy2 = labelsy[sely2]
	cogridy2 = cogridy[sely2]

	spacerY <- grid::convertWidth(grid::unit(.5, "lines"), unitTo="inch", valueOnly=TRUE) * cex / W
	marginY <- grid::convertWidth(grid::unit(a$labels.margin.y, "lines"), unitTo="inch", valueOnly=TRUE) * cex / W

	if (is_left) {
		just <- ifelse(a$labels.rot[2] == 90, "bottom", ifelse(a$labels.rot[2] == 270, "top", ifelse(a$labels.rot[2] == 180, "left", "right")))
	} else {
		just <- ifelse(a$labels.rot[2] == 90, "bottom", ifelse(a$labels.rot[2] == 270, "top", ifelse(a$labels.rot[2] == 180, "right", "left")))
	}

	if (a$labels.rot[2] %in% c(90, 270)) {
		x = 0.5 - (is_left - 0.5) * 2 * (spacerY + 0.5* marginY)
	} else {
		if (is_left) {
			x = 1 - spacerY - marginY
		} else {
			x = spacerY + marginY
		}
	}

	if (a$ticks[2]) {
		if (is_left) {
			ticks <- grid::polylineGrob(x = rep(c(1-spacerY*.5-marginY, 1), length(cogridy2)), y = rep(cogridy2, each = 2), id = rep(1:length(cogridy2), each = 2), gp=gpar(col=a$col, lwd=a$lwd))
		} else {
			ticks <- grid::polylineGrob(x = rep(c(0, spacerY*.5+marginY), length(cogridy2)), y = rep(cogridy2, each = 2), id = rep(1:length(cogridy2), each = 2), gp=gpar(col=a$col, lwd=a$lwd))
		}
	} else {
		ticks <- NULL
	}

	labels = grid::textGrob(labelsy2, y=cogridy2, x=x,
							just=just, rot=a$labels.rot[2], gp=gpar(col=a$labels.col, cex=cex, fontface=a$labels.fontface, fontfamily=a$labels.fontfamily))
	res = gTree(children = gList(ticks, labels), name = "gridTicksLabelsY")


	gt = add_to_gt(gt, res, row = rowid, col = colid)

	gts[[facet_page]] = gt

	assign("gts", gts, envir = .TMAP_GRID)
	NULL


}

#' @export
#' @keywords internal
#' @rdname tmapGridLeaflet
tmapGridAuxPlot.tm_aux_graticules = function(a, bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	tmapGridAuxPlot.tm_aux_grid(a, bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o)
}

#' @export
#' @keywords internal
#' @rdname tmapGridLeaflet
tmapGridAuxPlot.tm_aux_grid = function(a, bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	rc_text = frc(facet_row, facet_col)

	g = get("g", envir = .TMAP_GRID)

	a = g$grid_comp_per_bbx[[bi]]
	gp = list()

	fH = g$mapRowsIn[facet_row]
	fW = g$mapColsIn[facet_col]


	if (a$labels.inside_frame && any(a$ticks) && o$show.warnings) warning("Grid ticks are not supported when labels.inside_frame = TRUE", call. = FALSE)


	## might be confusing: gridx are grid lines for the x-axis, so they are vertical
	cogridx <- a$co.x
	cogridy <- a$co.y
	labelsx <- a$labels.x
	labelsy <- a$labels.y


	cex <- a$labels.size*o$scale

	selx <- (length(cogridx) > 0)
	sely <- (length(cogridy) > 0)

	# find margins due to grid labels
	if (o$frame) {
		if (o$frame.double_line) {
			fw <- (6 * grid::convertWidth(unit(1, "points"), unitTo = "inch", valueOnly = TRUE) * o$frame.lwd) / fW
			fh <- (6 * grid::convertHeight(unit(1, "points"), unitTo = "inch", valueOnly = TRUE) * o$frame.lwd) / fH
		} else {
			fw <- (grid::convertWidth(unit(1, "points"), unitTo = "inch", valueOnly = TRUE) * o$frame.lwd) / fW
			fh <- (grid::convertHeight(unit(1, "points"), unitTo = "inch", valueOnly = TRUE) * o$frame.lwd) / fH
		}
	} else {
		fw <- 0
		fh <- 0
	}



	if (a$add.labels[1]) {
		if (a$labels.rot[1] %in% c(0, 180)) {
			labelsXw <- if (selx) (max(text_height_inch(labelsx))  * cex + fh) / fH else 0
		} else {
			labelsXw <- if (selx) (max(text_width_inch(labelsx, space = FALSE))  * cex + fh) / fH else 0
		}
		spacerX <- grid::convertHeight(unit(.5, "lines"), unitTo="inch", valueOnly = TRUE) * cex / fH
		marginX <- grid::convertWidth(unit(a$labels.margin.x, "lines"), unitTo="inch", valueOnly = TRUE) * cex / fW
	} else {
		labelsXw <- spacerX <- marginX <- 0
	}


	if (a$add.labels[2]) {
		if (a$labels.rot[2] %in% c(0, 180)) {
			labelsYw <- if (sely) (max(text_width_inch(labelsy, space=FALSE))  * cex + fw) / fW else 0
		} else {
			labelsYw <- if (sely) (max(text_height_inch(labelsy, to_width = TRUE))  * cex + fw) / fW else 0
		}
		spacerY <- grid::convertWidth(grid::unit(.5, "lines"), unitTo="inch", valueOnly=TRUE) * cex / fW
		marginY <- grid::convertWidth(grid::unit(a$labels.margin.y, "lines"), unitTo="inch", valueOnly=TRUE) * cex / fW
	} else {
		labelsYw <- spacerY <- marginY <- 0
	}

	# find coordinates for projected grid labels
	if (!is.na(a$crs)) {
		if (selx) {
			glabelsx <- get_gridline_labels(lco=a$co.x[a$sel.x], xax = labelsXw + spacerX+marginX)
			cogridx <- glabelsx$cogrid
			idsx <- glabelsx$ids
			labelsx <- labelsx[idsx]
			cogridx_frst <- cogridx[sapply(1:max(idsx), function(i) which(i==idsx)[1])]
		}
		# } else {
		# 	glabelsx <- numeric(0)
		# }

		if (sely) {
			glabelsy <- get_gridline_labels(lco=a$co.y[a$sel.y], yax = labelsYw + spacerY+marginY)
			cogridy <- glabelsy$cogrid
			idsy <- glabelsy$ids
			labelsy <- labelsy[idsy]
			cogridy_frst <- cogridy[sapply(1:max(idsy), function(i) which(i==idsy)[1])]
		}

	} else {
		cogridx_frst <- cogridx
		cogridy_frst <- cogridy
	}

	# select grid labels to print
	selx2 <- if (selx) (cogridx >= labelsYw + spacerY + marginY & cogridx <= 1 - spacerY) else selx
	sely2 <- if (sely) (cogridy >= labelsXw + spacerX + marginX & cogridy <= 1 - spacerX) else sely


	# remove overlapping grid labels
	widths = text_width_inch(labelsx) * cex / fW
	heights = text_height_inch(labelsy) * cex / fH


	selx2 = selx2 & grid_nonoverlap(cogridx, widths)
	sely2 = sely2 & grid_nonoverlap(cogridy, heights)


	# select grid lines to draw
	selx <- if (selx) (cogridx_frst >= labelsYw + spacerY + marginY & cogridx_frst <= 1) else selx
	sely <- if (sely) (cogridy_frst >= labelsXw + spacerX + marginX & cogridy_frst <= 1) else sely

	# crop projected grid lines, and extract polylineGrob ingredients
	if (!is.na(a$crs)) {
		lnsList <- list(
			if (any(selx)) sf::st_multilinestring(a$co.x) else NULL,
			if (any(sely)) sf::st_multilinestring(a$co.y) else NULL
		)
		lnsSel <- !vapply(lnsList, is.null, logical(1))
		if (!any(lnsSel)) {
			grid.co.x <- numeric(0)
			grid.co.y <- numeric(0)
		} else {
			lns <- sf::st_sf(ID=c("x", "y")[lnsSel], geometry = sf::st_sfc(lnsList[lnsSel], crs = 4326)) # trick for 0-1 coordinates
			sf_bbox <- tmaptools::bb_poly(bb(c(labelsYw + spacerY + marginY, labelsXw + spacerX + marginX, 1, 1)), projection = 4326)
			lns_crop <- suppressWarnings(suppressMessages(sf::st_intersection(lns, sf_bbox)))

			# quick fix for #564
			if (!all(sf::st_geometry_type(lns_crop) == "MULTILINESTRING")) {
				lns_crop_lns = sf::st_collection_extract(lns_crop, "LINESTRING")
				stopifnot(nrow(lns_crop) == length(lns_crop_lns))
				lns_crop$geometry = sf::st_geometry(lns_crop_lns)
			}

			if (any(selx)) {
				cogridxlns <- as.data.frame(sf::st_coordinates(sf::st_geometry(lns_crop)[1])[,1:3])
				names(cogridxlns) <- c("x", "y", "ID")
			} else {
				cogridxlns <- numeric(0)
			}

			if (any(sely)) {
				cogridylns <- as.data.frame(sf::st_coordinates(sf::st_geometry(lns_crop)[sum(lnsSel)])[,1:3])
				names(cogridylns) <- c("x", "y", "ID")
			} else {
				cogridylns <- numeric(0)
			}
		}

	}

	## process x-axis grid lines and labels
	if (any(selx)) {
		cogridx2 <- cogridx_frst[selx]
		cogridx3 <- cogridx[selx2]
		labelsx <- labelsx[selx2]

		if (!a$lines) {
			grobGridX <- NULL
		} else if (is.na(a$crs)) {
			grobGridX <- grid::polylineGrob(x=rep(cogridx2, each=2), y=rep(c(labelsXw+spacerX+marginX,1), length(cogridx2)),
											id=rep(1:length(cogridx2), each=2), gp=grid::gpar(col=a$col, lwd=a$lwd))
		} else {
			grobGridX <- grid::polylineGrob(x=cogridxlns$x, y=cogridxlns$y, id=cogridxlns$ID, gp=grid::gpar(col=a$col, lwd=a$lwd))
		}

		grobGridTextX <- if (a$add.labels[1] && any(selx2)) {
			just <- ifelse(a$labels.rot[1] == 90, "right", ifelse(a$labels.rot[1] == 270, "left", ifelse(a$labels.rot[1] == 180, "bottom", "top")))

			grid::textGrob(labelsx, y=labelsXw+spacerX*.5+marginX, x=cogridx3, just=just, rot=a$labels.rot[1], gp=grid::gpar(col=a$labels.col, cex=cex, fontface=a$labels.fontface, fontfamily=a$labels.fontfamily))
		} else NULL
	} else {
		grobGridX <- NULL
		grobGridTextX <- NULL
	}


	## process y-axis grid lines and labels
	if (any(sely)) {
		cogridy2 <- cogridy_frst[sely]
		cogridy3 <- cogridy[sely2]
		labelsy <- labelsy[sely2]

		if (!a$lines) {
			grobGridY <- NULL
		} else if (is.na(a$crs)) {
			grobGridY <- grid::polylineGrob(y=rep(cogridy2, each=2), x=rep(c(labelsYw+spacerY+marginY,1), length(cogridy2)),
											id=rep(1:length(cogridy2), each=2), gp=grid::gpar(col=a$col, lwd=a$lwd))
		} else {
			grobGridY <- grid::polylineGrob(x=cogridylns$x, y=cogridylns$y, id=cogridylns$ID, gp=grid::gpar(col=a$col, lwd=a$lwd))
		}

		grobGridTextY <- if (a$add.labels[2] && any(sely2)) {
			just <- ifelse(a$labels.rot[2] == 90, "bottom", ifelse(a$labels.rot[2] == 270, "top", ifelse(a$labels.rot[2] == 180, "left", "right")))

			grid::textGrob(labelsy, x=labelsYw+spacerY*.5+marginY, y=cogridy3, just=just, rot=a$labels.rot[2], gp=grid::gpar(col=a$labels.col, cex=a$labels.size*o$scale, fontface=a$labels.fontface, fontfamily=a$labels.fontfamily))
		} else NULL
	} else {
		grobGridY <- NULL
		grobGridTextY <- NULL
	}
	res = list(treeGridLines=gTree(children=gList(grobGridX, grobGridY), name="grid_lines"),
			   treeGridLabels=gTree(children=gList(grobGridTextX, grobGridTextY), name="grid_labels"),
			   metaX=labelsYw+spacerY,
			   metaY=labelsXw+spacerX)





	grb = grid::grobTree(gList(res$treeGridLines, res$treeGridLabels))

	gts = get("gts", .TMAP_GRID)
	gt = gts[[facet_page]]

	gt_name = paste0("gt_facet_", rc_text)

	gt = grid::addGrob(gt, grb, gPath = grid::gPath(gt_name))

	gts[[facet_page]] = gt
	assign("gts", gts, envir = .TMAP_GRID)
	NULL


}





get_gridline_labels = function(lco, xax = NA, yax = NA) {
	k = length(lco)
	d = ifelse(is.na(xax), 2, 1)


	lns = sf::st_sf(geometry = sf::st_sfc(lapply(lco, function(l) {
		sf::st_linestring(l)
	})), crs = 4326) # trick for 0-1 coordinates


	if (!is.na(xax)) {
		ax = sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(matrix(c(0, 1, xax, xax), nrow=2))), crs = 4326)
		#ax <- SpatialLines(list(Lines(Line(matrix(c(0, 1, xax, xax), nrow=2)), ID="base")))
	} else {
		ax = sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(matrix(c(yax, yax, 0, 1), nrow=2))), crs = 4326)
		#ax <- SpatialLines(list(Lines(Line(matrix(c(yax, yax, 0, 1), nrow=2)), ID="base")))
	}
	gint = suppressMessages(sf::st_intersects(lns, ax, sparse = FALSE, prepared = FALSE))[ ,1]

	ins = vapply(lco, function(m) {
		l = m[1,]
		if (!is.na(xax)) {
			res = l[1] >= 0 && l[1] <= 1 && l[2] >= xax && l[2] <= 1
		} else {
			res = l[1] >= yax && l[1] <= 1 && l[2] >= 0 && l[2] <= 1
		}
		if (res) l[d] else -1
	}, numeric(1))
	cogrid = ifelse(gint, 0, ins)
	ids = seq_along(cogrid) # number of grid labels per grid line (could be 2 for warped grid lines)
	if (any(gint)) {
		cogrid = as.list(cogrid)
		ids = as.list(ids)

		wgint = which(gint)
		gints = suppressMessages(sf::st_intersection(lns[gint, ], ax))

		# count number of intersections per coordinate
		gnrs = vapply(sf::st_geometry(gints), function(g) {
			nr = nrow(g)
			if (is.null(nr)) 1L else nr
		}, integer(1))
		gints = suppressWarnings(sf::st_cast(x = sf::st_cast(gints, "MULTIPOINT"), to = "POINT"))

		coor = sf::st_coordinates(gints)[,d]

		ids2 = unlist(mapply(rep, 1:length(wgint), gnrs, SIMPLIFY = FALSE), use.names = FALSE)
		j = 1L
		for (i in which(gint)) {
			cogrid[[i]] = unname(coor[ids2 == j])
			ids[[i]] = rep(ids[[i]], gnrs[j])
			j = j + 1L
		}

		# remove non-selected items (#938)
		cogrid = cogrid[which(gint)]
		ids = ids[which(gint)]

		cogrid = unlist(cogrid, use.names = FALSE)
		ids = unlist(ids, use.names = FALSE)
	}
	list(cogrid = cogrid, ids = ids)
}
