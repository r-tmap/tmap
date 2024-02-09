rescale_gp = function(gp, scale, skip = character()) {
	if ("lwd" %in% names(gp) && (!"lwd" %in% skip)) gp$lwd = gp$lwd * scale
	if ("size" %in% names(gp) && (!"size" %in% skip)) gp$size = gp$size * sqrt(scale)
	if ("cex" %in% names(gp) && (!"cex" %in% skip)) gp$cex = gp$cex * sqrt(scale)
	gp
}

tmapGridPolygons = function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt
	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)
	
	# none should contain NA's && (length or content should be different)
	diffAlpha = !anyNA(c(gp$fill_alpha, gp$col_alpha)) && !(length(gp$fill_alpha) == length(gp$col_alpha) && all(gp$fill_alpha == gp$col_alpha))
	
	
	if (diffAlpha) {
		gp1 = gp_to_gpar(gp, sel = "fill", o = o, type = "polygons")
		gp2 = gp_to_gpar(gp, sel = "col", o = o, type = "polygons")
		grb1 = sf::st_as_grob(shp, gp = gp1, name = paste0("polygons_", id))
		grb2 = sf::st_as_grob(shp, gp = gp2, name = paste0("polygon_borders_", id))
		grb = grid::grobTree(grb1, grb2)
	} else {
		gp = gp_to_gpar(gp, sel = "all", o = o, type = "polygons")
		grb = sf::st_as_grob(shp, gp = gp, name = paste0("polygons_", id))
	}
	
	
	gts = get("gts", .TMAP_GRID)
	gt = gts[[facet_page]]
	
	gt_name = paste0("gt_facet_", rc_text)
	
	gt = grid::addGrob(gt, grb, gPath = grid::gPath(gt_name))
	
	gts[[facet_page]] = gt
	
	assign("gts", gts, envir = .TMAP_GRID)
	NULL	
}


tmapGridLines = function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	
	rc_text = frc(facet_row, facet_col)

	res = select_sf(shpTM, dt[!is.na(dt$lwd), ])
	shp = res$shp
	dt = res$dt
	
	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)
	
	gp = gp_to_gpar(gp, sel = "col", o = o, type = "lines")
	grb = sf::st_as_grob(shp, gp = gp, name = paste0("lines_", id))

	gts = get("gts", .TMAP_GRID)
	gt = gts[[facet_page]]
	
	gt_name = paste0("gt_facet_", rc_text)
	
	gt = grid::addGrob(gt, grb, gPath = grid::gPath(gt_name))
	
	gts[[facet_page]] = gt
	
	assign("gts", gts, envir = .TMAP_GRID)
	NULL	
}


appendGlist = function(glist, x) {
	glist = grid::gList(glist, x)
	names(glist)[length(glist)] = x$name
	glist
}

swap_pch_15_20 = function(gp) {
	# swap fill and col for pch 15-20
	if (any(gp$shape %in% 15:20)) {
		gp = make_equal_list(gp)
		pch15_20 = which(gp$shape %in% 15:20)
		fill = gp$col[pch15_20]
		fill_alpha = gp$col_alpha[pch15_20]
		gp$col[pch15_20] = gp$fill[pch15_20]
		gp$col_alpha[pch15_20] = gp$fill_alpha[pch15_20]
		gp$fill[pch15_20] = fill
		gp$fill_alpha[pch15_20] = fill_alpha
	}
	gp
}	




tmapGridSymbols = function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt[!is.na(dt$size), ])
	shp = res$shp
	dt = res$dt
	
	gp = impute_gp(gp, dt)
	gp = swap_pch_15_20(gp)	
	
	#gp = get_pch_1000p(gp)
	
	gp = rescale_gp(gp, o$scale_down)

	#gp = gp_to_gpar(gp, sel = "all")
	
	diffAlpha = !any(is.na(c(gp$fill_alpha, gp$col_alpha))) && !(length(gp$fill_alpha) == length(gp$col_alpha) && all(gp$fill_alpha == gp$col_alpha))
	
	coords = sf::st_coordinates(shp)
	
	
	
	if (diffAlpha) {
		gp1 = gp_to_gpar(gp, sel = "fill", o = o, type = "symbols")
		gp2 = gp_to_gpar(gp, sel = "col", o = o, type = "symbols")
		
		grb1 = grid::pointsGrob(x = grid::unit(coords[,1], "native"), y = grid::unit(coords[,2], "native"), pch = gp$shape, size = grid::unit(gp$size, "lines"), gp = gp1, name = paste0("symbols_", id))
		grb2 = grid::pointsGrob(x = grid::unit(coords[,1], "native"), y = grid::unit(coords[,2], "native"), pch = gp$shape, size = grid::unit(gp$size, "lines"), gp = gp2, name = paste0("symbols_borders_", id))
		grb = grid::grobTree(grb1, grb2)
	} else {
		gp = gp_to_gpar(gp, sel = "all", o = o, type = "symbols")
		
		if (any(!is.na(gp$shape) & gp$shape>999)) {
			shapeLib = get("shapeLib", envir = .TMAP)
			justLib = get("justLib", envir = .TMAP)
			
			grobs <- lapply(1:length(gp$shape), function(i) {
				if (!is.na(gp$shape[[i]]) && gp$shape[[i]]>999) {
					grbs <- if (gp$lwd[i] == 0) {
						gList(shapeLib[[gp$shape[[i]]-999]])	
					} else {
						gList(shapeLib[[gp$shape[[i]]-999]], rectGrob(gp=gpar(fill=NA, col=gp$col[i], lwd=gp$lwd[i])))	
					}
					gTree(children=grbs, vp=viewport(x=grid::unit(coords[i,1], "native"), 
													 y=grid::unit(coords[i,2], "native"),
													 width=unit(gp$size[i]*2/3, "lines"),
													 height=unit(gp$size[i]*2/3, "lines")))
				} else {
					pointsGrob(x=grid::unit(coords[i,1], "native"), 
							   y=grid::unit(coords[i,2], "native"),
							   size=unit(gp$size[i], "lines"),
							   pch=gp$shape[[i]],
							   gp=gp)
				}
			})
			grb = gTree(children=do.call(gList, grobs), name=paste0("symbols_", id))
		} else {
			grb = grid::pointsGrob(x = grid::unit(coords[,1], "native"), y = grid::unit(coords[,2], "native"), pch = gp$shape, size = grid::unit(gp$size, "lines"), gp = gp, name = paste0("symbols_", id))
		}
		
		
	}
	

	gts = get("gts", .TMAP_GRID)
	gt = gts[[facet_page]]
	
	gt_name = paste0("gt_facet_", rc_text)
	
	gt = grid::addGrob(gt, grb, gPath = grid::gPath(gt_name))
	
	gts[[facet_page]] = gt
	assign("gts", gts, envir = .TMAP_GRID)
	NULL	
	
}


# zero_one_to_hex = function(x) {
# 	# using indexing
# 	u = unique(x)
# 	
# 	x255 = round(u * 255)
# 	
# 	nc = c(0:9, LETTERS[1:6])
# 	
# 	y1 = (x255 %/% 16) + 1
# 	y2 = (x255 - (y1 - 1) * 16) + 1
# 	
# 	r = paste0(nc[y1], nc[y2])
# 	r[match(x, u)]
# }

# 255 to 2digit hex number
num_to_hex = function(x) {

	nc = c(0:9, LETTERS[1:6])
	
	y1 = (x %/% 16) + 1
	y2 = (x - (y1 - 1) * 16) + 1
	
	paste0(nc[y1], nc[y2])
}

hex_to_num = function(h) {
	nc = c(0:9, LETTERS[1:6])
	y1 = match(substr(h, 1, 1), nc)
	y2 = match(substr(h, 2, 2), nc)
	(y1 - 1) * 16 + (y2 - 1)
}

merge_alpha = function(dt, name) {
	name_a = paste0(name, "_alpha")
	f = function(d) {
		
		d1 = d[1,]
		col = d1[[name]]
		alpha = d1[[name_a]]
		
		if (nchar(col) == 9) {
			a = hex_to_num(substr(col, 8, 9)) * alpha
			cl = substr(col, 1, 7)
		} else {
			a = alpha
			cl = col
		}
		ac = paste0(cl, num_to_hex(round(a*255)))
		rep(ac, nrow(d))
	}
		
	dt[, ca:=f(.SD), by = c(name, name_a), .SDcols = c(name, name_a)]
}



tmapGridRaster <- function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	gts = get("gts", .TMAP_GRID)
	#bbx = get("bbx", .TMAP_GRID)
	
	g = get("g", .TMAP_GRID)
	
	g$fasp
	
	
	rc_text = frc(facet_row, facet_col)
	
	
	bb_target <- bb_asp(bbx, g$fasp)
	bb_real <-  stm_bbox_all(shpTM)
	
	shp = shpTM$shp
	tmapID = shpTM$tmapID
	
	if (is_regular_grid(shp)) {
		
		if (nrow(dt) == length(tmapID)) {
			# shortcut for else case
			
			dt = merge_alpha(dt, name = "col")
			color = dt$ca

			
		} else {
			# to be improved
			tid = intersect(tmapID, dt$tmapID__)
			
			color = rep(NA, length(tmapID)) #"#FFFFFF"
			
			sel = which(tmapID %in% tid)
			tid2 = tmapID[sel]
			dt = merge_alpha(dt, name = "col")
			color[sel] = dt$ca[match(tid2, dt$tmapID__)]
		}
		
		
		if (all(abs(bb_real-bb_target)< 1e-3)) {
			width <- 1
			height <- 1
			cent <- c(mean.default(c(bb_target[1], bb_target[3])), mean.default(c(bb_target[2], bb_target[4])))
		} else {
			width <- (bb_real[3] - bb_real[1]) / (bb_target[3] - bb_target[1])
			height <- (bb_real[4] - bb_real[2]) / (bb_target[4] - bb_target[2])
			cent <- c(mean.default(c(bb_real[1], bb_real[3])), mean.default(c(bb_real[2], bb_real[4])))
		}
		
		cx <- (cent[1] - bb_target[1]) / (bb_target[3] - bb_target[1])
		cy <- (cent[2] - bb_target[2]) / (bb_target[4] - bb_target[2])
		
		m <- matrix(color, ncol=nrow(shp), nrow=ncol(shp), byrow = TRUE)
		
		y_is_pos <- local({
			vals = stars::st_get_dimension_values(shp, "y")
			if (!is.null(vals)) {
				!all(diff(vals) < 0)
			} else {
				rst = attr(shp, "raster")
				name_y = names(get_xy_dim(shp))[2]
				delta = shp[[name_y]]$delta
				delta > 0
			}
		})
			
		if (y_is_pos) {
			m <- m[nrow(m):1L, ]
		}
		m[is.na(m)] = NA #"#0000FF"
		
		grb = grid::rasterGrob(m, x=cx, y=cy, width=width, height=height, interpolate = FALSE, name = paste0("raster_", id)) #gpl$raster.misc$interpolate
		gt = grid::addGrob(gts[[facet_page]], grb, gPath = grid::gPath(paste0("gt_facet_", rc_text)))
		gts[[facet_page]] = gt
		assign("gts", gts, envir = .TMAP_GRID)
	} else {
		m = matrix(tmapID, nrow = nrow(shp), ncol = ncol(shp))
		shp2 = structure(list(tmapID = m), class = "stars", dimensions = shp)
		shpTM = shapeTM(sf::st_geometry(sf::st_as_sf(shp2)), tmapID)

		#dt[, ":="(ord__ = 1, fill = col, fill_alpha = col_alpha, lty = "solid")]
		#dt[, ":="(col_alpha = 0)]
		
		dt[, ":="(ord__ = 1, lty = "solid")]
		
		tmapGridPolygons(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o)
		#grid.shape(s, gp=grid::gpar(fill=color, col=NA), bg.col=NA, i, k)
	}
	NULL
} 

npc_to_native = function(x, scale) {
	x * (scale[2] - scale[1])# + scale[1]
}

tmapGridText = function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt
	
	# specials non-vv (later on lost after gp_to_gpar)
	shadow = gp$shadow
	
	
	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)
	
	coords = sf::st_coordinates(shp)
	
	g = get("g", .TMAP_GRID)
	
	# specials vv (later on lost after gp_to_gpar)
	bgcol = gp$bgcol
	bgcol_alpha = gp$bgcol_alpha
	
	
	gp = gp_to_gpar(gp, sel = "col", o = o, type = "text")

	with_bg = any(bgcol_alpha != 0)
	with_shadow = (!identical(shadow, FALSE))
	
	
	if (with_bg || with_shadow) {
		# grobs are processed seperately because of the order: backgrond1, shadow1, text1, background2, shadow2, text2, etc.
		# becaues it is less efficient when there is no background/shadow (majority of use cases), this is a seperate routine
		
		n = nrow(dt)
		gps = split_gp(gp, n)
		
		grobTextList = mapply(function(txt, x , y, gp) {
			grid::textGrob(x = grid::unit(x, "native"), y = grid::unit(y, "native"), label = txt, gp = gp) #, name = paste0("text_", id))
		}, dt$text, coords[,1], coords[, 2], gps, SIMPLIFY = FALSE, USE.NAMES = FALSE)
		
		
		if (with_bg) {
			tGH = vapply(grobTextList, function(grb) {
				convertHeight(grobHeight(grb), "lines", valueOnly = TRUE)
			}, FUN.VALUE = numeric(1), USE.NAMES = FALSE)
			
			tGW = vapply(grobTextList, function(grb) {
				convertWidth(grobWidth(grb), "lines", valueOnly = TRUE)
			}, FUN.VALUE = numeric(1), USE.NAMES = FALSE)
			
			just = c(0.5, 0.5)
			bg.margin=.3
			
			justx <- .5 - just[1]
			justy <- .6 - just[2]
			
			tGX = unit(coords[,1] + tGW * justx, "native")
			tGY = unit(coords[,2] + tGH * justy, "native")
			
			tGH = unit(tGH + bg.margin, "lines")
			tGW = unit(tGW + bg.margin, "lines")
			
			grobTextBGList = mapply(function(x, y, w, h, b, a) {
				rectGrob(x=x, y=y, width=w, height=h, gp=gpar(fill=b, alpha = a, col=NA))
			}, tGX, tGY, tGW, tGH, bgcol, bgcol_alpha, SIMPLIFY = FALSE, USE.NAMES = FALSE)
		} else {
			grobTextBGList = NULL
		}
		
		
		
		if (with_shadow) {
			gp_sh = gp
			gp_sh$col = ifelse(is_light(gp$col), "#000000", "#FFFFFF")
			grobTextShList = mapply(function(x, y, txt) {
				grid::textGrob(x = grid::unit(x, "native") + grid::unit(0.05, "lines"), y = grid::unit(y, "native") - grid::unit(0.05, "lines"), label = txt, gp = gp_sh)
			}, coords[,1], coords[,2], dt$text, SIMPLIFY = FALSE, USE.NAMES = FALSE)
		} else {
			grobTextShList = NULL
		}

		grobTextAll = list(grobTextBGList, grobTextShList, grobTextList)
		grobTextAll2 = grobTextAll[!vapply(grobTextAll, is.null, FUN.VALUE = logical(1))]
		
		grb = grid::grobTree(do.call(grid::gList, do.call(c, do.call(mapply, c(list(FUN = list, SIMPLIFY = FALSE, USE.NAMES = FALSE), grobTextAll2)))))
		
	} else {
		grobText = grid::textGrob(x = grid::unit(coords[,1], "native"), y = grid::unit(coords[,2], "native"), label = dt$text, gp = gp, name = paste0("text_", id))
		grb = grid::grobTree(gList(grobText))
	}

	
	gts = get("gts", .TMAP_GRID)
	gt = gts[[facet_page]]
	
	gt_name = paste0("gt_facet_", rc_text)
	
	gt = grid::addGrob(gt, grb, gPath = grid::gPath(gt_name))
	
	gts[[facet_page]] = gt
	assign("gts", gts, envir = .TMAP_GRID)
	NULL	
}
