#' @export
#' @rdname tmap_internal
tmapGridText = function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	args = list(...)

	rc_text = frc(facet_row, facet_col)

	if (("prop_angle" %in% names(shpTM))) {
		args$point.label = FALSE
	}

	res = select_sf(shpTM, dt[!is.na(dt$size), ])

	shp = res$shp
	dt = res$dt



	# specials non-vv (later on lost after gp_to_gpar)
	shadow = args$shadow


	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)

	coords = sf::st_coordinates(shp)

	# in case shp is a multipoint (point_per == "segment"), expand gp:
	cp = expand_coords_gp(coords, gp, ndt = nrow(dt))
	coords = cp$coords
	gp = cp$gp

	n = nrow(coords)

	# by default point.label = NA for tm_labels
	# for small n (< 500) it will be set to TRUE
	if (is.na(args$point.label)) args$point.label = (n < 500)

	if (cp$expanded) {
		shpTM_match = match(shpTM$tmapID_expanded, shpTM$tmapID)
		text = dt$text[shpTM_match]
	} else {
		text = dt$text
	}



	g = get("g", .TMAP_GRID)


	# calculate native per line
	wIn = g$colsIn[g$cols_facet_ids[facet_col]]
	hIn = g$rowsIn[g$rows_facet_ids[facet_row]]

	wNative = bbx[3] - bbx[1]
	hNative = bbx[4] - bbx[2]

	xIn = wNative / wIn
	yIn = hNative / hIn

	lineIn = convertHeight(unit(1, "lines"), "inch", valueOnly = TRUE)



	just = process_just(args$just, interactive = FALSE)
	if (args$point.label) {
		if (!all(just == 0.5)) {
			just = c(0.5, 0.5)
			if (get("tmapOptions", envir = .TMAP)$show.messages) message("Point labeling is enabled. Therefore, just will be ignored.")

		}
	}


	# apply xmod and ymod
	coords[,1] = coords[,1] + xIn * lineIn * gp$cex * gp$xmod
	coords[,2] = coords[,2] + yIn * lineIn * gp$cex * gp$ymod


	# specials vv (later on lost after gp_to_gpar)
	bgcol = gp$bgcol
	bgcol_alpha = gp$bgcol_alpha

	angle = gp$angle

	gp = gp_to_gpar(gp, sel = "col", o = o, type = "text")

	with_bg = any(bgcol_alpha != 0)
	with_shadow = (!identical(args$shadow, FALSE))


	if (with_bg || with_shadow || args$remove_overlap || args$point.label) {
		# grobs are processed seperately because of the order: backgrond1, shadow1, text1, background2, shadow2, text2, etc.
		# becaues it is less efficient when there is no background/shadow (majority of use cases), this is a separate routine

		gps = split_gp(gp, n)

		grobTextList = mapply(function(txt, x , y, gp, a) {
			grid::textGrob(x = grid::unit(x, "native"), y = grid::unit(y, "native"), label = txt, gp = gp, rot = a, just = just) #, name = paste0("text_", id))
		}, text, coords[,1], coords[,2], gps, angle, SIMPLIFY = FALSE, USE.NAMES = FALSE)

		if (with_shadow) {
			gp_sh = gp
			gp_sh$col = ifelse(is_light(gp$col), "#000000", "#FFFFFF")
			gps_sh = split_gp(gp_sh, n)
			grobTextShList = mapply(function(x, y, txt, g, a) {
				grid::textGrob(x = grid::unit(x + args$shadow.offset.x * xIn * lineIn, "native"), y = grid::unit(y -  args$shadow.offset.y * yIn * lineIn, "native"), label = txt, gp = g, rot = a)
			}, coords[,1], coords[,2], text, gps_sh, angle, SIMPLIFY = FALSE, USE.NAMES = FALSE)
		} else {
			grobTextShList = NULL
		}

		if (with_bg || args$remove_overlap) {
			tGH = vapply(grobTextList, function(grb) {
				grb$rot = 0
				convertHeight(grobHeight(grb), "inch", valueOnly = TRUE)
			}, FUN.VALUE = numeric(1), USE.NAMES = FALSE) * yIn

			tGW = vapply(grobTextList, function(grb) {
				grb$rot = 0
				convertWidth(grobWidth(grb), "inch", valueOnly = TRUE)
			}, FUN.VALUE = numeric(1), USE.NAMES = FALSE) * xIn

			justx <- .5 - just[1]
			justy <- .5 - just[2]

			#tGX <- grobText$x + unit(tGW * justx, "native")
			#tGY <- grobText$y + unit(tGH * justy, "native")


			tGX = unit(coords[,1] + justx * tGW, "native")
			tGY = unit(coords[,2] + justy * tGH, "native")

			tGH = unit(tGH + args$bg.padding * yIn * lineIn, "native")
			tGW = unit(tGW + args$bg.padding * xIn * lineIn, "native")

			grobTextBGList = mapply(function(x, y, w, h, b, a, rot) {
				rect = rectGrob(x=x, y=y, width=w, height=h, gp=gpar(fill=b, alpha = a, col=NA))
				if (rot != 0) {
					.rectGrob2pathGrob(rect, rot, bbx)$poly
				} else {
					rect
				}
			}, tGX, tGY, tGW, tGH, bgcol, bgcol_alpha, angle, SIMPLIFY = FALSE, USE.NAMES = FALSE)
		} else {
			grobTextBGList = NULL
		}

		#if (args$auto.placement || args$remove_overlap) {
		# grobs to sf
		s = do.call(c,lapply(grobTextBGList, .grob2Poly))
		#}

		if (args$point.label) {
			get_rect_coords = function(polygon) {
				co = sf::st_coordinates(polygon)
				xr = range(co[,1])
				yr = range(co[,2])
				c(x = mean(xr),
				  y = mean(yr),
				  width = xr[2] - xr[1],
				  height = yr[2] - yr[1])
			}

			rect = do.call(rbind, lapply(s, get_rect_coords))

			res = pointLabel2(x = rect[,1], y = rect[,2], width = rect[,3], height = rect[,4], bbx = bbx, gap = yIn * lineIn * args$point.label.gap, method = args$point.label.method)

			sx = res$x - rect[,1]
			sy = res$y - rect[,2]

			grobTextList = mapply(function(grb, sxi, syi) {
				grb$x = grb$x + grid::unit(sxi, "native")
				grb$y = grb$y + grid::unit(syi, "native")
				grb
			}, grobTextList, sx, sy, SIMPLIFY = FALSE)


			grobTextBGList = mapply(function(grb, sxi, syi) {
				grb$x = grb$x + grid::unit(sxi, "native")
				grb$y = grb$y + grid::unit(syi, "native")
				grb
			}, grobTextBGList, sx, sy, SIMPLIFY = FALSE)


			if (with_shadow) {
				grobTextShList = mapply(function(grb, sxi, syi) {
					grb$x = grb$x + grid::unit(sxi, "native")
					grb$y = grb$y + grid::unit(syi, "native")
					grb
				}, grobTextShList, sx, sy, SIMPLIFY = FALSE)
			}
		}

		if (args$remove_overlap) {
			im = sf::st_intersects(s, sparse = FALSE)
			sel = rep(TRUE, length(s))
			rs = rowSums(im)
			while(any(rs>1)) {
				id = which.max(rs)[1]
				sel[id] = FALSE
				im[id,] = FALSE
				im[, id] = FALSE
				rs = rowSums(im)
			}
		} else {
			sel = TRUE
		}

		if (!with_bg && args$remove_overlap) {
			grobTextBGList = NULL
		}



		grobTextAll = list(grobTextBGList[sel], grobTextShList[sel], grobTextList[sel])
		grobTextAll2 = grobTextAll[!vapply(grobTextAll, is.null, FUN.VALUE = logical(1))]

		grb = grid::grobTree(do.call(grid::gList, do.call(c, do.call(mapply, c(list(FUN = list, SIMPLIFY = FALSE, USE.NAMES = FALSE), grobTextAll2)))))

	} else {
		grobText = grid::textGrob(x = grid::unit(coords[,1], "native"), y = grid::unit(coords[,2], "native"), label = text, gp = gp, name = paste0("text_", id), rot = angle)
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

