#' @export
#' @rdname tmapGridLeaflet
tmapGridDataPlot.tm_data_text = function(a, shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, glid, o, ...) {
	rc_text = frc(facet_row, facet_col)

	if (("prop_angle" %in% names(shpTM))) {
		a$point.label = FALSE
	}

	res = select_sf(shpTM, dt[!is.na(dt$size), ])

	shp = res$shp
	dt = res$dt



	# specials non-vv (later on lost after gp_to_gpar)
	shadow = a$shadow


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
	if (is.na(a$point.label)) a$point.label = (n < 500)

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



	just = process_just(a$just, interactive = FALSE)
	if (a$point.label) {
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

	grbLeader = NULL

	# label placement (tm_labels, see tm_placement): polygons, and points (method "greedy")
	if (!is.null(a$placement) && !cp$expanded && n > 0L) {
		is_poly = if ("poly" %in% names(dt)) !vapply(dt$poly, is.null, logical(1)) else rep(FALSE, n)
		is_line = "angle" %in% names(dt) && ("prop_angle" %in% names(shpTM))
		pmode = if (any(is_poly)) {
			"polygons"
		} else if (!is_line && a$placement$method == "greedy") {
			"points"
		} else "none"
		if (pmode == "none" && !is_line) {
			# point labels with the optimization of car::pointLabel
			a$point.label.method = a$placement$method
			a$point.label.gap = a$placement$gap
		}
		ids = if (pmode == "polygons") which(is_poly) else if (pmode == "points") seq_len(n) else integer(0)
		lab_short = if ("short" %in% names(dt)) as.character(dt$short) else rep(NA_character_, n)
		lab_short[!is.na(lab_short) & !nzchar(lab_short)] = NA
		lab_prio = if ("priority" %in% names(dt) && length(unique(dt$priority)) > 1L) as.numeric(dt$priority) else NULL
		if (length(ids)) {
			a$point.label = FALSE
			just = c(0.5, 0.5)
			gp$cex = rep_len(gp$cex, n)
			angle = rep_len(angle, n)
			gps_m = split_gp(gp, n)
			measure = function(txt, i) {
				g = grid::textGrob(label = txt, gp = gps_m[[ids[i]]])
				pad = 0.25 * lineIn * gps_m[[ids[i]]]$cex
				c((convertWidth(grobWidth(g), "inch", valueOnly = TRUE) + pad) * xIn,
				  (convertHeight(grobHeight(g), "inch", valueOnly = TRUE) + pad) * yIn)
			}
			if (pmode == "polygons") {
				# polygons that cross the map frame: labels at the center of the visible part
				pc = placement_clip_polys(dt$poly[ids], coords[ids, 1:2, drop = FALSE], dt$poly_r[ids], bbx)
				coords[ids, 1:2] = pc$anchors
			}
			pl = if (pmode == "polygons") {
				placement_polygons(a$placement, polys = pc$polys, anchors = pc$anchors,
								   radii = pc$radii, text = text[ids], measure = function(txt, i) measure(txt, i),
								   bbx = bbx, line = lineIn * yIn, short = lab_short[ids], priority = lab_prio[ids])
			} else {
				placement_points(a$placement, anchors = coords[ids, 1:2, drop = FALSE], text = text[ids],
								 measure = function(txt, i) measure(txt, i), bbx = bbx, line = lineIn * yIn,
								 short = lab_short[ids], priority = lab_prio[ids], cex = gp$cex[ids])
			}

			# moved labels with text color "auto": contrast with the background instead of the polygon fill
			if (any(pl$moved) && "col_auto__" %in% names(dt)) {
				ia = which(pl$moved & dt$col_auto__[ids])
				if (length(ia)) {
					gp$col = rep_len(gp$col, n)
					gp$col[ids[ia]] = auto_text_col_bg(o, rep_len(bgcol, n)[ids[ia]], rep_len(bgcol_alpha, n)[ids[ia]])
				}
			}

			# leader lines of moved labels
			if (any(pl$moved) && !isFALSE(a$leader)) {
				ld = a$leader
				if (is.null(ld)) ld = tm_leader()
				im = which(pl$moved)
				L = pl$leader[im, , drop = FALSE]
				len = sqrt((L[, 3] - L[, 1])^2 + (L[, 4] - L[, 2])^2)
				show = len >= ld$min.length * lineIn * yIn
				im = im[show]
				L = L[show, , drop = FALSE]
				if (length(im)) {
					lcol = if (is.na(ld$col)) rep_len(gp$col, n)[ids[im]] else ld$col
					if (ld$type == "elbow") {
						# a short horizontal segment at the label (if the leader ends at its left or right edge)
						wbox = vapply(im, function(i) measure(pl$text[i], i)[1], numeric(1))
						dxs = L[, 3] - pl$x[im]
						side = ifelse(abs(abs(dxs) - wbox / 2) < 1e-9 * max(1, abs(dxs)), sign(dxs), 0)
						ex = L[, 3] + side * 0.5 * lineIn * xIn
						xs = rbind(L[, 1], ex, L[, 3])
						ys = rbind(L[, 2], L[, 4], L[, 4])
						grbLeader = grid::polylineGrob(x = grid::unit(as.vector(xs), "native"), y = grid::unit(as.vector(ys), "native"),
													   id = rep(seq_along(im), each = 3),
													   gp = grid::gpar(col = lcol, lwd = ld$lwd * o$scale_down, lty = ld$lty))
					} else {
						grbLeader = grid::segmentsGrob(x0 = grid::unit(L[, 1], "native"), y0 = grid::unit(L[, 2], "native"),
													   x1 = grid::unit(L[, 3], "native"), y1 = grid::unit(L[, 4], "native"),
													   gp = grid::gpar(col = lcol, lwd = ld$lwd * o$scale_down, lty = ld$lty))
					}
				}
			}

			coords[ids, 1] = pl$x
			coords[ids, 2] = pl$y
			text[ids] = pl$text
			gp$cex[ids] = gp$cex[ids] * pl$cex
			if (pmode == "polygons") angle[ids] = pl$angle

			keep = rep(TRUE, n)
			keep[ids] = pl$keep
			if (!all(keep)) {
				coords = coords[keep, , drop = FALSE]
				text = text[keep]
				angle = angle[keep]
				bgcol = rep_len(bgcol, n)[keep]
				bgcol_alpha = rep_len(bgcol_alpha, n)[keep]
				gp = structure(lapply(unclass(gp), function(x) if (length(x) == n) x[keep] else x), class = "gpar")
				n = sum(keep)
			}
		}
	}

	with_bg = any(bgcol_alpha != 0)
	with_shadow_halo = a$shadow || a$halo


	if (with_bg || with_shadow_halo || a$remove_overlap || a$point.label) {
		# grobs are processed seperately because of the order: backgrond1, shadow1, text1, background2, shadow2, text2, etc.
		# becaues it is less efficient when there is no background/shadow (majority of use cases), this is a separate routine

		gps = split_gp(gp, n)

		# decode (possibly) expression-encoded labels; each element is either a
		# character string or a language object, both accepted by textGrob()
		text_dec = lapply(text, decode_expr)

		grobTextList = mapply(function(txt, x , y, gp, a) {
			grid::textGrob(x = grid::unit(x, "native"), y = grid::unit(y, "native"), label = txt, gp = gp, rot = a, just = just) #, name = paste0("text_", id))
		}, text_dec, coords[,1], coords[,2], gps, angle, SIMPLIFY = FALSE, USE.NAMES = FALSE)

		if (with_shadow_halo) {
			gp_sh = gp

			if (a$halo) {
				gp_sh$col = ifelse(is.na(a$halo.col), ifelse(is_light(gp$col), "#000000", "#FFFFFF"), a$halo.col)
			} else {
				gp_sh$col = ifelse(is.na(a$shadow.col), ifelse(is_light(gp$col), "#000000", "#FFFFFF"), a$shadow.col)
			}
			gps_sh = split_gp(gp_sh, n)
			grobTextShList = mapply(function(x, y, txt, g, ai) {
				if (a$halo) {
					offsets <- rbind(
						c(-1, 0),
						c( 1, 0),
						c( 0,-1),
						c( 0, 1)
					)

					diag_offsets <- rbind(
						c(-1,-1),
						c( 1,-1),
						c(-1, 1),
						c( 1, 1)
					)

					# allow smaller diagonal distance for rounder halo
					diag_offsets <- diag_offsets * .5 * sqrt(2)

					offsets <- rbind(offsets, diag_offsets)

					grid::textGrob(x = grid::unit(x + a$halo.width * xIn * lineIn * g$cex * offsets[,1], "native"),
								   y = grid::unit(y -  a$halo.width * yIn * lineIn * g$cex * offsets[,2], "native"),
								   label = txt,
								   gp = g,
								   rot = ai,
								   just = just)
				} else {
					grid::textGrob(x = grid::unit(x + a$shadow.offset.x * xIn * lineIn * g$cex, "native"),
								   y = grid::unit(y -  a$shadow.offset.y * yIn * lineIn * g$cex, "native"),
								   label = txt,
								   gp = g,
								   rot = ai,
								   just = just)
				}
			}, coords[,1], coords[,2], text_dec, gps_sh, angle, SIMPLIFY = FALSE, USE.NAMES = FALSE)
		} else {
			grobTextShList = NULL
		}

		if (with_bg || a$remove_overlap) {
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

			tGH = unit(tGH + (0.1 + a$bg.padding) * yIn * lineIn * gp$cex, "native")
			tGW = unit(tGW + a$bg.padding * xIn * lineIn * gp$cex, "native")

			if (a$bg.border) {
				bordercol = a$bg.border.col
				lwd = a$bg.border.lwd
			} else {
				bordercol = NA
				lwd = 0
			}

			grobTextBGList = mapply(function(x, y, w, h, b, a, rot) {
				rect = rectGrob(x=x, y=y, width=w, height=h, gp=gpar(fill=b, alpha = a, col=bordercol, lwd = lwd))
				if (rot != 0) {
					.rectGrob2pathGrob(rect, rot, bbx)$poly
				} else {
					rect
				}
			}, tGX, tGY, tGW, tGH, bgcol, bgcol_alpha, angle, SIMPLIFY = FALSE, USE.NAMES = FALSE)
		} else {
			grobTextBGList = NULL
		}

		#if (a$auto.placement || a$remove_overlap) {
		# grobs to sf
		s = do.call(c,lapply(grobTextBGList, .grob2Poly))
		#}

		if (a$point.label) {
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

			res = pointLabel2(x = rect[,1], y = rect[,2], width = rect[,3], height = rect[,4], bbx = bbx, gap = yIn * lineIn * a$point.label.gap, method = a$point.label.method)

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


			if (with_shadow_halo) {
				grobTextShList = mapply(function(grb, sxi, syi) {
					grb$x = grb$x + grid::unit(sxi, "native")
					grb$y = grb$y + grid::unit(syi, "native")
					grb
				}, grobTextShList, sx, sy, SIMPLIFY = FALSE)
			}
		}

		if (a$remove_overlap) {
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

		if (!with_bg && a$remove_overlap) {
			grobTextBGList = NULL
		}



		grobTextAll = list(grobTextBGList[sel], grobTextShList[sel], grobTextList[sel])
		grobTextAll2 = grobTextAll[!vapply(grobTextAll, is.null, FUN.VALUE = logical(1))]

		grb = grid::grobTree(do.call(grid::gList, do.call(c, do.call(mapply, c(list(FUN = list, SIMPLIFY = FALSE, USE.NAMES = FALSE), grobTextAll2)))))

	} else {
		grobText = grid::textGrob(x = grid::unit(coords[,1], "native"), y = grid::unit(coords[,2], "native"), label = decode_expr_vec(text), gp = gp, name = paste0("text_", id), rot = angle)
		grb = grid::grobTree(gList(grobText))
	}


	if (!is.null(grbLeader)) grb = grid::grobTree(grbLeader, grb)

	gts = get("gts", .TMAP_GRID)
	gt = gts[[facet_page]]

	gt_name = paste0("gt_facet_", rc_text)

	gt = grid::addGrob(gt, grb, gPath = grid::gPath(gt_name))



	gts[[facet_page]] = gt
	assign("gts", gts, envir = .TMAP_GRID)
	NULL
}

#' @export
#' @rdname tmapGridLeaflet
tmapGridDataPlot.tm_data_labels = function(a, shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, glid, o, ...) {
	NextMethod()
}



#' @export
#' @rdname tmapGridLeaflet
tmapGridDataPlot.tm_data_labels_highlighted = function(a, shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, glid, o, ...) {
	NextMethod()
}
