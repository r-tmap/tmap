#' Label placement: how labels find a spot
#'
#' Specifies how [tm_labels()] places labels. Every label is first placed at its natural spot. For polygons this
#' is the center of the largest inscribed circle (the pole of inaccessibility), where the label is placed if it
#' fits inside the polygon. For points, these are the eight positions around the point, in the order of
#' preference of Imhof (top right, top left, bottom right, bottom left, right, left, top, bottom), where the label
#' is placed if it does not overlap other labels or points. If the label does not fit, the steps of `fallback` are
#' tried in the given order. Labels are placed in the order of their `priority` (see [tm_labels()]).
#'
#' The steps of `fallback`:
#' * `"abbreviate"`: use the short text (the `short` argument of [tm_labels()]), if specified.
#' * `"wrap"`: break the text into two or three lines at spaces.
#' * `"rotate"`: rotate the text along the main axis of the polygon (at most `rotate.max` degrees). Polygons only.
#' * `"shrink"`: decrease the font size down to `shrink.min` times the original size.
#' * `"move"`: place the label outside its polygon, in free space (not on top of polygons or other labels), with
#'   a leader line (see [tm_leader()]). Leader lines do not cross each other or other labels. With `move = "free"`,
#'   labels are moved at most 15 label heights away; labels that do not fit there are tried again, after all other
#'   labels have been placed, at most 25 label heights away. Steps after `"move"` are only tried for labels that cannot
#'   be moved.
#' * `"drop"` or `"overlap"`: the last step, what to do with labels that still do not fit: remove them
#'   (`"drop"`) or keep them at their natural spot with the original size (`"overlap"`). If neither is specified,
#'   `"overlap"` is used.
#'
#' Steps are cumulative: e.g. with `c("wrap", "rotate")`, a rotated label is also tried with wrapped text.
#'
#' Instead of a `tm_placement()` object, the `placement` argument of [opt_tm_labels()] also accepts a preset:
#' `"auto"` (the default, `tm_placement()`), `"inside"` (labels never leave their polygon),
#' `"callout"` (labels that do not fit are moved to a column next to the map), and `"none"`
#' (labels are placed at the natural spot, as with [tm_text()]).
#'
#' In plot mode, all steps are applied. In view mode, labels are placed at their natural spot. Note that moved labels
#' need free space within the map frame; use the `inner.margins` of [tm_layout()] to create space if needed.
#'
#' @param fallback ordered character vector of steps that are tried when a label does not fit. See details.
#' @param rotate.max maximum absolute rotation angle (degrees) of the `"rotate"` step.
#' @param shrink.min smallest font scale factor of the `"shrink"` step.
#' @param move where labels are moved to by the `"move"` step: `"free"` (the nearest free space around the label's
#'   feature), `"column"` (stacked in a column at the left or right, whichever side of the map is nearest), or a
#'   side: `"left"`, `"right"` (stacked in a column), `"top"`, or `"bottom"` (in a row). Labels that do not fit
#'   in a column or row are placed as with `"free"`.
#' @param move.max maximum fraction of labels that are moved by the `"move"` step. If more labels do not fit inside
#'   their polygons (as in maps with many small polygons), no labels are moved, since the many leader lines would
#'   clutter the map. Set it to 1 to move labels regardless.
#' @param gap distance between points and their labels, in line heights. Points only.
#' @param method method to place point labels: `"greedy"` (default) places the labels in the order of their priority
#'   at the first position that is free; `"SANN"` (simulated annealing) and `"GA"` (genetic algorithm) use the
#'   optimization of `car::pointLabel()` (as in tmap 4.0-4.4), in which case `fallback` is not used for points.
#' @return A `tm_placement` object.
#' @seealso [tm_labels()], `r .doc_see_vig("adv_labels")`
#' @example ./examples/tm_placement.R
#' @export
tm_placement = function(fallback = c("wrap", "rotate", "shrink", "move", "drop"),
						rotate.max = 45,
						shrink.min = 0.7,
						move = "free",
						move.max = 0.25,
						gap = 0.4,
						method = "greedy") {
	steps = c("abbreviate", "wrap", "rotate", "shrink", "move", "drop", "overlap")
	if (!is.character(fallback) || !all(fallback %in% steps)) {
		cli::cli_abort("{.arg fallback} should be a character vector with values from {.val {steps}}")
	}
	if (anyDuplicated(fallback)) cli::cli_abort("{.arg fallback} should not contain duplicated steps")
	terminal = intersect(fallback, c("drop", "overlap"))
	if (length(terminal) > 1L || (length(terminal) == 1L && utils::tail(fallback, 1) != terminal)) {
		cli::cli_abort("{.arg fallback} can contain either {.val drop} or {.val overlap}, and only as the last step")
	}
	if (!length(terminal)) fallback = c(fallback, "overlap")
	if (!is.numeric(rotate.max) || length(rotate.max) != 1L || rotate.max < 0 || rotate.max > 90) {
		cli::cli_abort("{.arg rotate.max} should be a number between 0 and 90")
	}
	if (!is.numeric(shrink.min) || length(shrink.min) != 1L || shrink.min <= 0 || shrink.min > 1) {
		cli::cli_abort("{.arg shrink.min} should be a number between 0 (exclusive) and 1")
	}
	move = match.arg(move, c("free", "column", "left", "right", "top", "bottom"))
	if (!is.numeric(move.max) || length(move.max) != 1L || move.max < 0 || move.max > 1) {
		cli::cli_abort("{.arg move.max} should be a number between 0 and 1")
	}
	if (!is.numeric(gap) || length(gap) != 1L || gap < 0) cli::cli_abort("{.arg gap} should be a non-negative number")
	method = match.arg(method, c("greedy", "SANN", "GA"))
	structure(list(fallback = fallback, rotate.max = rotate.max, shrink.min = shrink.min, move = move,
				   move.max = move.max, gap = gap, method = method),
			  class = "tm_placement")
}

#' @export
print.tm_placement = function(x, ...) {
	cat("tm_placement: fallback ", paste(x$fallback, collapse = " > "), "; rotate.max = ", x$rotate.max,
		"; shrink.min = ", x$shrink.min, "; move = \"", x$move, "\"; move.max = ", x$move.max, "; gap = ", x$gap,
		"; method = \"", x$method, "\"\n", sep = "")
	invisible(x)
}

#' Leader lines of labels
#'
#' Specifies the leader lines that connect labels to their features, used by [tm_labels()] for labels that are
#' moved outside (the `"move"` step of [tm_placement()]). Plot mode only.
#'
#' @param col color of the leader lines. `NA` (default) means the color of the text.
#' @param lwd line width.
#' @param lty line type.
#' @param type `"straight"` (default) or `"elbow"` (a short horizontal segment at the label, then straight).
#' @param min.length minimum distance (in line heights) between a label and its feature for which a leader line
#'   is drawn.
#' @return A `tm_leader` object.
#' @seealso [tm_placement()], [tm_labels()], `r .doc_see_vig("adv_labels")`
#' @export
tm_leader = function(col = NA, lwd = 1, lty = "solid", type = "straight", min.length = 0.5) {
	type = match.arg(type, c("straight", "elbow"))
	if (!is.numeric(min.length) || length(min.length) != 1L || min.length < 0) {
		cli::cli_abort("{.arg min.length} should be a non-negative number")
	}
	structure(list(col = col, lwd = lwd, lty = lty, type = type, min.length = min.length), class = "tm_leader")
}

# Messages of label placement
message_placement = function(text, info = NULL, id) {
	if (!isTRUE(get("tmapOptions", envir = .TMAP)$show.messages)) return(invisible(NULL))
	msg = c(paste("{.field [labels]}", text), if (!is.null(info)) c(i = info))
	cli::cli_inform(msg, .frequency = "regularly", .frequency_id = paste0("placement_", id))
	invisible(NULL)
}

# Resolve the placement argument of opt_tm_labels: a preset or a tm_placement object. NULL means no placement.
placement_resolve = function(placement) {
	if (is.null(placement) || inherits(placement, "tm_placement")) return(placement)
	if (!is.character(placement) || length(placement) != 1L) {
		cli::cli_abort("{.arg placement} should be {.fun tm_placement} or one of {.val {c('auto', 'inside', 'callout', 'none')}}")
	}
	switch(placement,
		   auto = tm_placement(),
		   inside = tm_placement(fallback = c("wrap", "rotate", "shrink", "drop")),
		   callout = tm_placement(move = "column"),
		   none = NULL,
		   cli::cli_abort("{.arg placement} should be {.fun tm_placement} or one of {.val {c('auto', 'inside', 'callout', 'none')}}"))
}

# Natural spot of polygon labels: the center of the largest inscribed circle, computed planar on the map
# coordinates. Returns the anchor points (sfc) and the radii.
placement_poly_anchor = function(polys) {
	crs = sf::st_crs(polys)
	g = polys
	sf::st_crs(g) = NA
	res = lapply(g, function(p) {
		bb = sf::st_bbox(p)
		tol = max(bb[3] - bb[1], bb[4] - bb[2]) / 200
		ln = tryCatch(suppressWarnings(sf::st_inscribed_circle(sf::st_sfc(p), dTolerance = tol, nQuadSegs = 0)[[1]]),
					  error = function(e) NULL)
		if (is.null(ln) || sf::st_is_empty(ln)) {
			pt = sf::st_point_on_surface(sf::st_sfc(p))[[1]]
			list(pt = pt, r = 0)
		} else {
			co = unclass(ln)
			list(pt = sf::st_point(co[1, 1:2]), r = sqrt(sum((co[2, 1:2] - co[1, 1:2])^2)))
		}
	})
	list(shp = sf::st_sfc(lapply(res, `[[`, "pt"), crs = crs), r = vapply(res, `[[`, numeric(1), "r"))
}

# Polygons that cross the map frame are clipped to the frame, so that their labels are placed at the center of the
# visible part (e.g. Russia on a map of Europe). Returns the (clipped) polygons, anchors, and radii.
placement_clip_polys = function(polys, anchors, radii, bbx) {
	bb = unname(c(bbx[1], bbx[2], bbx[3], bbx[4]))
	frame = sf::st_as_sfc(sf::st_bbox(c(xmin = bb[1], ymin = bb[2], xmax = bb[3], ymax = bb[4])))
	for (i in seq_along(polys)) {
		p = polys[[i]]
		if (is.null(p)) next
		pb = sf::st_bbox(p)
		if (pb[1] >= bb[1] && pb[2] >= bb[2] && pb[3] <= bb[3] && pb[4] <= bb[4]) next
		cl = tryCatch(suppressWarnings(sf::st_intersection(sf::st_make_valid(sf::st_sfc(p)), frame)), error = function(e) NULL)
		if (is.null(cl) || !length(cl) || all(sf::st_is_empty(cl))) next
		cl = suppressWarnings(sf::st_collection_extract(cl, "POLYGON"))
		if (!length(cl)) next
		cl = sf::st_union(cl)
		pa = placement_poly_anchor(cl)
		polys[[i]] = cl[[1]]
		anchors[i, ] = sf::st_coordinates(pa$shp)[1, 1:2]
		radii[i] = pa$r
	}
	list(polys = polys, anchors = anchors, radii = radii)
}

# Angle (degrees, in (-90, 90]) of the main axis of a polygon (the long side of its minimum rotated rectangle)
placement_main_axis = function(poly) {
	m = tryCatch(sf::st_minimum_rotated_rectangle(sf::st_sfc(poly)), error = function(e) NULL)
	if (is.null(m)) return(0)
	co = sf::st_coordinates(m)[1:3, 1:2]
	s1 = co[2, ] - co[1, ]
	s2 = co[3, ] - co[2, ]
	s = if (sum(s1^2) >= sum(s2^2)) s1 else s2
	a = atan2(s[2], s[1]) * 180 / pi
	if (a > 90) a = a - 180
	if (a <= -90) a = a + 180
	a
}

# Text variants with line breaks at spaces: for 2 and 3 lines, the split with the shortest longest line
placement_wrap_variants = function(txt) {
	if (!is.character(txt) || is.na(txt) || grepl("\n", txt, fixed = TRUE)) return(character(0))
	# break at spaces and after hyphens
	words = strsplit(trimws(txt), "(?<=-)|\\s+", perl = TRUE)[[1]]
	words = words[nzchar(words)]
	nw = length(words)
	if (nw < 2L) return(character(0))
	join = function(w) {
		if (length(w) == 1L) return(w)
		sep = ifelse(grepl("-$", w[-length(w)]), "", " ")
		paste0(c(rbind(w[-length(w)], sep), w[length(w)]), collapse = "")
	}
	vapply(2:min(3L, nw), function(nl) {
		cuts = utils::combn(nw - 1L, nl - 1L, simplify = FALSE)
		best = NULL
		bestw = Inf
		for (ct in cuts) {
			grp = findInterval(seq_len(nw) - 0.5, c(0, ct, nw))
			lns = vapply(split(words, grp), join, character(1))
			w = max(nchar(lns))
			if (w < bestw) {
				bestw = w
				best = lns
			}
		}
		paste(best, collapse = "\n")
	}, character(1))
}

# Edges of a polygon (all rings) as a 4-column matrix x1, y1, x2, y2
placement_edges = function(poly) {
	co = sf::st_coordinates(poly)
	lcols = grep("^L", colnames(co))
	ring = do.call(paste, c(lapply(lcols, function(j) co[, j]), sep = "_"))
	n = nrow(co)
	same = ring[-1] == ring[-n]
	cbind(co[-n, 1][same], co[-n, 2][same], co[-1, 1][same], co[-1, 2][same])
}

# Find a position for a w x h box (rotated by angle degrees) inside a polygon with edges e (see placement_edges), as
# close as possible to anchor (with inscribed radius r). Returns c(x, y) or NULL.
#
# The polygon is rotated around the anchor by -angle, so that the box is horizontal. For horizontal bands of height
# h (nearest to the anchor first), the x ranges where the band crosses the boundary are blocked; the gaps in
# between are either fully inside or fully outside the polygon (tested with one point).
placement_fit_box = function(e, anchor, r, w, h, angle) {
	# a box only fits if a circle of diameter min(w, h) fits
	if (min(w, h) / 2 > r) return(NULL)
	# the box fits in the inscribed circle
	if (sqrt(w^2 + h^2) / 2 <= r) return(anchor)

	th = -angle * pi / 180
	cs = cos(th)
	sn = sin(th)
	x1 = e[, 1] - anchor[1]
	y1 = e[, 2] - anchor[2]
	x2 = e[, 3] - anchor[1]
	y2 = e[, 4] - anchor[2]
	ex1 = x1 * cs - y1 * sn
	ey1 = x1 * sn + y1 * cs
	ex2 = x2 * cs - y2 * sn
	ey2 = x2 * sn + y2 * cs

	xr = range(ex1, ex2)
	yr = range(ey1, ey2)
	if (diff(xr) < w || diff(yr) < h) return(NULL)

	ys = c(0, seq(yr[1] + h / 2, yr[2] - h / 2, length.out = 25))
	ys = ys[order(abs(ys))]

	ylo = pmin(ey1, ey2)
	yhi = pmax(ey1, ey2)

	best = NULL
	bestd = Inf
	for (y in ys) {
		if (abs(y) >= bestd) break
		b0 = y - h / 2
		b1 = y + h / 2
		sel = which(yhi >= b0 & ylo <= b1)
		blocked = if (length(sel)) {
			# clip the edges to the band and take their x ranges
			ax = ex1[sel]; ay = ey1[sel]; bx = ex2[sel]; by = ey2[sel]
			dy = by - ay
			t0 = ifelse(dy == 0, 0, pmin(pmax((b0 - ay) / dy, 0), 1))
			t1 = ifelse(dy == 0, 1, pmin(pmax((b1 - ay) / dy, 0), 1))
			xa = ax + t0 * (bx - ax)
			xb = ax + t1 * (bx - ax)
			cbind(pmin(xa, xb), pmax(xa, xb))
		} else matrix(numeric(0), ncol = 2)
		free = placement_free_intervals(xr, blocked)
		free = free[(free[, 2] - free[, 1]) >= w, , drop = FALSE]
		if (!nrow(free)) next
		# keep gaps that are inside the polygon (even-odd rule at the center line of the band)
		inside = vapply(rowMeans(free), function(px) {
			cr = (ey1 > y) != (ey2 > y)
			xi = ex1[cr] + (y - ey1[cr]) * (ex2[cr] - ex1[cr]) / (ey2[cr] - ey1[cr])
			sum(xi > px) %% 2 == 1
		}, logical(1))
		free = free[inside, , drop = FALSE]
		if (!nrow(free)) next
		xs = pmin(pmax(0, free[, 1] + w / 2), free[, 2] - w / 2)
		x = xs[which.min(abs(xs))]
		d = sqrt(x^2 + y^2)
		if (d < bestd) {
			bestd = d
			best = c(x, y)
		}
	}
	if (is.null(best)) return(NULL)
	# rotate back
	c(best[1] * cs + best[2] * sn, -best[1] * sn + best[2] * cs) + anchor
}

# Free intervals of range xr, given a matrix of blocked intervals
placement_free_intervals = function(xr, blocked) {
	if (!nrow(blocked)) return(matrix(xr, ncol = 2))
	blocked = blocked[order(blocked[, 1]), , drop = FALSE]
	free = list()
	cur = xr[1]
	for (i in seq_len(nrow(blocked))) {
		if (blocked[i, 1] > cur) free[[length(free) + 1L]] = c(cur, blocked[i, 1])
		cur = max(cur, blocked[i, 2])
	}
	if (cur < xr[2]) free[[length(free) + 1L]] = c(cur, xr[2])
	if (!length(free)) return(matrix(numeric(0), ncol = 2))
	do.call(rbind, free)
}

# Fit one polygon label inside its polygon, trying the configurations of the given (inside) steps in order of
# preference. Returns list(x, y, text, cex, angle) or NULL.
placement_fit_label = function(p, e, anchor, r, txt, steps, placement, measure, short = NA) {
	angle = if ("rotate" %in% steps) {
		a = placement_main_axis(p)
		a = max(-placement$rotate.max, min(placement$rotate.max, a))
		if (abs(a) >= 5) a else NA
	} else NA
	cfgs = placement_configs(txt, short, steps, placement, angle = angle)
	sizes = list()
	for (cf in cfgs) {
		wh = sizes[[cf$text]]
		if (is.null(wh)) {
			wh = measure(cf$text)
			sizes[[cf$text]] = wh
		}
		pos = placement_fit_box(e, anchor, r, w = wh[1] * cf$cex, h = wh[2] * cf$cex, angle = cf$angle)
		if (!is.null(pos)) return(list(x = pos[1], y = pos[2], text = cf$text, cex = cf$cex, angle = cf$angle))
	}
	NULL
}

# Configurations (text, angle, cex) of a label, in order of preference, given the (inside) steps. The steps are
# cumulative: each step adds variants of all configurations so far.
placement_configs = function(txt, short, steps, placement, angle = NA) {
	shrink_levels = if ("shrink" %in% steps) {
		unique(c(seq(0.9, placement$shrink.min, by = -0.1), placement$shrink.min))
	} else numeric(0)
	shrink_levels = shrink_levels[shrink_levels < 1]
	has_short = is.character(short) && length(short) == 1L && !is.na(short) && nzchar(short) && short != txt

	cfgs = list(list(text = txt, angle = 0, cex = 1))
	for (st in steps) {
		if (st == "abbreviate" && has_short) {
			cfgs = c(cfgs, lapply(cfgs, function(cf) utils::modifyList(cf, list(text = short))))
		} else if (st == "wrap") {
			cfgs = c(cfgs, unlist(lapply(cfgs, function(cf) {
				lapply(placement_wrap_variants(cf$text), function(v) utils::modifyList(cf, list(text = v)))
			}), recursive = FALSE))
		} else if (st == "rotate" && !is.na(angle)) {
			cfgs = c(cfgs, lapply(cfgs, function(cf) utils::modifyList(cf, list(angle = angle))))
		} else if (st == "shrink") {
			cfgs = c(cfgs, unlist(lapply(shrink_levels, function(f) {
				lapply(cfgs, function(cf) utils::modifyList(cf, list(cex = f)))
			}), recursive = FALSE))
		}
	}
	# remove duplicates (e.g. wrapped variants of the same text)
	key = vapply(cfgs, function(cf) paste(cf$text, cf$angle, cf$cex), character(1))
	cfgs[!duplicated(key)]
}

# Place polygon labels (plot mode).
#   polys:   list of polygon geometries (sfg, map coordinates) of the labels
#   anchors: matrix of anchor coordinates
#   radii:   inscribed circle radii
#   text:    label texts
#   measure: function(txt, i) that returns c(w, h) of the text (including padding) in native units (at the
#            label's own size)
#   bbx:     bounding box of the panel (native units)
#   line:    line height (native units)
#   short:   short texts (for the "abbreviate" step)
#   priority: priority of the labels (labels with a higher priority are moved first); NULL: polygon area
#
# The steps before "move" are tried inside the polygons first. Labels that do not fit are moved outside (if "move"
# is a step); labels that cannot be moved are tried inside again, now also with the steps after "move".
# Returns a list with x, y, text, cex (scale factor), angle, keep, and moved; for moved labels, the leader lines
# (x0, y0, x1, y1).
placement_polygons = function(placement, polys, anchors, radii, text, measure, bbx = NULL, line = NULL,
							  short = rep(NA_character_, length(polys)), priority = NULL) {
	n = length(polys)
	out = list(x = anchors[, 1], y = anchors[, 2], text = text, cex = rep(1, n), angle = rep(0, n),
			   keep = rep(TRUE, n), moved = rep(FALSE, n),
			   leader = matrix(NA_real_, nrow = n, ncol = 4, dimnames = list(NULL, c("x0", "y0", "x1", "y1"))))
	steps = placement$fallback
	terminal = utils::tail(steps, 1)
	steps = setdiff(steps, c("drop", "overlap"))
	im = match("move", steps)
	pre = if (is.na(im)) steps else steps[seq_len(im - 1L)]
	all_inside = setdiff(steps, "move")

	edges = lapply(polys, placement_edges)
	if (is.null(priority)) priority = vapply(polys, function(p) as.numeric(sf::st_area(p)), numeric(1))
	valid = vapply(text, function(txt) is.character(txt) && !is.na(txt) && nzchar(txt), logical(1))

	set_fit = function(i, f) {
		out$x[i] <<- f$x
		out$y[i] <<- f$y
		out$text[i] <<- f$text
		out$cex[i] <<- f$cex
		out$angle[i] <<- f$angle
	}

	# phase 1: inside, with the steps before "move"
	fitted = !valid
	for (i in which(valid)) {
		f = placement_fit_label(polys[[i]], edges[[i]], anchors[i, ], radii[i], text[i], pre, placement,
								function(txt) measure(txt, i), short = short[i])
		if (!is.null(f)) {
			set_fit(i, f)
			fitted[i] = TRUE
		}
	}

	# phase 2: move outside (unless too many labels need to be moved)
	if (!is.na(im) && any(!fitted) && !is.null(bbx)) {
		# only the polygons with their label spot inside the map frame count
		inmap = valid & anchors[, 1] >= bbx[1] & anchors[, 1] <= bbx[3] & anchors[, 2] >= bbx[2] & anchors[, 2] <= bbx[4]
		frac = if (any(inmap)) sum(!fitted & inmap) / sum(inmap) else 0
		if (frac > placement$move.max) {
			message_placement(paste0(round(100 * frac), "% of the labels do not fit inside their polygons, ",
									 "which is too many to move outside (see {.arg move.max} of {.fun tm_placement})"),
							  id = "move_max")
			im = NA
		}
	}
	if (!is.na(im) && any(!fitted) && !is.null(bbx)) {
		ids = which(!fitted)
		wh = t(vapply(ids, function(i) measure(text[i], i), numeric(2)))
		# obstacles: boxes of the labels placed inside (axis-aligned bounding boxes of rotated boxes)
		ids_in = which(fitted & valid)
		obst = if (length(ids_in)) {
			t(vapply(ids_in, function(i) {
				w0 = measure(out$text[i], i) * out$cex[i]
				a = out$angle[i] * pi / 180
				W = abs(w0[1] * cos(a)) + abs(w0[2] * sin(a))
				H = abs(w0[1] * sin(a)) + abs(w0[2] * cos(a))
				c(out$x[i] - W / 2, out$y[i] - H / 2, out$x[i] + W / 2, out$y[i] + H / 2)
			}, numeric(4)))
		} else matrix(numeric(0), ncol = 4)
		mv = placement_move_variants(placement, ids, anchors, text, measure, polys, edges, obst, bbx, line, priority)
		ok = !is.na(mv$x)
		out$x[ids[ok]] = mv$x[ok]
		out$y[ids[ok]] = mv$y[ok]
		out$text[ids[ok]] = mv$text[ok]
		out$moved[ids[ok]] = TRUE
		out$leader[ids[ok], ] = mv$leader[ok, , drop = FALSE]
		fitted[ids[ok]] = TRUE
		if (!all(ok)) {
			nk = sum(!ok)
			message_placement(paste0(nk, ifelse(nk == 1, " label", " labels"), " could not be moved outside: not enough free space ",
									 "near ", ifelse(nk == 1, "its feature", "their features")),
							  info = "Add space around the map with e.g. {.code tm_layout(inner.margins = c(0.02, 0.1, 0.02, 0.1))} (bottom, left, top, right)",
							  id = "no_space")
		}
	}

	# phase 3: inside, with all steps (if there are steps after "move")
	if (!is.na(im) && length(setdiff(all_inside, pre))) {
		for (i in which(!fitted)) {
			f = placement_fit_label(polys[[i]], edges[[i]], anchors[i, ], radii[i], text[i], all_inside, placement,
									function(txt) measure(txt, i), short = short[i])
			if (!is.null(f)) {
				set_fit(i, f)
				fitted[i] = TRUE
			}
		}
	}

	if (terminal == "drop") out$keep[!fitted] = FALSE
	out
}

# Move labels outside (see placement_move), first with their texts; labels that cannot be moved are tried again with
# their texts wrapped in two lines. ids: the labels to move (indices of anchors and text); measure: function(txt, i).
# Returns x, y, leader, text, and wh (the sizes of the moved labels) for the labels ids (NA if not moved).
placement_move_variants = function(placement, ids, anchors, text, measure, polys, edges, obst, bbx, line, priority) {
	n = length(ids)
	res = list(x = rep(NA_real_, n), y = rep(NA_real_, n), leader = matrix(NA_real_, n, 4), text = text[ids],
			   wh = matrix(NA_real_, n, 2))
	leaders = matrix(numeric(0), ncol = 4)
	todo = seq_len(n)
	for (pass in 1:2) {
		if (!length(todo)) break
		txts = if (pass == 1L) text[ids[todo]] else vapply(text[ids[todo]], function(t) {
			v = placement_wrap_variants(t)
			if (length(v)) v[1] else NA_character_
		}, character(1), USE.NAMES = FALSE)
		sel = !is.na(txts)
		todo = todo[sel]
		txts = txts[sel]
		if (!length(todo)) break
		wh = t(vapply(seq_along(todo), function(k) measure(txts[k], ids[todo[k]]), numeric(2)))
		mv = placement_move(placement, anchors[ids[todo], , drop = FALSE], wh, polys, edges, obst, bbx, line,
							pts = anchors[-ids[todo], , drop = FALSE], priority = priority[ids[todo]], leaders = leaders)
		ok = !is.na(mv$x)
		k = todo[ok]
		res$x[k] = mv$x[ok]
		res$y[k] = mv$y[ok]
		res$leader[k, ] = mv$leader[ok, , drop = FALSE]
		res$text[k] = txts[ok]
		res$wh[k, ] = wh[ok, , drop = FALSE]
		if (any(ok)) {
			obst = rbind(obst, placement_rects(mv$x[ok], mv$y[ok], wh[ok, 1], wh[ok, 2]))
			leaders = rbind(leaders, mv$leader[ok, , drop = FALSE])
		}
		todo = todo[!ok]
	}
	res
}

# Rectangles (xmin, ymin, xmax, ymax) of boxes with centers cx, cy and sizes w, h
placement_rects = function(cx, cy, w, h) cbind(cx - w / 2, cy - h / 2, cx + w / 2, cy + h / 2)

# Which rectangles of a overlap with any rectangle of b
placement_rects_overlap = function(a, b) {
	if (!nrow(b) || !nrow(a)) return(rep(FALSE, nrow(a)))
	vapply(seq_len(nrow(a)), function(i) {
		any(a[i, 1] < b[, 3] & a[i, 3] > b[, 1] & a[i, 2] < b[, 4] & a[i, 4] > b[, 2])
	}, logical(1))
}

# Which rectangles intersect any of the polygons (land); land is an sfc without crs
placement_rects_on_land = function(rects, land) {
	if (!nrow(rects)) return(logical(0))
	boxes = sf::st_sfc(lapply(seq_len(nrow(rects)), function(i) {
		r = rects[i, ]
		sf::st_polygon(list(rbind(r[c(1, 2)], r[c(3, 2)], r[c(3, 4)], r[c(1, 4)], r[c(1, 2)])))
	}))
	lengths(sf::st_intersects(boxes, land)) > 0L
}

# Nearest point of a rectangle to point p
placement_rect_nearest = function(rect, p) c(min(max(p[1], rect[1]), rect[3]), min(max(p[2], rect[2]), rect[4]))

# Do segments (x0, y0, x1, y1) of a cross segment s?
placement_segments_cross = function(a, s) {
	if (!nrow(a)) return(0L)
	ccw = function(ax, ay, bx, by, cx, cy) (cx - ax) * (by - ay) - (bx - ax) * (cy - ay)
	d1 = ccw(a[, 1], a[, 2], a[, 3], a[, 4], s[1], s[2])
	d2 = ccw(a[, 1], a[, 2], a[, 3], a[, 4], s[3], s[4])
	d3 = ccw(s[1], s[2], s[3], s[4], a[, 1], a[, 2])
	d4 = ccw(s[1], s[2], s[3], s[4], a[, 3], a[, 4])
	sum((d1 * d2 < 0) & (d3 * d4 < 0))
}

# Does segment s (x0, y0, x1, y1) intersect any of the rectangles (xmin, ymin, xmax, ymax)? (Liang-Barsky clipping)
placement_segment_hits_rects = function(s, rects) {
	if (!nrow(rects)) return(FALSE)
	dx = s[3] - s[1]
	dy = s[4] - s[2]
	t0 = rep(0, nrow(rects))
	t1 = rep(1, nrow(rects))
	clip = function(p, q) {
		# p * t <= q
		if (p == 0) {
			t1[q < 0] <<- -1
		} else {
			r = q / p
			if (p < 0) t0 <<- pmax(t0, r) else t1 <<- pmin(t1, r)
		}
	}
	clip(-dx, s[1] - rects[, 1])
	clip(dx, rects[, 3] - s[1])
	clip(-dy, s[2] - rects[, 2])
	clip(dy, rects[, 4] - s[2])
	any(t0 < t1)
}

# Move labels outside: "column" / side or "free". Labels that cannot be moved get NA coordinates.
#   anchors: matrix of the anchors of the labels to move; wh: matrix of their sizes (w, h)
#   polys, edges: all polygons of the layer (land) and their edges
#   obst: matrix of obstacle rectangles; bbx: panel bounding box; line: line height
#   pts: anchors of the other polygons: a moved label may not cover them (so that it does not hide small features)
placement_move = function(placement, anchors, wh, polys, edges, obst, bbx, line, pts = matrix(numeric(0), ncol = 2),
						  priority = rep(0, nrow(anchors)), leaders = matrix(numeric(0), ncol = 4)) {
	n = nrow(anchors)
	res = list(x = rep(NA_real_, n), y = rep(NA_real_, n),
			   leader = matrix(NA_real_, nrow = n, ncol = 4))
	if (length(polys)) {
		land = sf::st_sfc(polys)
		sf::st_crs(land) = NA
		e = do.call(rbind, edges)
	} else {
		land = NULL
		e = matrix(numeric(0), ncol = 4)
	}
	gap = 0.5 * line
	bb = unname(c(bbx[1], bbx[2], bbx[3], bbx[4]))

	if (placement$move != "free") {
		side = if (placement$move == "column") {
			ifelse(anchors[, 1] - bb[1] < bb[3] - anchors[, 1], "left", "right")
		} else rep(placement$move, n)
		for (sd in unique(side)) {
			ids = which(side == sd)
			cl = placement_move_column(sd, anchors[ids, , drop = FALSE], wh[ids, , drop = FALSE], e, obst, bb, gap)
			if (!is.null(cl)) {
				res$x[ids] = cl$x
				res$y[ids] = cl$y
				res$leader[ids, ] = cl$leader
				obst = rbind(obst, placement_rects(cl$x, cl$y, wh[ids, 1], wh[ids, 2]))
			}
		}
	}

	# free placement (also for labels that could not be placed in a column)
	# labels are moved at most 15 label heights away; labels that cannot be placed are tried again (after all other
	# labels have been placed) at most 25 label heights away
	leaders = rbind(leaders, res$leader[!is.na(res$leader[, 1]), , drop = FALSE])
	pts_all = rbind(pts, anchors)
	angs = seq(0, 2 * pi, length.out = 17)[-17]
	for (fac in c(15, 25)) {
		ids = which(is.na(res$x))
		ids = ids[order(-priority[ids])]
		for (i in ids) {
			w = wh[i, 1]
			h = wh[i, 2]
			maxd = min(0.4 * min(bb[3] - bb[1], bb[4] - bb[2]), fac * h)
			cand = expand.grid(a = angs, d = seq(gap, max(gap, maxd), by = gap))
			ux = cos(cand$a)
			uy = sin(cand$a)
			cx = anchors[i, 1] + ux * cand$d + ux * w / 2
			cy = anchors[i, 2] + uy * cand$d + uy * h / 2
			rects = placement_rects(cx, cy, w, h)
			inpanel = rects[, 1] >= bb[1] & rects[, 3] <= bb[3] & rects[, 2] >= bb[2] & rects[, 4] <= bb[4]
			ok = which(inpanel)
			ok = ok[!placement_rects_overlap(rects[ok, , drop = FALSE], obst)]
			if (length(ok) && nrow(pts_all)) {
				covers = vapply(ok, function(k) {
					any(pts_all[, 1] > rects[k, 1] & pts_all[, 1] < rects[k, 3] & pts_all[, 2] > rects[k, 2] & pts_all[, 2] < rects[k, 4])
				}, logical(1))
				ok = ok[!covers]
			}
			if (!length(ok)) next
			# labels are moved into free space only (not on top of other polygons)
			if (!is.null(land)) ok = ok[!placement_rects_on_land(rects[ok, , drop = FALSE], land)]
			if (!length(ok)) next
			cost = cand$d[ok]
			# penalty for leader lines that cross other leader lines
			# leader lines may not cross other leader lines or labels
			ord = order(cost)
			best = NULL
			for (j in ord) {
				k = ok[j]
				np = placement_rect_nearest(rects[k, ], anchors[i, ])
				sg = c(anchors[i, ], np)
				if (placement_segments_cross(leaders, sg) == 0L && !placement_segment_hits_rects(sg, obst)) {
					best = list(k = k, sg = sg)
					break
				}
			}
			if (is.null(best)) next
			k = best$k
			res$x[i] = cx[k]
			res$y[i] = cy[k]
			res$leader[i, ] = best$sg
			leaders = rbind(leaders, best$sg)
			obst = rbind(obst, rects[k, , drop = FALSE])
		}
	}
	res
}

# 1D layout: positions (centers) of boxes with sizes h, as close as possible to ideal (sorted ascending), without
# overlap (with gap), within [lo, hi]. Clusters of overlapping boxes are centered around their ideal positions.
placement_layout_1d = function(ideal, h, gap, lo, hi) {
	n = length(ideal)
	off = cumsum(c(0, (h[-n] + h[-1]) / 2 + gap)) # offsets of the centers relative to the first center
	groups = as.list(seq_len(n))
	start = function(g) {
		s = mean(ideal[g] - off[g] + off[g[1]])
		s = max(s, lo + h[g[1]] / 2)
		s = min(s, hi - (off[g[length(g)]] - off[g[1]]) - h[g[length(g)]] / 2)
		s
	}
	repeat {
		st = vapply(groups, start, numeric(1))
		merged = FALSE
		if (length(groups) > 1L) for (k in 2:length(groups)) {
			g0 = groups[[k - 1L]]
			g1 = groups[[k]]
			end0 = st[k - 1L] + off[g0[length(g0)]] - off[g0[1]] + h[g0[length(g0)]] / 2 + gap
			if (st[k] - h[g1[1]] / 2 < end0 - 1e-12) {
				groups[[k - 1L]] = c(g0, g1)
				groups[[k]] = NULL
				merged = TRUE
				break
			}
		}
		if (!merged) break
	}
	st = vapply(groups, start, numeric(1))
	pos = numeric(n)
	for (k in seq_along(groups)) {
		g = groups[[k]]
		pos[g] = st[k] + off[g] - off[g[1]]
	}
	pos
}

# Labels in a column next to the land (side "left" or "right") or in a row ("top", "bottom"). Computed in a frame
# in which the column is at the right; returns NULL if the labels do not fit.
placement_move_column = function(side, anchors, wh, e, obst, bb, gap) {
	# to the canonical frame (column at the right, stacked along y)
	fwd = switch(side,
				 right = function(x, y) cbind(x, y),
				 left = function(x, y) cbind(-x, y),
				 top = function(x, y) cbind(y, x),
				 bottom = function(x, y) cbind(-y, x))
	bwd = switch(side,
				 right = function(x, y) cbind(x, y),
				 left = function(x, y) cbind(-x, y),
				 top = function(x, y) cbind(y, x),
				 bottom = function(x, y) cbind(y, -x))
	swap = side %in% c("top", "bottom")
	W = if (swap) wh[, 2] else wh[, 1]
	H = if (swap) wh[, 1] else wh[, 2]
	A = fwd(anchors[, 1], anchors[, 2])
	p1 = fwd(e[, 1], e[, 2])
	p2 = fwd(e[, 3], e[, 4])
	cb = fwd(bb[c(1, 3)], bb[c(2, 4)])
	xr = range(cb[, 1])
	yr = range(cb[, 2])
	if (nrow(obst)) {
		o1 = fwd(obst[, 1], obst[, 2])
		o2 = fwd(obst[, 3], obst[, 4])
		ob = cbind(pmin(o1[, 1], o2[, 1]), pmin(o1[, 2], o2[, 2]), pmax(o1[, 1], o2[, 1]), pmax(o1[, 2], o2[, 2]))
	} else ob = matrix(numeric(0), ncol = 4)

	ord = order(A[, 2])
	m = length(ord)
	maxw = max(W)
	if (max(A[, 1]) + gap > xr[2] - maxw) return(NULL)
	x0s = seq(max(A[, 1]) + gap, xr[2] - maxw, length.out = 20)
	# penalty for a leader line that crosses a label
	pen = diff(xr) + diff(yr)

	# cost of a group of labels (positions a..b in ord) in free interval iv at x0: total leader length (+ penalties)
	group_layout = function(a, b, iv, x0) {
		idx = ord[a:b]
		if (sum(H[idx]) + gap * (b - a) > iv[2] - iv[1]) return(NULL)
		ys = placement_layout_1d(A[idx, 2], H[idx], gap, iv[1], iv[2])
		cost = sum(sqrt((x0 - A[idx, 1])^2 + (ys - A[idx, 2])^2))
		if (nrow(ob)) {
			hits = sum(vapply(seq_along(idx), function(j) {
				placement_segment_hits_rects(c(A[idx[j], ], x0, ys[j]), ob)
			}, logical(1)))
			cost = cost + pen * hits
		}
		list(ys = ys, cost = cost)
	}

	best = NULL
	bestc = Inf
	for (x0 in x0s) {
		x1 = x0 + maxw
		# free y intervals of the strip [x0, x1]: blocked by land edges, land (gaps inside), and obstacles
		sel = which(pmax(p1[, 1], p2[, 1]) >= x0 & pmin(p1[, 1], p2[, 1]) <= x1)
		blocked = if (length(sel)) {
			ax = p1[sel, 1]; ay = p1[sel, 2]; bx = p2[sel, 1]; by = p2[sel, 2]
			dx = bx - ax
			t0 = ifelse(dx == 0, 0, pmin(pmax((x0 - ax) / dx, 0), 1))
			t1 = ifelse(dx == 0, 1, pmin(pmax((x1 - ax) / dx, 0), 1))
			ya = ay + t0 * (by - ay)
			yb = ay + t1 * (by - ay)
			cbind(pmin(ya, yb), pmax(ya, yb))
		} else matrix(numeric(0), ncol = 2)
		if (nrow(ob)) {
			so = ob[, 1] < x1 & ob[, 3] > x0
			blocked = rbind(blocked, ob[so, c(2, 4), drop = FALSE])
		}
		free = placement_free_intervals(yr, blocked)
		if (!nrow(free)) next
		# remove gaps inside land (even-odd rule on a vertical line at the strip center)
		xm = (x0 + x1) / 2
		inland = vapply(rowMeans(free), function(py) {
			cr = (p1[, 1] > xm) != (p2[, 1] > xm)
			yi = p1[cr, 2] + (xm - p1[cr, 1]) * (p2[cr, 2] - p1[cr, 2]) / (p2[cr, 1] - p1[cr, 1])
			sum(yi > py) %% 2 == 1
		}, logical(1))
		free = free[!inland & (free[, 2] - free[, 1]) >= min(H), , drop = FALSE]
		K = nrow(free)
		if (!K) next

		# dynamic programming: the labels (in the order of their anchors) are split into consecutive groups, each
		# group is placed in a free interval (intervals in ascending order); dp[i + 1, k + 1] is the minimal cost of
		# the first i labels in the first k intervals
		dp = matrix(Inf, m + 1L, K + 1L)
		dp[1, ] = 0
		back = array(list(NULL), dim = c(m + 1L, K + 1L))
		for (k in seq_len(K)) {
			for (i in 0:m) {
				dp[i + 1L, k + 1L] = dp[i + 1L, k]
				back[[i + 1L, k + 1L]] = list(skip = TRUE)
				if (i == 0L) next
				for (j in (i - 1L):0) {
					if (!is.finite(dp[j + 1L, k])) next
					g = group_layout(j + 1L, i, free[k, ], x0)
					if (is.null(g)) break # larger groups do not fit either
					c2 = dp[j + 1L, k] + g$cost
					if (c2 < dp[i + 1L, k + 1L]) {
						dp[i + 1L, k + 1L] = c2
						back[[i + 1L, k + 1L]] = list(skip = FALSE, j = j, ys = g$ys, iv = free[k, ])
					}
				}
			}
		}
		if (dp[m + 1L, K + 1L] < bestc) {
			bestc = dp[m + 1L, K + 1L]
			# trace back the groups
			groups = list()
			i = m
			k = K
			while (i > 0L) {
				bk = back[[i + 1L, k + 1L]]
				if (!bk$skip) {
					groups[[length(groups) + 1L]] = list(a = bk$j + 1L, b = i, ys = bk$ys, iv = bk$iv)
					i = bk$j
				}
				k = k - 1L
			}
			best = list(x0 = x0, groups = groups)
		}
	}
	if (is.null(best)) return(NULL)

	# untangle per group: swap labels of which the leader lines cross (this shortens the total leader length)
	y = numeric(m)
	for (g in best$groups) {
		o = ord[g$a:g$b]
		ys = g$ys
		mg = length(o)
		if (mg > 1L) for (iter in seq_len(2L * mg^2)) {
			segs = cbind(A[o, 1], A[o, 2], best$x0, ys)
			pair = NULL
			for (k in seq_len(mg - 1L)) {
				for (l in (k + 1L):mg) {
					if (placement_segments_cross(segs[l, , drop = FALSE], segs[k, ]) > 0L) {
						pair = c(k, l)
						break
					}
				}
				if (!is.null(pair)) break
			}
			if (is.null(pair)) break
			o[pair] = o[rev(pair)]
			ys = placement_layout_1d(A[o, 2], H[o], gap, g$iv[1], g$iv[2])
		}
		y[o] = ys
	}

	cx = best$x0 + W / 2
	ctr = bwd(cx, y)
	near = bwd(rep(best$x0, m), y)
	list(x = ctr[, 1], y = ctr[, 2], leader = cbind(anchors, near))
}

# Place point labels (plot mode), greedy in the order of priority.
#   anchors: matrix of point coordinates; text, short: (short) texts; priority: NULL means the text size
#   measure: function(txt, i) that returns c(w, h) (including padding) in native units
#   cex:     text sizes (used for the default priority)
# Returns a list as placement_polygons.
placement_points = function(placement, anchors, text, measure, bbx, line, short = rep(NA_character_, nrow(anchors)),
							priority = NULL, cex = rep(1, nrow(anchors))) {
	n = nrow(anchors)
	out = list(x = anchors[, 1], y = anchors[, 2], text = text, cex = rep(1, n), angle = rep(0, n),
			   keep = rep(TRUE, n), moved = rep(FALSE, n),
			   leader = matrix(NA_real_, nrow = n, ncol = 4, dimnames = list(NULL, c("x0", "y0", "x1", "y1"))))
	if (!n) return(out)
	if (is.null(priority)) priority = cex
	steps = placement$fallback
	terminal = utils::tail(steps, 1)
	steps = setdiff(steps, c("drop", "overlap", "rotate"))
	im = match("move", steps)
	pre = if (is.na(im)) steps else steps[seq_len(im - 1L)]
	all_steps = setdiff(steps, "move")
	bb = unname(c(bbx[1], bbx[2], bbx[3], bbx[4]))
	gap = placement$gap * line
	valid = vapply(text, function(txt) is.character(txt) && !is.na(txt) && nzchar(txt), logical(1))

	# points are obstacles (a label may not cover another point)
	pr = 0.15 * line
	obst = placement_rects(anchors[, 1], anchors[, 2], 2 * pr, 2 * pr)
	ord = order(-priority)

	# Imhof's positions: top right, top left, bottom right, bottom left, right, left, top, bottom
	pos = rbind(c(1, 1), c(-1, 1), c(1, -1), c(-1, -1), c(1, 0), c(-1, 0), c(0, 1), c(0, -1))

	try_place = function(i, stps) {
		cfgs = placement_configs(text[i], short[i], stps, placement)
		for (cf in cfgs) {
			wh = measure(cf$text, i) * cf$cex
			dg = gap / sqrt(2) # diagonal positions: the corner of the box at distance gap
			dx = ifelse(pos[, 1] == 0, 0, pos[, 1] * (wh[1] / 2 + ifelse(pos[, 2] == 0, gap, dg)))
			dy = ifelse(pos[, 2] == 0, 0, pos[, 2] * (wh[2] / 2 + ifelse(pos[, 1] == 0, gap, dg)))
			rects = placement_rects(anchors[i, 1] + dx, anchors[i, 2] + dy, wh[1], wh[2])
			inpanel = rects[, 1] >= bb[1] & rects[, 3] <= bb[3] & rects[, 2] >= bb[2] & rects[, 4] <= bb[4]
			free = inpanel & !placement_rects_overlap(rects, obst[-i, , drop = FALSE])
			if (any(free)) {
				k = which(free)[1]
				return(list(x = anchors[i, 1] + dx[k], y = anchors[i, 2] + dy[k], text = cf$text, cex = cf$cex,
							rect = rects[k, ]))
			}
		}
		NULL
	}
	set_fit = function(i, f) {
		out$x[i] <<- f$x
		out$y[i] <<- f$y
		out$text[i] <<- f$text
		out$cex[i] <<- f$cex
		obst <<- rbind(obst, f$rect)
	}

	fitted = !valid
	for (i in ord[valid[ord]]) {
		f = try_place(i, pre)
		if (!is.null(f)) {
			set_fit(i, f)
			fitted[i] = TRUE
		}
	}

	# move
	if (!is.na(im) && any(!fitted)) {
		# only the points inside the map frame count (e.g. not the cities of the world on a map of Europe)
		inmap = valid & anchors[, 1] >= bb[1] & anchors[, 1] <= bb[3] & anchors[, 2] >= bb[2] & anchors[, 2] <= bb[4]
		frac = if (any(inmap)) sum(!fitted & inmap) / sum(inmap) else 0
		if (frac > placement$move.max) {
			message_placement(paste0(round(100 * frac), "% of the labels do not fit next to their points, ",
									 "which is too many to move (see {.arg move.max} of {.fun tm_placement})"),
							  id = "move_max_points")
			im = NA
		}
	}
	if (!is.na(im) && any(!fitted)) {
		ids = which(!fitted)
		wh = t(vapply(ids, function(i) measure(text[i], i), numeric(2)))
		mv = placement_move_variants(placement, ids, anchors, text, measure, NULL, NULL, obst, bbx, line, priority)
		ok = !is.na(mv$x)
		out$x[ids[ok]] = mv$x[ok]
		out$y[ids[ok]] = mv$y[ok]
		out$text[ids[ok]] = mv$text[ok]
		out$moved[ids[ok]] = TRUE
		out$leader[ids[ok], ] = mv$leader[ok, , drop = FALSE]
		fitted[ids[ok]] = TRUE
		if (any(ok)) obst = rbind(obst, placement_rects(mv$x[ok], mv$y[ok], mv$wh[ok, 1], mv$wh[ok, 2]))
	}
	if (!is.na(im) && length(setdiff(all_steps, pre))) {
		for (i in ord[!fitted[ord]]) {
			f = try_place(i, all_steps)
			if (!is.null(f)) {
				set_fit(i, f)
				fitted[i] = TRUE
			}
		}
	}

	# labels that do not fit: dropped, or (overlap) at the first position
	for (i in which(!fitted)) {
		if (terminal == "drop") {
			out$keep[i] = FALSE
		} else {
			wh = measure(text[i], i)
			out$x[i] = anchors[i, 1] + wh[1] / 2 + gap / sqrt(2)
			out$y[i] = anchors[i, 2] + wh[2] / 2 + gap / sqrt(2)
		}
	}
	out
}
