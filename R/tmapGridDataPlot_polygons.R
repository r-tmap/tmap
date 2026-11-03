#' @export
#' @rdname tmapGridLeaflet
tmapGridDataPlot.tm_data_polygons = function(a, shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, glid, o, ...) {

	rc_text = frc(facet_row, facet_col)

	res = select_sf(shpTM, dt)
	shp = res$shp

	shp_is_pointer = inherits(shp, "character")

	if (shp_is_pointer) {
		cli::cli_inform("{.field plot mode} polygons of pmtiles are not supported in plot mode yet")
		return(NULL)
	}


	dt = res$dt
	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)

	# none should contain NA's && (length or content should be different)
	diffAlpha = !anyNA(c(gp$fill_alpha, gp$col_alpha)) && !(length(gp$fill_alpha) == length(gp$col_alpha) && all(gp$fill_alpha == gp$col_alpha))


	# patterns: drawn between the fill and the borders
	grb_pat = pattern_grobs(shp, gp, o, id)

	if (!is.null(grb_pat)) {
		gp1 = gp_to_gpar(gp, sel = "fill", o = o, type = "polygons")
		gp2 = gp_to_gpar(gp, sel = "col", o = o, type = "polygons")
		grb1 = sf::st_as_grob(shp, gp = gp1, name = paste0("polygons_", id))
		grb2 = sf::st_as_grob(shp, gp = gp2, name = paste0("polygon_borders_", id))
		grb = grid::grobTree(grb1, grb_pat, grb2)
	} else if (diffAlpha) {
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
	gt_name = paste0("gt_facet_", rc_text)

	if (!is.null(a$blend) && a$blend != "over") {
		existing = gts[[facet_page]]$children$gt_main$children[[gt_name]]$children
		dst = do.call(grid::grobTree, existing)
		grb = blend_grobs(grb, a$blend, dst = dst)
	}

	gt = grid::addGrob(gts[[facet_page]], grb, gPath = grid::gPath(gt_name))
	gts[[facet_page]] = gt
	assign("gts", gts, envir = .TMAP_GRID)

	NULL
}


#' @export
#' @rdname tmapGridLeaflet
tmapGridDataPlot.tm_data_fill = function(a, shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, glid, o, ...) {
	NextMethod()
}


#' @export
#' @rdname tmapGridLeaflet
tmapGridDataPlot.tm_data_borders = function(a, shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, glid, o, ...) {
	NextMethod()
}


# Pattern grobs for polygons: one grob per unique pattern (pattern, color, size, density), drawn
# within the outlines of the features that share it. All patterns are aligned to the same origin.
# Returns NULL if there are no patterns, or the device does not support clipping paths.
pattern_grobs = function(shp, gp, o, id) {
	# unresolved map variables (e.g. "__pattern" in inset maps) are not drawn / take defaults
	unresolved = function(x) is.null(x) || (is.character(x) && any(startsWith(x, "__"), na.rm = TRUE))
	if (unresolved(gp$pattern) || all(is.na(gp$pattern) | gp$pattern == "none")) return(NULL)
	for (v in c("pattern_col", "pattern_size", "pattern_density")) if (unresolved(gp[[v]])) gp[v] = list(NULL)
	if (!pattern_device_support()) return(NULL)

	n = length(shp)
	pdt = data.frame(i = seq_len(n),
					 pattern = rep_len(gp$pattern, n),
					 col = rep_len(if (is.null(gp$pattern_col)) "grey25" else gp$pattern_col, n),
					 size = rep_len(if (is.null(gp$pattern_size)) 1 else gp$pattern_size, n),
					 density = rep_len(if (is.null(gp$pattern_density)) 0.8 else gp$pattern_density, n),
					 stringsAsFactors = FALSE)
	drawn = !is.na(pdt$pattern) & pdt$pattern != "none" & !is.na(pdt$density) & pdt$density >= 0.05
	pdt = pdt[drawn, , drop = FALSE]
	if (!nrow(pdt)) return(NULL)

	scale = o$scale * (o$scale_down %||% 1)
	grps = split(pdt, pdt[c("pattern", "col", "size", "density")], drop = TRUE)

	grbs = lapply(seq_along(grps), function(k) {
		r = grps[[k]]
		clip = sf::st_as_grob(shp[r$i], gp = grid::gpar())
		pattern_area_grob(clip, r$pattern[1], density = r$density[1], size = r$size[1], col = r$col[1],
						  scale = scale, line_in = o$lin, name = paste0("polygon_patterns_", id, "_", k))
	})
	grbs = grbs[!vapply(grbs, is.null, logical(1))]
	if (!length(grbs)) return(NULL)
	do.call(grid::grobTree, c(grbs, list(name = paste0("polygon_patterns_", id))))
}

# Does the current graphics device support clipping paths (needed for patterns)? Warn once if not.
pattern_device_support = function() {
	caps = tryCatch(grDevices::dev.capabilities("clippingPaths")$clippingPaths,
					error = function(e) NA)
	ok = is.null(caps) || is.na(caps) || isTRUE(caps)
	if (!ok) {
		cli::cli_warn(paste("{.field [pattern]} the graphics device does not support clipping paths,",
							"so patterns are not drawn. Use a device such as {.fn png}, {.fn svg},",
							"or {.fn pdf} (R >= 4.1)."),
					  .frequency = "regularly", .frequency_id = "pattern_device")
	}
	ok
}

# Legend items of pattern legends (portrait and landscape): fill, pattern, and border, per item.
# The pattern spacing and line width match the map (also in facets).
legend_pattern_items = function(gp, nlev, comp, o) {
	gpars1 = gp_to_gpar(gp, sel = "fill", split_to_n = nlev, o = o, type = "rect")
	gpars2 = gp_to_gpar(gp, sel = "col", split_to_n = nlev, o = o, type = "rect")
	sd = o$scale_down %||% 1
	pat = rep_len(gp$pattern, nlev)
	pcol = rep_len(if (is.null(gp$pattern_col)) "grey25" else gp$pattern_col, nlev)
	psize = rep_len(if (is.null(gp$pattern_size)) 1 else gp$pattern_size, nlev) * sd
	pdens = rep_len(if (is.null(gp$pattern_density)) 0.8 else gp$pattern_density, nlev)
	lapply(seq_len(nlev), function(i) {
		rect = rndrectGrob(gp = gpars1[[i]], r = comp$item.r)
		pg = if (is.na(pat[i]) || pat[i] == "none") NULL else {
			pattern_area_grob(grid::rectGrob(), pat[i], density = pdens[i], size = psize[i], col = pcol[i],
							  scale = o$scale * sd, line_in = o$lin)
		}
		grid::grobTree(rect, pg, rndrectGrob(gp = gpars2[[i]], r = comp$item.r))
	})
}
