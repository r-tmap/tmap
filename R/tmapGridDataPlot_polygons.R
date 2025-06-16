#' @export
#' @rdname tmapGridLeaflet
tmapGridDataPlot.tm_data_polygons = function(a, shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {

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


#' @export
#' @rdname tmapGridLeaflet
tmapGridDataPlot.tm_data_fill = function(a, shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	NextMethod()
}


#' @export
#' @rdname tmapGridLeaflet
tmapGridDataPlot.tm_data_borders = function(a, shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	NextMethod()
}
