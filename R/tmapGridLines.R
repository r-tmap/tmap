#' @export
#' @rdname tmap_internal
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
