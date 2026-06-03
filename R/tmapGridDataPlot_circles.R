# Helper: convert per-feature metre radii to native CRS units, for use with
# grid::circleGrob(r = unit(r_native, "native")).
#
# For geographic CRS the approximation is 111 320 m/degree.
# For projected CRS (including Web Mercator) we derive the conversion by
# displacing a reference point 1 degree north in geographic space and measuring
# how many native units that corresponds to. This correctly accounts for scale
# distortion (e.g. Mercator stretching at higher latitudes) without assuming
# any specific projection.
.circles_m_to_native = function(radius_m, shp) {
	if (isTRUE(sf::st_is_longlat(shp))) {
		return(radius_m / 111320)
	}

	# Back-project the centroid to geographic, displace 1 degree northward,
	# re-project both points to the display CRS, and compute the dy in native units.
	crs_disp = sf::st_crs(shp)
	ctr = sf::st_centroid(sf::st_union(shp))
	ctr_geo = sf::st_transform(ctr, 4326)

	ctr_geo_n = sf::st_sfc(
		sf::st_point(sf::st_coordinates(ctr_geo) + c(0, 1)),
		crs = 4326
	)

	dy_native = abs(
		sf::st_coordinates(sf::st_transform(ctr_geo_n, crs_disp))[, 2] -
		sf::st_coordinates(sf::st_transform(ctr_geo,   crs_disp))[, 2]
	)

	# dy_native = native units per degree; 111 320 m ≈ 1 degree latitude
	m_to_native = dy_native / 111320
	radius_m * m_to_native
}


#' @export
#' @rdname tmapGridLeaflet
tmapGridDataPlot.tm_data_circles = function(a, shpTM, dt, gp, bbx,
											facet_row, facet_col, facet_page,
											id, pane, group, glid, o, ...) {
	rc_text = frc(facet_row, facet_col)

	# Filter to rows where size is known (no shape filter: circles have none)
	res = select_sf(shpTM, dt[!is.na(dt$size), ])
	shp = res$shp
	dt  = res$dt

	gp = impute_gp(gp, dt)
	# NOTE: do NOT call rescale_gp — that converts size to "lines" units.
	# For tm_circles, gp$size is already in metres (the direct scale output).

	coords = sf::st_coordinates(shp)
	cp     = expand_coords_gp(coords, gp, ndt = nrow(dt))
	coords = cp$coords
	gp     = cp$gp

	# gp$size is in metres; convert to native CRS units for grid, correcting for
	# any projection scale distortion (e.g. Web Mercator at higher latitudes).
	radius_native = .circles_m_to_native(gp$size, shp)

	gp_par = gp_to_gpar(gp, sel = "all", o = o, type = "symbols")

	grobs = lapply(seq_along(radius_native), function(i) {
		gpi = structure(lapply(gp_par, function(x) if (length(x) == 1L) x else x[i]),
						class = "gpar")
		grid::circleGrob(
			x  = grid::unit(coords[i, 1], "native"),
			y  = grid::unit(coords[i, 2], "native"),
			r  = grid::unit(radius_native[i], "native"),
			gp = gpi
		)
	})

	grb = grid::gTree(children = do.call(grid::gList, grobs),
					  name     = paste0("circles_", id))

	gts = get("gts", .TMAP_GRID)
	gt  = gts[[facet_page]]
	gt  = grid::addGrob(gt, grb, gPath = grid::gPath(paste0("gt_facet_", rc_text)))
	gts[[facet_page]] = gt
	assign("gts", gts, envir = .TMAP_GRID)
	NULL
}
