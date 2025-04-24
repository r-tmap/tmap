crs_is_ortho = function(crs) {
	grepl("Orthographic", crs$wkt, fixed = TRUE)
}

crs_ortho_visible = function(crs, projected = TRUE, max_cells = 1e5) {
	wkt = sf::st_crs(crs)$wkt
	lst = strsplit(wkt, ",")[[1]]
	lat0 = as.numeric(lst[[which(grepl("Latitude of natural origin", lst))+1]])
	lon0 = as.numeric(lst[[which(grepl("Longitude of natural origin", lst))+1]])

	b = s2::s2_buffer_cells(s2::as_s2_geography(paste0("POINT(", lon0, " ", lat0, ")")), 9800000, max_cells = max_cells) # visible half

	pole_n = s2::s2_intersects("POINT(0 90)", b)
	pole_s = s2::s2_intersects("POINT(0 -90)", b)

	sfc = sf::st_as_sfc(b)
	co = sf::st_coordinates(sfc)[,1:2]

	if (pole_n || pole_s) {
		# STEP 1: create pattern from -360 to 360 (to fix potential 180/-180 meridian problems)

		# step 1a: arrange coordinates by lon
		lonmin_id = which.min(co[,1])
		lonmax_id = which.max(co[,1])

		if (pole_n) {
			co2 = co[c(lonmin_id:nrow(co), 1:lonmax_id), ]
		} else {
			co2 = co[c(lonmax_id:nrow(co), 1:lonmin_id), ]
		}

		# step 1b: replicate one before and one after
		co_left = co2
		co_left[,1] = co_left[,1] - 360
		co_right = co2
		co_right[,1] = co_right[,1] + 360

		if (pole_n) {
			co3 = rbind(co_left, co2, co_right)
		} else {
			co3 = rbind(co_right, co2, co_left)
		}

		co4 = co3[co3[,1] >= -360 & co3[,1] <= 360, ]

		# step 1c; make sure to close the polygon
		if (pole_n) {
			co4[1,1] = -360
			co4[nrow(co4),1] = 360
		} else {
			co4[1,1] = 360
			co4[nrow(co4),1] = -360
		}
		lat = co4[1,2]
		co4[nrow(co4),2] = lat

		# STEP 2: add block for the pole
		if (pole_n) {
			to_add = t(sapply(seq(360, -360, by = -45), c, 90))
		} else {
			to_add = t(sapply(seq(-360, 360, by = 45), c, -90))
		}
		co5 = rbind(co4,
					to_add,
					co4[1,,drop=FALSE])

		sfc = sf::st_sfc(sf::st_polygon(list(co5)), crs = sf::st_crs(sfc))

	} else {
		if (lon0 >= 0) {
			lon_shift = lon0 - 180

			co2 = co
			co2[,1][co2[,1] < lon_shift] = co2[,1][co2[,1] < lon_shift] + 360

			co_left = co2
			co_left[,1] = co_left[,1] - 360
			co_right = co2
			co_right[,1] = co_right[,1] + 360

			sfc = sf::st_sfc(sf::st_multipolygon(list(list(co_left), list(co2), list(co_right))), crs = sf::st_crs(sfc))
		} else {
			lon_shift = lon0 + 180

			co2 = co
			co2[,1][co2[,1] > lon_shift] = co2[,1][co2[,1] > lon_shift] - 360

			co_left = co2
			co_left[,1] = co_left[,1] - 360
			co_right = co2
			co_right[,1] = co_right[,1] + 360

			co3 = rbind(co_left, co2, co_right)

			sfc = sf::st_sfc(sf::st_multipolygon(list(list(co_left), list(co2), list(co_right))), crs = sf::st_crs(sfc))
		}


	}

	if (projected) sf::st_transform(sfc, crs) else sfc
}

end_of_the_world = function(crs, earth_datum) {
	wkt = sf::st_crs(crs)$wkt

	# orthographic
	if (grepl("Orthographic", wkt, fixed = TRUE)) {
		lst = strsplit(wkt, ",")[[1]]
		lat0 = as.numeric(lst[[which(grepl("Latitude of natural origin", lst))+1]])
		lon0 = as.numeric(lst[[which(grepl("Longitude of natural origin", lst))+1]])

		b = s2::s2_buffer_cells(s2::as_s2_geography(paste0("POINT(", lon0, " ", lat0, ")")), 9800000) # visible half

		sf::st_as_sfc(b) |>
			sf::st_transform(crs)
	} else {
		sf::st_bbox(stars::st_as_stars()) |>
			sf::st_as_sfc() |>
			sf::st_set_crs(NA) |>
			sf::st_segmentize(1) |>
			sf::st_set_crs(earth_datum) |>
			sf::st_transform(crs)
	}


}

crop_lat = function(bb, crs, limit_latitude_3857 = NULL) {
	if ((crs == 3857 || crs == st_crs(3857)) && (!is.null(limit_latitude_3857) && (!identical(limit_latitude_3857, FALSE)))) {
		crp = sf::st_bbox(c(xmin = -180, xmax = 180, ymin = limit_latitude_3857[1], ymax = limit_latitude_3857[2]), crs = 4326)
		crp2 = sf::st_transform(crp, crs = crs)
		bb['xmin'] = max(bb['xmin'], crp2['xmin'])
		bb['xmax'] = min(bb['xmax'], crp2['xmax'])
		bb['ymin'] = max(bb['ymin'], crp2['ymin'])
		bb['ymax'] = min(bb['ymax'], crp2['ymax'])
		bb
	} else {
		bb
	}
}
