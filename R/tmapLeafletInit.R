tmapLeafletInit = function(o, return.asp = FALSE, vp, prx, lf = NULL, ...) {
	args = list(...)
	if (return.asp) return(1)

	per_page = rep(o$ncols * o$nrows, o$npages)
	k = o$ncols * o$nrows * o$npages
	if (o$n < k) {
		per_page[o$npages] = per_page[o$npages] - (k - o$n)
	}

	#proxy = !is.null(lf)
	proxy2 = (length(prx) > 0)

	# leaflet options

	if (o$crs_leaflet$crsClass == "L.CRS.Simple") {
		if (is.na(o$set_zoom_limits)[1]) o$set_zoom_limits[1] = -1000
	}
	leaflet_opts = do.call(leaflet::leafletOptions, c(list(crs=o$crs_leaflet), o$leaflet.options))

	if (!is.na(o$set_zoom_limits[1])) leaflet_opts$minZoom = o$set_zoom_limits[1]
	if (!is.na(o$set_zoom_limits[2])) leaflet_opts$maxZoom = o$set_zoom_limits[2]

	leaflet_opts$attributionControl = TRUE

	if (!.TMAP$proxy) {
		.TMAP_LEAFLET$layerIds = NULL
		.TMAP_LEAFLET$markerLayers = character(0)
	}


	lfs = lapply(per_page, function(p) {
		lapply(seq_len(p), function(i) {
			if (!.TMAP$proxy) {
				lf = leaflet::leaflet(options = leaflet_opts)
			}

			if (proxy2) {
				L2 = list()
				for (px in prx) {
					z = px$zindex
					L = .TMAP_LEAFLET$layerIds
					Lnames = vapply(L, function(l) {
						l$name
					}, FUN.VALUE = character(1))
					Lids = lapply(L, function(l) {
						l$Lid
					})
					Ltypes = vapply(L, function(l) {
						l$type
					}, FUN.VALUE = character(1))

					id = which(Lnames == pane_name(z))[1]
					if (!is.na(id)) {
						tp = Ltypes[id]
						if (tp %in% c("polygons", "symbols", "text", "lines")) {
							L2 = c(L2, list(list(name = Lnames[id], type = tp, Lid = Lids[[id]])))
						}
					}
					#L = L[-id]
				}
				#.TMAP_LEAFLET$layerIds = L
				.TMAP_LEAFLET$layerIds2 = L2
			}

			if (!.TMAP$in.shiny) {
				appendContent(lf, {
					tags$head(
						tags$style(HTML(paste(".leaflet-container {background:", o$bg.color, ";}", sep="")))
					)
				})
			} else {
				lf
			}
		})
	})

    #.TMAP_LEAFLET$layerIds = layerIds

	.TMAP_LEAFLET$lfs = lfs
	.TMAP_LEAFLET$nrow = o$nrows
	.TMAP_LEAFLET$ncol = o$ncols
	.TMAP_LEAFLET$leg_id = 1
	NULL
}

tmapLeafletAux = function(o, q) {
	lfs = .TMAP_LEAFLET$lfs


	isTMAP = substr(q$pane, 1, 4) == "tmap"
	isNEW = q$new

	lids = setdiff(q$lid[isTMAP & isNEW], .TMAP$pane_ids)

	groups_radio = unique(unlist(strsplit(q$group[q$group.control == "radio"], split = "__", fixed = TRUE)))
	groups_check = unique(unlist(strsplit(q$group[q$group.control == "check"], split = "__", fixed = TRUE)))


	# remove radio button when there is only one
	if (is.null(groups_radio) || length(groups_radio) == 1) groups_radio = character(0)
	if (is.null(groups_check)) groups_check = character(0)

	lfs = lapply(lfs, function(lfp) {
		lapply(lfp, function(lf) {
			if (length(lids)) for (lid in lids) lf = leaflet::addMapPane(lf, pane_name(lid), zIndex = lid)
			if (length(groups_radio) > 0L || length(groups_check) > 0L) {
				lf = leaflet::addLayersControl(lf, baseGroups = groups_radio, overlayGroups = groups_check, position = leaflet_pos(str2pos(o$control.position)), options = leaflet::layersControlOptions(collapsed = o$control.collapse))
			}
			lf
		})
	})


	.TMAP_LEAFLET$lfs = lfs
	NULL
}




view_set_bounds = function(lf, bbx, o) {
	if (!is.logical(o$set_bounds)) {
		lims = unname(o$set_bounds)
	} else {
		lims = unname(bbx)
	}
	if (!(identical(o$set_bounds, FALSE))) {
		lf = lf %>% leaflet::setMaxBounds(lims[1], lims[2], lims[3],lims[4])
	}

	if (is.na(o$set_view[1])) {

		# check if zoom is used from tm_basemap, and if so, use that
		zm = NA
		for (x in .TMAP_LEAFLET$tiles) {
			for (xi in x) {
				if (!is.na(xi$zoom)) {
					zm = xi$zoom
				}
			}
		}

		if (!is.na(zm)) {
			set_view = c(mean.default(lims[c(1,3)]), mean.default(lims[c(2,4)]), zm)
		} else if (!is.na(o$set_zoom_limits[2])) { # 2nd is checked to bypass (-1000, NA) used for simple CRS
			set_view = c(mean.default(lims[c(1,3)]), mean.default(lims[c(2,4)]), o$set_zoom_limits[1])
		} else {
			set_view = NULL
		}
	} else if (length(o$set_view) == 1) {
		set_view = c(mean.default(lims[c(1,3)]), mean.default(lims[c(2,4)]), o$set_view)
	} else {
		set_view = o$set_view
	}

	if (!is.null(set_view)) {
		names(set_view) = NULL
		lf = lf %>% setView(set_view[1], set_view[2], set_view[3])
	}

	if (!is.null(o$center)) lf = lf %>% addMarkers(o$center$lon, o$center$lat, label = o$center$query)

	lf
}
