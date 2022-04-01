addBaseGroup <- function(group) {
	for (g in group) {
		if (is.na(bases[1])) {
			bases <- g
		} else if (!(g %in% bases)) {
			bases <- c(bases, g)
		}
	}
	assign("bases", bases, envir = .TMAP_LEAFLET)
}

eraseBaseGroup <- function() {
	assign("bases", character(0), envir = .TMAP_LEAFLET)
}

eraseOverlayTiles <- function() {
	overlays <- setdiff(overlays, overlays_tiles)
	assign("overlays", overlays, envir = .TMAP_LEAFLET)
}

addOverlayGroup <- function(group, are.tiles = FALSE) {
	for (g in group) {
		if (is.na(overlays[1])) {
			overlays <- g
		} else if (!(g %in% overlays)) {
			overlays <- c(overlays, g)
		}
	}
	assign("overlays", overlays, envir = .TMAP_LEAFLET)
	if (are.tiles) assign("overlays_tiles", c(overlays_tiles, group), envir = .TMAP_LEAFLET)
}


tmapLeafletInit = function(o) {
	if (!requireNamespace("leaflet")) stop("leaflet package required but not installed yet.")

	per_page = rep(o$ncols * o$nrows, o$npages)
	k = o$ncols * o$nrows * o$npages
	if (o$n < k) {
		per_page[o$npages] = per_page[o$npages] - (k - o$n)
	}
	
	proxy = FALSE
	
	# leaflet options
	
	if (o$crs_leaflet$crsClass == "L.CRS.Simple") {
		if (is.na(o$set.zoom.limits)[1]) o$set.zoom.limits[1] <- -1000
	}	
	leaflet_opts = do.call(leaflet::leafletOptions, c(list(crs=o$crs_leaflet), o$leaflet.options))
	
	if (!is.na(o$set.zoom.limits[1])) leaflet_opts$minZoom = o$set.zoom.limits[1]
	if (!is.na(o$set.zoom.limits[2])) leaflet_opts$maxZoom = o$set.zoom.limits[2]

	leaflet_opts$attributionControl = FALSE
	
	lfs = lapply(per_page, function(p) {
		lapply(seq_len(p), function(i) {
			if (!proxy) lf = leaflet::leaflet(options = leaflet_opts)
			
			lf = appendContent(lf, {
				tags$head(
					tags$style(HTML(paste(".leaflet-container {background:", o$bg.color, ";}", sep="")))
				)	
			})
			lf
		})
	})
	
	# bases = if ("bases" %in% ls(envir = .TMAP_LEAFLET)) get("bases", envir = .TMAP_LEAFLET) else NA
	# overlays = if ("overlays" %in% ls(envir = .TMAP_LEAFLET)) get("overlays", envir = .TMAP_LEAFLET) else NA
	# overlays_tiles = if ("overlays_tiles" %in% ls(envir = .TMAP_LEAFLET)) get("overlays_tiles", envir = .TMAP_LEAFLET) else character(0)
	# 
	# layerIds = list()
	# start_pane_id = 401
	# 
	# # should the layer control include base layers? TRUE if |basemaps| > 1 || names/groups are specified
	# basename.specified = FALSE
	# 
	# 
	# 
	# ### find z indeces and create tmapXXX panes
	# zids <- lapply(gp, function(gpl) {
	# 	po <- gpl$plot.order
	# 	
	# 	po2 <- substr(po, 4, nchar(po))
	# 	po2[po2 == "symbols"] <- "symbol"
	# 	po2[po2 == "tiles"] <- "tile"
	# 	po2[po2 == "lines"] <- "line"
	# 	
	# 	zi <- sapply(po2, function(p) {
	# 		if (p == "grid") gt$grid.zindex else gpl[[paste0(p, ".zindex")]]
	# 	})
	# 	
	# 	if (!is.null(gpl$tile.gtype) && gpl$tile.gtype == "base") {
	# 		zi[names(zi) == "tile"] <- 0
	# 	}
	# 	zi
	# })
	# zids_vec <- unlist(zids, use.names = FALSE)
	# 
	# # For tmapProxy: only use pane with a higher z number than existing ones
	# # Only use free panes: every layer must be in a different pane
	# z_free <- setdiff(start_pane_id:(start_pane_id+length(zids_vec)*2-1), na.omit(zids_vec))
	# zids_vec[is.na(zids_vec)] <- rep(z_free, length.out = sum(is.na(zids_vec)))
	# zids_len <- sapply(zids, length)
	# zindices <- split(zids_vec, unlist(mapply(rep, 1:length(zids), each = zids_len, SIMPLIFY = FALSE), use.names = FALSE))
	# tmap_zindices <- sort(unique(unname(setdiff(zids_vec, 0))))
	# 
	# ## get/set existing panes
	# if (!proxy) {
	# 	assign("pane_ids", tmap_zindices, envir = .TMAP_CACHE)
	# 	z_panes <- integer()
	# } else {
	# 	z_panes <- get("pane_ids", envir = .TMAP_CACHE)
	# 	assign("pane_ids", union(tmap_zindices, z_panes), envir = .TMAP_CACHE)
	# }
	# 
	# # add new panes
	# for (z in setdiff(tmap_zindices, z_panes)) {
	# 	lf <- addMapPane(lf, paneName(z), zIndex = z)
	# }
	# 
	# 
	# 
	# 
	# 
	# 
	.TMAP_LEAFLET$lfs = lfs
	.TMAP_LEAFLET$nrow = o$nrows
	.TMAP_LEAFLET$ncol = o$ncols
	.TMAP_LEAFLET$leg_id = 1
	NULL
}
