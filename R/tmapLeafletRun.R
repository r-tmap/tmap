tmapLeafletRun = function(o, q, show, knit, knit_opts, args) {
	lfs = get("lfs", envir = .TMAP_LEAFLET)

	if (show && o$show_gif_ani) {
		cli::cli_alert("{.field view mode} Animations are not implemented in view mode, so they are shown as facets")
	}

	# switchable zoom levels
	zids = which(!is.na(q$group.zoom_levels))
	if (length(zids)) {
		lfs = lapply(lfs, function(lfp) {
			lapply(lfp, function(lf) {
				for (zid in zids) lf = leaflet::groupOptions(lf, group = q$group[zid], zoomLevels = q$group.zoom_levels[[zid]])
				lf
			})
		})
	}

	lfs2 = lapply(lfs, function(lfsi) {
		x = if (o$nrows == 1 && o$ncols == 1) {
			lf = lfsi[[1]]
			# proxy remove
			if (!is.null(.TMAP_LEAFLET$layerIds2)) {
				L = .TMAP_LEAFLET$layerIds
				for (L2 in .TMAP_LEAFLET$layerIds2) {
					if (L2$type == "raster") {
						lf = leaflet::removeImage(lf, L2$Lid)
					} else if (L2$type %in% c("symbols", "text")) {
						lf = leaflet::removeMarker(lf, L2$Lid)
					} else {
						lf = leaflet::removeShape(lf, L2$Lid)
					}
					L = lapply(L, function(x) {
						if (any(L2$Lid %in% x$Lid)) NULL else x
					})
					L = L[!vapply(L, is.null, FUN.VALUE = logical(1))]
				}
				# to do: update group and type arguments
				.TMAP_LEAFLET$layerIds = L
				.TMAP_LEAFLET$layerIds2 = NULL
			}
			lf
		} else {
			fc = o$free.coords
			sync = if (identical(o$sync, TRUE) || all(!fc)) {
				"all"
			} else if (all(fc)) {
				"none"
			} else if (fc[1]) {
				asplit(matrix(1:(o$nrows*o$ncols), ncol = o$ncols, byrow = TRUE), 1)
			} else {
				asplit(matrix(1:(o$nrows*0$ncols), ncol = 0$ncols, byrow = TRUE), 2)
			}
			marg = paste0(o$between_margin, "em")

			#print(do.call(leafsync::latticeView, c(lfsi, list(ncol = o$ncols, sync = sync, sync.cursor = all(!fc), no.initial.sync = FALSE, between = list(x = marg, y = marg)))))
			do.call(leafsync::latticeView, c(lfsi, list(ncol = o$ncols, sync = sync, sync.cursor = all(!fc), no.initial.sync = FALSE)))
		}


		if (length(.TMAP_LEAFLET$markerLayers)) {
			style_markers = paste(vapply(unique(.TMAP_LEAFLET$markerLayers), function(pane) {
				paste0(".leaflet-container .leaflet-", pane, "-pane img {
				max-width: none !important;
				max-height: none !important;
				width: auto;
				padding: 0;
				}\n")}, FUN.VALUE = character(1)), collapse = "\n")
		} else {

			style_markers = NULL
		}

		if (o$pc$sepia_intensity != 0) {
			col = process_color("#ffffff", sepia_intensity = o$pc$sepia_intensity)
			style_sepia = paste0(".leaflet-control-layers {background: ", col, ";}
				.leaflet-control-zoom-in {background: ", col, " !important;}
				.leaflet-control-zoom-out {background: ", col, " !important;}\n"
			)
		} else {
			style_sepia = NULL
		}

		if (is.null(style_markers) && is.null(style_sepia)) {
			x
		} else {
			if (.TMAP$in.shiny) {

				x
			} else {
				htmlwidgets::prependContent(x, htmltools::tags$style(paste0(style_markers, style_sepia)))
			}
		}
	})

	if (length(lfs2) == 1) lfs2 = lfs2[[1]]
	if (show && !knit && !.TMAP$in.shiny) {

		# borrowed from mapview
		viewer = getOption("viewer")
		ide = get_ide()
		if (!is.null(viewer)) {
			viewerFunc = function(url) {
				paneHeight = lfs2$sizingPolicy$viewer$paneHeight
				if (identical(paneHeight, "maximize")) {
					paneHeight = -1
				}
				if (ide == "vscode") {
					# VSCode's viewer can't ignore cross-origin requests. Need to serve the
					# map so assests can be read, e.g. .fgb files.
					server <- servr::httd(
						dir = get_url_dir(url),
						verbose = FALSE,
						browser = FALSE
					)
					url <- server$url

				}
				viewer(url, height = paneHeight)
			}
		} else {
			viewerFunc = function(url) {
				dir = get_url_dir(url)
				switch(ide,
					   "rstudio" = if (o$use_browser) {
					   	fl = file.path(dir, "index.html")
					   	utils::browseURL(fl)
					   } else {
					   	servr::httd(
					   		dir = dir,
					   		verbose = FALSE
					   	)
					   },
					   "vscode" = servr::httd(
					   	dir = dir,
					   	verbose = FALSE
					   ),
					   # default
					   servr::httd(
					   	dir = dir,
					   	verbose = FALSE
					   )
				)
			}
		}

		htmltools::html_print(
			htmltools::as.tags(lfs2, standalone = TRUE)
			, viewer = if (interactive()) viewerFunc
		)
	}
	lfs2
}
