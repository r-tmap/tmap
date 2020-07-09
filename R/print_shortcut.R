print_shortcut <- function(x, interactive, in.shiny, args, knit) {
	if (getOption("tmap.mode")=="plot") {
		stop("Either specify shp, or set mode to \"view\" with tmap_mode or ttm", call.=FALSE)	
	} else {
		xtiles <- which(names(x) == "tm_tiles")
		
		gt <- pre_process_gt(x, interactive=interactive)
		gt$shp_name <- rep("dummy", length(xtiles))
		
		gt$shape.units <- list(unit = get("tmapOptions", envir = .TMAP_CACHE)$unit)
		if (is.null(gt$bbox)) gt$bbox <- c(-190, -90, 180, 90)
		
		if (any(names(x) == "tm_scale_bar")) {
			gsbid <- which(names(x) == "tm_scale_bar")[1]
			gsb <- x[[gsbid]]
		} else {
			gsb <- NULL
		}
		gsb <- process_meta_scale_bar(gsb, interactive = TRUE, gt)
		
		if (any(names(x) == "tm_grid")) {
			ggid <- which(names(x) == "tm_grid")[1]
			gg <- x[[ggid]]
		} else {
			gg <- NULL
		}		
		gg <- process_meta_grid(gg, gt)
		
		gmmid <- which(names(x)=="tm_minimap")[1]
		gmm <- x[[gmmid]]
		gmm <- process_meta_minimap(gmm, interactive = TRUE, gt)
		
		gmmcid <- which(names(x)=="tm_mouse")[1]
		gmmc <- x[[gmmcid]]
		if (is.null(gmmc)) gmmc = list(mouse.show = FALSE)
		
		gt <- c(gt, gsb, gg, gmm, gmmc)
		
		
		#gt$scale.show <- FALSE
		#gt$shape.bbx <- x$tm_shortcut$bbx
		#gt$shape.center <- x$tm_shortcut$center
		
		x[xtiles] <- lapply(x[xtiles], function(xi) {
			xi <- process_tiles(xi, gt)
			xi$plot.order <- "tm_tiles"
			xi
		})
		
		if (gt$grid.show) x[[xtiles[1]]]$plot.order <- c("tm_tiles", "tm_grid")
		
		
		x[names(x) == "tm_shape"] <- NULL
		
		x <- x[!(names(x) %in% c("tm_layout", "tm_view", "tm_style", "tm_grid", "tm_facets", "tm_credits", "tm_logo", "tm_compass", "tm_scale_bar", "tm_xlab", "tm_ylab", "tm_minimap", "tm_mouse"))]
		
		x$tm_layout <- gt
		
		view_tmap(x, shps = list(dummy = NULL), in.shiny = in.shiny)
	}
}
