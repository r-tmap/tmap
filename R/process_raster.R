check_raster_specials <- function(x, g, gt, shpcols, data, nx) {
	## check raster shortcuts
	# if (isTRUE(attr(data, "is.RGB"))) {
	# 	is.colors <- TRUE
	# 	nx <- 1
	# 	x <- setdiff(names(data), c("tmapfilter", "GROUP_BY", "ALONG"))
	# } else 
	if ("PIXEL__COLOR" %in% names(data)) {
		x <- "PIXEL__COLOR"
		data$PIXEL__COLOR <- do.call("process_color", c(list(col=data$PIXEL__COLOR, alpha=g$alpha), gt$pc))
		is.colors <- TRUE
		nx <- 1
	} else if ("FILE__VALUES" %in% names(data)) {
		x <- "FILE__VALUES"
		is.colors <- FALSE
		nx <- 1
	} else {
		x <- g$col
		
		if (attr(data, "treat_as_by")) {
			if (!is.na(x)) warning("col specification in tm_raster is ignored, since stars object contains a 3rd dimension, where its values are used to create facets", call. = FALSE)
			x <- NA
		}
		
		# by default, use the all data variables
		if (is.na(x[1])) {
			if (nlevels(data$GROUP_BY) > 1) {
				x <- gt$aes.colors["dots"]
			} else {
				x <- setdiff(names(data), c("tmapfilter", "GROUP_BY", "ALONG"))
				g$col <- x
			}
		}
		
		## general 'by' check: if by => |aes| = 1, and determine nx
		if (nlevels(by)>1 && length(x) > 1) {
			warning("When by is specified (tm_facets), only one value can be assigned to each aesthetic.", call. = FALSE)
			x <- x[1]
		}
		nx <- length(x)
		
		# check for direct color input
		if (all(x %in% shpcols)) {
			is.colors <- FALSE
		} else {
			# check for direct color input
			is.colors <- all(valid_colors(x))
			if (!is.colors) stop("Invalid color specification. The available raster variables are: \"", paste(attr(data, "shpnames"), collapse = "\", \""), "\"." , call. = FALSE)
			
			x <- do.call("process_color", c(list(col=col2hex(x), alpha=g$alpha), gt$pc))
			for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- x[i]
			x <- paste("COLOR", 1:nx, sep="_")
		}
	}
	
	## set interpolate: TRUE if is.colors (i.e. image)
	interpolate <- ifelse(is.na(g$interpolate), is.colors, g$interpolate)
	
	if (is.colors) {
		is.OSM <- attr(data, "is.OSM")
		leaflet.server <- attr(data, "leaflet.server")
	} else {
		is.OSM <- FALSE
		leaflet.server <- NA
	}
	
	list(g = g,
		 x = x,
		 data = data,
		 is.colors = is.colors,
		 nx = nx,
		 misc = list(is.OSM=is.OSM, leaflet.server=leaflet.server, interpolate=interpolate))
}

process_raster <- function(data, g, gt, gby, z, interactive) {
	## aesthetics
	xs <- list(raster = g$col)
	process_aes(type = "raster", xs, "xraster", "raster", data, g, gt, gby, z, interactive)
}
