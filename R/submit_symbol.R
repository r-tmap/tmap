submit_symbols_Grid = function(x, just = c(0.5, 0.5), just.override = FALSE, grob.dim = c(width = 48, height = 48, render.width = 256, render.height = 256
)) {
	submit_symbols(x = x, gs = "Grid", just = just, just.override = just.override, grob.dim = grob.dim)}

submit_symbols_Leaflet = function(x, just = c(0.5, 0.5), just.override = FALSE, grob.dim = c(width = 48, height = 48, render.width = 256, render.height = 256
)) {
	submit_symbols(x = x, gs = "Leaflet", just = just, just.override = just.override, grob.dim = grob.dim)
}


submit_symbols = function(x, gs, just, just.override, grob.dim) {
	if (any(vapply(x, is.null, logical(1)))) stop("one of more shapes are NULL")
	shapeLib = get("shapeLib", envir = .TMAP)
	justLib = get("justLib", envir = .TMAP)
	n = length(x)
	id = 999 + length(shapeLib)
	if (gs == "Leaflet") {
		items = lapply(x, function(xs) {
			ic = if ("iconUrl" %in% names(xs)) {
				split_icon(xs)[[1]]
			} else if (is.grob(xs)) {
				grob2icon(xs, grob.dim, just)
			} else NA
			
			# add anchor based on just specs
			if (all(c("iconWidth", "iconHeight") %in% names(ic)) && 
				((!any(c("iconAnchorX", "iconAnchorY") %in% names(ic))) || just.override)) {
				ic$iconAnchorX = ic$iconWidth * (1-just[1])
				ic$iconAnchorY = ic$iconHeight * just[2]
			}
			ic
		})
		just_items = as.list(rep(NA, n))
	} else if (gs == "Grid") {
		just_items = lapply(x, function(xs) {
			if (just.override) {
				just
			} else if ("iconUrl" %in% names(xs)) {
				if (all(c("iconWidth", "iconHeight", "iconAnchorX", "iconAnchorY") %in% names(xs))) {
					c(1-(xs$iconAnchorX / xs$iconWidth), xs$iconAnchorY / xs$iconHeight)
				} else NA
			} else NA
		})
		
		items = lapply(x, function(xs) {
			if ("iconUrl" %in% names(xs)) {
				grb = icon2grob(xs)
				# take first one
				if (is.grob(grb)) grb else grb[[1]]
			} else if (is.grob(xs)) {
				xs
			} else NA
		})	
	} else {
		stop("Symbols not supported", call. = FALSE)
	}
	
	numbers = is.na(items)
	
	if (all(numbers)) return(unlist(x, use.names = FALSE))
	
	new_id = id + 1:sum(!numbers)
	
	x2 = integer(n)
	x2[numbers] = unlist(x[numbers], use.names = FALSE)
	x2[!numbers] = new_id
	
	shapeLib = c(shapeLib, items[!numbers])
	justLib = c(justLib, just_items[!numbers])
	assign("shapeLib", shapeLib, envir = .TMAP)
	assign("justLib", justLib, envir = .TMAP)
	names(x2) = names(x)
	x2
}

