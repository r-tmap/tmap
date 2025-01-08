submit_symbols = function(x, grid, args) {
	if (any(vapply(x, is.null, logical(1)))) stop("one of more shapes are NULL")
	shapeLib = get("shapeLib", envir = .TMAP)
	justLib = get("justLib", envir = .TMAP)
	n = length(x)
	id = 999 + length(shapeLib)
	if (grid) {
		# symbols as grobs
		just_items = lapply(x, function(xs) {
			if (args$just.override) {
				args$just
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
		# symbols as images
		items = lapply(x, function(xs) {
			ic = if ("iconUrl" %in% names(xs)) {
				split_icon(xs)[[1]]
			} else if (is.grob(xs)) {
				grob2icon(xs, args$grob.dim, args$just)
			} else NA

			# add anchor based on just specs
			if (all(c("iconWidth", "iconHeight") %in% names(ic)) &&
				((!any(c("iconAnchorX", "iconAnchorY") %in% names(ic))) || args$just.override)) {
				ic$iconAnchorX = ic$iconWidth * (1-args$just[1])
				ic$iconAnchorY = ic$iconHeight * args$just[2]
			}
			ic
		})
		just_items = as.list(rep(NA, n))
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

