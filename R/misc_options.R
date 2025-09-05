get_option_class = function(o, class = NULL, spatial_class = TRUE) {
	is_spatial = !spatial_class || (any(names(o) %in% c("stars", "sf", "sfc", "raster", "terra", "sp", "dimensions")))
	if (!is.null(class) && is_spatial) { # && is.list(o)
		mtch = which(names(o) %in% class)
		if (!length(mtch)) mtch = which(names(o) == "")[1]
		o = o[[mtch]]
	}
	o
}




# add/merge options x to the full option set o: x can be style options
complete_options = function(x, o, erase_style = TRUE) {
	nmx = names(x)
	nmo = names(o)
	if ("calls" %in% nmx) {
		o$calls = unique(c(x$calls, o$calls))
		x$calls = NULL
		nmx = setdiff(nmx, "calls")
	}
	if (length(x) == 0L) return(o)
	if (is.null(nmo) || is.null(nmx)) return(x)
	d = setdiff(nmx, nmo)
	e = intersect(nmx, nmo)
	if (length(d)) o = c(o, x[d])
	if (length(e)) {
		for (i in e) {
			if (i %in% c("value.const", "value.na", "value.null", "value.blank", "values.var") && erase_style) {
				# special case to cover the following issue
				#    if o = list(value.const = list(fill = "red", fill.polygons = "blue", fill.dots = "black)), and
				#       x = list(value.const = list(fill = "white", fill.polygons = "grey"))
				#    the new option set should be x (so dot fill color should be white)
				o[[i]] = complete_value_list(x[[i]], o[[i]])
			} else {
				if (i != "") {
					tmp = complete_options(x[[i]], o[[i]])
					if (is.null(tmp)) {
						o[i] = list(NULL)
					} else {
						o[[i]] = tmp
					}
				}else {
					o[[which("" == nmo)]] = x[[which("" == nmx)]]
				}
			}

		}
	}
	o
}

complete_value_list = function(x, o) {
	aes_x = gsub("\\..*", "", names(x))
	aes_o = gsub("\\..*", "", names(o))

	aes_o_not_x = setdiff(aes_o, aes_x)

	c(x, o[aes_o %in% aes_o_not_x])
}




getAesOption = function(x, o, aes, layer, cls = NULL) {
	y = o[[x]]
	al = paste(aes, layer, sep = ".")



	if (any(al %in% names(y))) {
		id = which(al %in% names(y))[1] # take first, most specific layer, e.g. when layer = c("dots", "symbols"), take dots if exists
		z = y[[al[id]]]
	} else if (aes %in% names(y)) {
		# take matching visual variable (regardless what layer)
		z = y[[aes]]
	} else if (is.list(y)) {
		# check if there are non-named list items, if so take the first one
		eid = which(names(y) == "")[1]
		if (!is.na(eid)) {
			z = y[[eid]]
		} else {
			return(NA)
		}
	} else {
		return(y)
	}

	if (!is.null(cls) && is.list(z)) {
		mid = vapply(names(z), FUN = "%in%", FUN.VALUE = logical(1), cls)
		if (any(mid)) {
			z = z[[which(mid)[1]]]
		}
	}
	z
}

getAesValue = function(x, aes) {
	nms = names(x)

	if (is.null(nms)) {
		x
	} else if (any(nms %in% c("fill", "col", "size", "shape", "lwd", "lty", "fontsize", "fontface"))) {
		if (aes %in% nms) {
			x[[aes]]
		} else {
			if (any(nms == "")) {
				x[[which(nms == "")[1]]]
			} else {
				x
			}
		}
	} else {
		x
	}
}


# get options with a prefic
get_prefix_opt = function(prefix, class, o) {
	if (missing(prefix)) prefix = substr(class, 4, nchar(class))
	prefixdot = paste0(prefix, ".")
	ot = o[names(o)[substr(names(o), 1, nchar(prefixdot)) == prefixdot]]
	names(ot) = substr(names(ot), nchar(prefix)+2, nchar(names(ot)))
	ot
}

# (partly) named vector: get 1st name match or otherwise 1st non-named argument
# used in tm_scale_continuous, but similar function should exists for options? (todo: check)
get_vector_id = function(x, id) {
	if (is.null(names(x))) {
		x[1]
	} else if (id %in% names(x)) {
		unname(x[id][1L])
	} else if (any("" %in% names(x))) {
		unname(x[which(names(x) == "")[1]])
	} else {
		x[1]
	}
}

# raster.max_cell can be mode specific: e.g. c(plot = 3000, view = 1000, 1000),
max_cells = function(raster.max_cells) {
	mode = getOption("tmap.mode")

	if (mode %in% names(raster.max_cells)) {
		raster.max_cells[mode]
	} else {
		raster.max_cells[length(raster.max_cells)]
	}
}



