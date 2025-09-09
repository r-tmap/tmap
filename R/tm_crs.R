#' Set the map projection (CRS)
#'
#' This function sets the map projection. It can also be set via [tm_shape()], but `tm_crs` is generally recommended for most cases. It can also be determined automatically (see details); however, this is still work-in-progress.
#'
#' @param crs Map projection (CRS). Can be set to an `crs` object (see [sf::st_crs()]), a proj4string, an EPSG number, the value `"auto"` (automatic crs recommendation), or one the the following generic projections: `c("laea", "aeqd", "utm", "pconic", "eqdc", "stere")`. See details.
#' @param property Which property should the projection have? One of: `"global"`, `"area"` (equal-area), `"distance"` (equidistant), `"shape"` (conformal). Only applicable if `crs = "auto"`. See details.
#' @param bbox bounding box. Three options: a [sf::st_bbox()] object, an Open Street Map query (passed on to [tmaptools::geocode_OSM()]), or `"FULL"`, which means the whole earth , which means the whole earth (this also guarantees that transformations to another CRS keep the whole earth, unlike \code{\link[sf:st_bbox]{sf::st_bbox()}}).
#' @note Plans are to migrate the functionality regarding generic crs and automatic crs recommendation to a separate package.
#' @inherit tm_shape details
#' @example ./examples/tm_crs.R
#' @seealso \href{https://r-tmap.github.io/tmap/articles/foundations_crs}{vignette about CRS}
#' @export
tm_crs = function(crs = NA, property = NA, bbox = NULL) {
	if (is.na(crs)) {
		if (is.na(property)) {
			return(NULL)
		} else {
			crs = "auto"
		}
	}
	if (identical(crs, "auto") && !is.na(property)) {
		if (!(property %in% c("global", "area", "distance", "shape"))) {
			message_crs_property_unknown()
		}
		extra = property
	} else {
		extra = ""
	}

	if (is.character(crs) && crs %in% c("laea", "aeqd", "utm", "pconic", "eqdc")) {
		extra = crs
		crs = "auto"
	}

	tm_options(crs = crs, crs_extra = extra, bbox = bbox)
}


consider_global = function(x, th = 0.6) {
	b = sf::st_bbox(x)
	# in case margins are applied
	if (sf::st_is_longlat(b)) {
		b["xmin"] = max(b["xmin"], -180)
		b["xmax"] = min(b["xmax"], 180)
		b["ymin"] = max(b["ymin"], -90)
		b["ymax"] = min(b["ymax"], 90)
	}
	if (b$xmin == b$xmax || b$ymin == b$ymax) return(FALSE)
	earth_surface = 5.1e14
	area = b |>
		sf::st_as_sfc() |>
		sf::st_area() |>
		as.numeric()
	area > (earth_surface * 0.6)
}

auto_crs = function(x, crs_extra, crs_global) {

	if (identical(x, TRUE)) return(crs_global)




    # check if global should be used
	is_global = if (crs_extra == "global") {
		TRUE
	} else if (crs_extra != "") {
		FALSE
	} else {
		consider_global(x)
	}

	y = if (is_global) {
		crs_global
	} else {
		# impute family
		proj = switch(crs_extra,
			area = "laea",
			distance = "aeqd",
			shape = "stere",
			"laea"
		)
		to_generic_projected(x, proj = proj, return_as = "crs")
	}
	sf::st_crs(y)
}


to_generic_projected <- function(
		x,
		proj = c("laea", "aeqd", "utm", "pconic", "eqdc", "stere"),
		ellps = "WGS84",
		no_defs = TRUE,
		opts = "",
		return_as = c("sf", "proj4", "wkt", "crs")) {
	# arg assertions
	if (!rlang::is_true(rlang::inherits_any(x, c("sf", "sfc", "stars")))) {
		rlang::abort(
			"`x` must be either an sf object or an sfc object or a stars object",
			class = "to_generic_projected_incorrect_input"
		)
	}

	proj <- rlang::arg_match(proj)
	ellps <- rlang::arg_match(ellps, sf::sf_proj_info(type = "ellps")$name)

	if (!rlang::is_logical(no_defs)) {
		rlang::abort("`no_defs` must be a logical value",
					 class = "to_generic_projected_incorrect_input"
		)
	}

	if (!rlang::is_character(opts) && !nchar(opts)) {
		rlang::abort("`opts` must be a character vector",
					 class = "to_generic_projected_incorrect_input"
		)
	}

	return_as <- rlang::arg_match(return_as)

	# was centroid
	cent_coor <- sf::sf_project(
		sf::st_crs(x), "EPSG:4326",
		sf::st_bbox(x) |>
			sf::st_as_sfc() |>
			sf::st_centroid() |>
			sf::st_coordinates()
	)

	# configure proj args
	n_or_s <- ifelse(cent_coor[2] == 0, "",
					 ifelse(cent_coor[2] > 0, "+north", "+south")
	)

	no_defs <- ifelse(no_defs, "+no_defs", "")

	if (proj %in% c("pconic", "eqdc")) {
		bounds <- sf::st_bbox(sf::st_transform(x, 4326))
		lat_1 <- bounds$ymax
		lat_2 <- bounds$ymin
	}

	# base R replacement of glue::glue to reduce dependencies
	glue = function(..., .sep) {
		args = list(...)
		paste(lapply(args, gluestick, src = parent.frame()), collapse = .sep)
	}


	# construct proj4 string
	prj <- trimws(switch(proj,
						 laea = glue(
						 	"+proj=laea +lon_0={cent_coor[1]} +lat_0={cent_coor[2]}",
						 	"+ellps={ellps} {no_defs}",
						 	opts,
						 	.sep = " "
						 ),
						 utm = glue(
						 	"+proj=utm +zone={round((180 + cent_coor[1]) / 6)} {n_or_s}",
						 	"+ellps={ellps} {no_defs}",
						 	opts,
						 	.sep = " "
						 ),
						 aeqd = glue(
						 	"+proj=aeqd +lon_0={cent_coor[1]} +lat_0={cent_coor[2]}",
						 	"+ellps={ellps} {no_defs}",
						 	opts,
						 	.sep = " "
						 ),
						 pconic = glue(
						 	"+proj=pconic +lon_0={cent_coor[1]} +lat_0={cent_coor[2]}",
						 	"+lat_1={lat_1} +lat_2={lat_2}",
						 	"+ellps={ellps} {no_defs}",
						 	opts,
						 	.sep = " "
						 ),
						 eqdc = glue(
						 	"+proj=eqdc +lon_0={cent_coor[1]}",
						 	"+lat_1={lat_1} +lat_2={lat_2}",
						 	"+ellps={ellps} {no_defs}",
						 	opts,
						 	.sep = " "
						 ),
						 stere = glue(
						 	"+proj=stere +lon_0={cent_coor[1]} +lat_0={cent_coor[2]}",
						 	"+ellps={ellps} {no_defs}",
						 	opts,
						 	.sep = " "
						 )
	))

	switch(return_as,
		   sf = sf::st_transform(x, prj),
		   proj4 = prj,
		   wkt = sf::st_crs(prj)$wkt,
		   crs = sf::st_crs(prj)
	)
}




# Source: https://github.com/coolbutuseless/gluestick
gluestick <- function(fmt, src = parent.frame(), open = "{", close = "}", eval = TRUE) {

	nchar_open  <- nchar(open)
	nchar_close <- nchar(close)

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# Sanity checks
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stopifnot(exprs = {
		is.character(fmt)
		length(fmt) == 1L
		is.character(open)
		length(open) == 1L
		nchar_open > 0L
		is.character(close)
		length(close) == 1
		nchar_close > 0
	})

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# Brute force the open/close characters into a regular expression for
	# extracting the expressions from the format string
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	open  <- gsub("(.)", "\\\\\\1", open ) # Escape everything!!
	close <- gsub("(.)", "\\\\\\1", close) # Escape everything!!
	re    <- paste0(open, ".*?", close)

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# Extract the delimited expressions
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	matches  <- gregexpr(re, fmt)
	exprs    <- regmatches(fmt, matches)[[1]]

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# Remove the delimiters
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	exprs <- substr(exprs, nchar_open + 1L, nchar(exprs) - nchar_close)

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# create a valid sprintf fmt string.
	#  - replace all "{expr}" strings with "%s"
	#  - escape any '%' so sprintf() doesn't try and use them for formatting
	#    but only if the '%' is NOT followed by an 's'
	#
	# gluestick() doesn't deal with any pathological cases
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	fmt_sprintf <- gsub(re      , "%s", fmt)
	fmt_sprintf <- gsub("%(?!s)", "%%", fmt_sprintf, perl=TRUE)

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# Evaluate
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	if (eval) {
		args <- lapply(exprs, function(expr) {eval(parse(text = expr), envir = src)})
	} else {
		args <- unname(mget(exprs, envir = as.environment(src)))
	}

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# Create the string(s)
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	do.call(sprintf, c(list(fmt_sprintf), args))
}
