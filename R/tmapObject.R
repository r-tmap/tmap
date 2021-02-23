#' @param tmel tm_element_list
tmapObject = function(tmel) {
	# find shapes and layers
	is_tms = sapply(tmel, inherits, "tm_shape")
	is_tml = sapply(tmel, inherits, "tm_layer")
	is_tmf = sapply(tmel, inherits, "tm_facets")
	ids = cumsum(is_tms)
	ids[!is_tml & !is_tms & !is_tmf] = 0
	
	# create groups, for each group: tms (tmap shape), tmls (tmap layers), tmf (tmap facets)
	tmel_spl = split(tmel, f = ids)
	tmo = lapply(tmel_spl, function(tmg) {
		is_tms = sapply(tmg, inherits, "tm_shape")
		is_tml = sapply(tmg, inherits, "tm_layer")
		is_tmf = sapply(tmg, inherits, "tm_facets")
		if (!any(is_tmf)) {
			tmf = tm_facets_wrap()[[1]]
		} else {
			# get last tm_facets element
			k = sum(is_tmf)
			if (k < 1) warning("Multiple tm_facets defined per layer group. Only the last one is processed", call. = FALSE)
			tmf = tmg[[which(is_tmf)[k]]]
		}
		
		structure(list(tms = tmg[[1]], tmls = tmg[is_tml], tmf = tmf), class = c("tmapGroup", "list"))
	})
	
	# get the crs of the main shape
	crs = get_main_crs(tmo)
	
	# reproject other shapes if needed
	tmo = structure(lapply(tmo, function(tmg) {
		tmg$tms$crs = crs
		tmg$tms = do.call(tmapShape, tmg$tms)
		#dt = tmg$tms$dt
		#tmg$tmls = lapply(tmg$tmls, tmapLayer, dt)
		tmg
	}), names = paste0("group", seq_len(length(tmo))), class = c("tmapObject", "list"))
	
	tmo
}


# helper function to find the main crs: 
# - it checks which tm_shape is "main" (the first that is set to is.main, and if none, the first)
# - for the main tm_shape, it gets the crs attribute, and if not specified, it extracts the crs from the shape object
get_main_crs = function(tmo) {
	is_main = vapply(tmo, function(tmg) {
		identical(tmg$tms$is.main, TRUE)
	}, FUN.VALUE = logical(1))
	
	main_id = if (any(is_main)) which(is_main)[1L] else 1L
	
	tms_main = tmo[[main_id]]$tms
	
	crs_main = tms_main$crs
	if (is.null(crs_main)) crs_main = sf::st_crs(tms_main$shp)
	crs_main
}
