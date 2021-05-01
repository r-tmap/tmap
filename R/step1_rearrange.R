#' @param tmel tmap object
step1_rearrange = function(tmel) {
	# find shapes and layers
	is_tms = sapply(tmel, inherits, "tm_shape")
	is_tml = sapply(tmel, inherits, "tm_layer")
	is_tmf = sapply(tmel, inherits, "tm_facets")
	is_other = !is_tml & !is_tms & !is_tmf
	ids = cumsum(is_tms)
	ids[is_other] = 0
	
	# create groups, for each group: tms (tmap shape), tmls (tmap layers), tmf (tmap facets)
	tmel_spl = split(tmel, f = ids)
	
	if (any(is_other)) {
		oth = tmel_spl[[1]]
		tmel_spl = tmel_spl[-1]
	} else {
		oth = list()		
	}
	
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
	
	ids = get_main_ids(tmo)
	
	crs_option = tmap_graphics()$crs
	crs = if (is.na(crs_option[1])) get_crs(tmo[[ids[1]]]$tms) else crs_option

	# reproject other shapes if needed
	tmo = structure(lapply(tmo, function(tmg) {
		#tmg$tms$crs = crs
		tmg$tms = do.call(tmapShape, tmg$tms)
		#dt = tmg$tms$dt
		#tmg$tmls = lapply(tmg$tmls, tmapLayer, dt)
		tmg
	}), names = paste0("group", seq_len(length(tmo))), class = c("tmapObject", "list"))
	
	#tmfs = lapply(tmo, "[[", "tmf")
	
	
	opt = tmap_options()
	is_opt = sapply(oth, inherits, "tm_options")
	if (any(is_opt)) for (id in which(is_opt)) {
		nms = intersect(names(opt), names(oth[[id]]))
		if (length(nms)) opt[nms] = oth[[id]][nms]
	}
	
	meta = c(opt, list(main = ids, crs = crs))
	list(tmo = tmo, meta = meta)
}

get_main_ids = function(tmo) {
	is_main = vapply(tmo, function(tmg) {
		identical(tmg$tms$is.main, TRUE)
	}, FUN.VALUE = logical(1), USE.NAMES = FALSE)
	
	if (any(is_main)) which(is_main) else 1L
}
get_crs = function(tms) {
	if (is.null(tms$crs)) sf::st_crs(tms$shp) else tms$crs
}
