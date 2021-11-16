step1_rearrange = function(tmel) {
	# find shape, (aesthetic) layer, facet, and other elements
	is_tms = vapply(tmel, inherits, "tm_shape", FUN.VALUE = logical(1))
	is_tml = vapply(tmel, inherits, "tm_layer", FUN.VALUE = logical(1))
	is_tmf = vapply(tmel, inherits, "tm_facets", FUN.VALUE = logical(1))
	is_other = !is_tml & !is_tms & !is_tmf

	is_aux = vapply(tmel, inherits, "tm_aux_layer", FUN.VALUE = logical(1))
	
	# find layer id numbers (needed to plot layers in correct order, which is not trivial due to the two layer types)
	lay_id = cumsum(is_tml | is_aux)
	lay_id[!is_tml & !is_aux] = 0L
	
	# create groups, for each group: tms (tmap shape), tmls (tmap layers), tmf (tmap facets)
	ids = cumsum(is_tms)
	ids[is_other] = 0L
	tmel_spl = split(tmel, f = ids)
	lay_id_spl = split(lay_id, f = ids)
	if (any(is_other)) {
		oth = tmel_spl[[1]]
		oth_lay_id = lay_id_spl[[1]]
		tmel_spl = tmel_spl[-1]
		lay_id_spl = lay_id_spl[-1]
	} else {
		oth = list()
		oth_lay_id = list()
	}
	
	# organize groups, 1 tm_shape, at least 1 tm_layers, 1 tm_facets
	tmo = mapply(function(tmg, lid) {
		is_tms = vapply(tmg, inherits, "tm_shape", FUN.VALUE = logical(1))
		is_tml = vapply(tmg, inherits, "tm_layer", FUN.VALUE = logical(1))
		is_tmf = vapply(tmg, inherits, "tm_facets", FUN.VALUE = logical(1))
		
		# make sure there is exactly one tm_facets per group (if there are none, add one, if there are mutple, take last)
		if (!any(is_tmf)) {
			tmf = tm_facets()[[1]]
		} else {
			# get last tm_facets element
			k = sum(is_tmf)
			if (k < 1) warning("Multiple tm_facets defined per layer group. Only the last one is processed", call. = FALSE)
			tmf = tmg[[which(is_tmf)[k]]]
		}
		
		# extract layers and add layer id number
		tmls = mapply(function(l, i) {
			l$lid = i
			l
		}, tmg[is_tml], lid[is_tml], SIMPLIFY = FALSE)
		
		structure(list(tms = tmg[[1]], tmls = tmls, tmf = tmf), class = c("tmapGroup", "list"))
	}, tmel_spl, lay_id_spl, SIMPLIFY = FALSE)
	
	
	is_aux = vapply(oth, inherits, "tm_aux_layer", FUN.VALUE = logical(1))
	
	if (any(is_aux)) {
		aux = mapply(function(l, i) {
			l$lid = i
			l
		}, oth[is_aux], oth_lay_id[is_aux], SIMPLIFY = FALSE)
	} else {
		aux = list()
	}
	
	
	
	# get options (mode specific)
	opt = tmap_options_mode()
	
	
	tmo = step1_rearrange_facets(tmo) # save smeta's and keep track of group id (to obtain smeta)
	
	tmf = tmo[[1]]$tmf # first tmf contains global settings
	

	
# 
# 	fl  = list(1L, 1L, 1L)
# 	for (tmg in tmo) {
# 		
# 	}
	 
	
	# find the 'main' group: this is the group for which tm_shape is used for CRS and bbox. By default, take the first, unless is.main is set to TRUE.
	# is.main can be set multiple times: the CRS will be taken from the first, but the bbox from all
	ids = get_main_ids(tmo)
	
	
	# get main crs (option or extracted from first main shape)
	crs_option = opt$crs
	tms = tmo[[ids[1]]]$tms
	crs = if (is.na(crs_option[1])) get_crs(tms) else crs_option
	main_class = get_class(tms)

	# update options with tm_option elements
	is_opt = sapply(oth, inherits, "tm_options")
	if (any(is_opt)) for (id in which(is_opt)) {
		nms = intersect(names(opt), names(oth[[id]]))
		if (length(nms)) opt[nms] = oth[[id]][nms]
	}

	# to be used later
	opt$main = ids # to determine total bounding box in step 4
	opt$main_class = main_class # inner.margins are class specific (preprecess_meta)
	opt$crs = crs # in step 3, when other shapes are transformed to this crs
	
	# process shapes: put non-spatial data in data.table, keep spatial data separately 
	tmo = structure(lapply(tmo, function(tmg) {
		tmg$tms = do.call(tmapShape, c(tmg$tms, list(o = opt)))
		tmg
	}), names = paste0("group", seq_len(length(tmo))), class = c("tmapObject", "list"))
	
	list(tmo = tmo, aux = aux, meta = opt)
}

# see above
get_main_ids = function(tmo) {
	is_main = vapply(tmo, function(tmg) {
		identical(tmg$tms$is.main, TRUE)
	}, FUN.VALUE = logical(1), USE.NAMES = FALSE)
	
	if (any(is_main)) which(is_main) else 1L
}


get_crs = function(tms) {
	if (is.null(tms$crs)) sf::st_crs(tms$shp) else tms$crs
}

get_class = function(tms) {
	class(tms$shp)
}


