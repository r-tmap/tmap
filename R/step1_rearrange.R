step1_rearrange = function(tmel) {
	dev = getOption("tmap.devel.mode")
	
	# get options (mode specific)
	o = tmap_options_mode()
	
	# reset symbols
	assign("shapeLib", list(), envir = .TMAP)
	assign("justLib", list(), envir = .TMAP)
	
	
	# find shape, (aesthetic) layer, facet, and other elements
	is_tms = vapply(tmel, inherits, "tm_shape", FUN.VALUE = logical(1))
	is_tml = vapply(tmel, inherits, "tm_layer", FUN.VALUE = logical(1))
	is_tmf = vapply(tmel, inherits, "tm_facets", FUN.VALUE = logical(1))
	is_other = !is_tml & !is_tms & !is_tmf

	is_aux = vapply(tmel, inherits, "tm_aux_layer", FUN.VALUE = logical(1))
	
	# find layer id numbers (needed to plot layers in correct order, which is not trivial due to the two layer types)
	lay_id = cumsum(is_tml | is_aux)
	lay_id[!is_tml & !is_aux] = 0
	
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
	
	
	# warning("v3 code detected")
	# args$v3 = NULL
	# names(args) = paste0("legend.", names(args))
	# do.call(tm_options, args)
	# 
	
	# update options with tm_option elements
	#sapply(oth, inherits, "tm_legend")
	
	is_opt = sapply(oth, inherits, "tm_options")
	if (any(is_opt)) for (id in which(is_opt)) {
		o2 = oth[[id]]
		
		if ("v3" %in% o2$calls) message("v3 code detected: as of tmap v4, the legend should be specified for each visual variable\n(e.g. with `fill.legend = tm_legend()` inside tm_polygons to control the legend of fill for polygons.", call. = FALSE)
		
		# special case: position, in case c("left", "top") is used
		pids = grep(".position", names(o2), fixed = TRUE)
		
		if (length(pids) > 0L) {
			for (i in 1L:length(pids)) {
				if (is.character(o2[[pids[i]]])) o2[[pids[i]]] = tm_pos_in(o2[[pids[i]]][1], o2[[pids[i]]][2])
			}
		}

		if ("style" %in% names(o2) && !is.na(o2$style)) { #() {
			o = tmap_options_mode(default.options = TRUE)
			styleOptions <- get("tmapStyles", envir = .TMAP)[[o2$style]]
			if (!is.null(styleOptions)) o = complete_options(styleOptions, o)
			o2$style = NULL
		}
		o = complete_options(o2, o)
	}
	o = preprocess_meta_step1(o)
	
	
	any_data_layer = length(tmel_spl) > 0L
	
	if (any_data_layer) {
		# organize groups, 1 tm_shape, at least 1 tm_layers, 1 tm_facets
		tmo = mapply(function(tmg, lid) {
			is_tms = vapply(tmg, inherits, "tm_shape", FUN.VALUE = logical(1))
			is_tml = vapply(tmg, inherits, "tm_layer", FUN.VALUE = logical(1))
			is_tmf = vapply(tmg, inherits, "tm_facets", FUN.VALUE = logical(1))
			
			# make sure there is exactly one tm_facets per group (if there are none, add one, if there are mutple, take last)
			if (!any(is_tmf)) {
				tmf = tm_facets()[[1]]
				tmf$calls = NULL
			} else {
				# get last tm_facets element
				k = sum(is_tmf)
				if (k < 1) warning("Multiple tm_facets defined per layer group. Only the last one is processed", call. = FALSE)
				tmf = tmg[[which(is_tmf)[k]]]
			}
			
			# extract layers and add layer id number
			tmls = mapply(function(l, i) {
				l$lid = if (is.na(l$zindex)) i else l$zindex
				l
			}, tmg[is_tml], lid[is_tml], SIMPLIFY = FALSE)
			
			structure(list(tms = tmg[[1]], tmls = tmls, tmf = tmf), class = c("tmapGroup", "list"))
		}, tmel_spl, lay_id_spl, SIMPLIFY = FALSE)
		
		tmo = step1_rearrange_facets(tmo, o) # save smeta's and keep track of group id (to obtain smeta)
		tmf = tmo$tmf_global # global facetting options, to be appended to options o
		tmo$tmf_global = NULL
		
		# find the 'main' group: this is the group for which tm_shape is used for CRS and bbox. By default, take the first, unless is.main is set to TRUE.
		# is.main can be set multiple times: the CRS will be taken from the first, but the bbox from all
		ids = get_main_ids(tmo)
		
		tms = tmo[[ids[1]]]$tms
		
		
		
		
	} else {
		tmo = NULL
		tmf = tm_facets()[[1]]
		tmf$calls = NULL
		tmf$fn = c(1, 1, 1)
		tmf$n = 1
		tmf$fl = list(NULL, NULL, NULL)
		tmf$type = "wrap"
		ids = 0
		
	}
	
	# crs in options refers to which crs is used in the plot, not necessarily in the transformation (step 3) 
	crs_option = o$crs
	
	# get main crs (used in step 3, not necessarily in the plot (e.g. view mode will use 4326/3857))
	crs_main = if (any_data_layer) get_crs(tms) else NA
	
	if (inherits(crs_option, "leaflet_crs")) {
		crs_leaflet = crs_option
		crs = leaflet2crs(crs_leaflet)
	} else if (any_data_layer && (is.na(crs_option[1]) || (is.numeric(crs_option) && crs_option == 0))) {
		crs = crs_main
		crs_leaflet = leafletSimple
	} else if (is.na(crs_option[1])) {
		crs = sf::st_crs(4326)
		crs_leaflet = crs2leaflet(get_option_class(crs, "dimensions"))
	} else {
		crs = crs_option
		crs_leaflet = crs2leaflet(get_option_class(crs, "dimensions"))
	}
	
	if (any_data_layer) {
		main_class = get_class(tms)
	} else {
		main_class = "stars" # basemaps
	}
	
	if (dev) timing_add(s2 = "facet meta")
	
	
	# # add basemaps
	if (o$basemap.show) {
		if (!any(vapply(oth, inherits, "tm_basemap", FUN.VALUE = logical(1)))) {
			oth = c(oth, tm_basemap())
			oth_lay_id = c(oth_lay_id, 0L)
		}
	}

	is_aux = vapply(oth, inherits, "tm_aux_layer", FUN.VALUE = logical(1))

	if (any(is_aux)) {
		
		aux = mapply(function(l, i) {
			l$lid = if (is.na(l$zindex)) i else l$zindex
			
			cls = class(l)[1]
			ot = get_prefix_opt(class = cls, o = o)
			l$args = complete_options(l$args, ot)
			l
		}, oth[is_aux], oth_lay_id[is_aux], SIMPLIFY = FALSE)
	} else {
		aux = list()
	}
	

	is_comp = sapply(oth, inherits, "tm_component")
	if (any(is_comp)) {
		cmp = oth[is_comp]
		cmp = lapply(cmp, impute_comp, o = o)
	} else {
		cmp = list()
	}
	
	# to be used later
	o$main = ids # to determine total bounding box in step 4
	o$main_class = main_class # inner.margins are class specific (preprecess_meta)
	o$crs = crs # in step 3, when other shapes are transformed to this crs
	o$crs_leaflet = crs_leaflet
	o$crs_main = crs_main
	
	
	o = c(o, tmf)
	# process shapes: put non-spatial data in data.table, keep spatial data separately 
	
	if (any_data_layer) {
		tmo = structure(lapply(tmo, function(tmg) {
			tmg$tms = do.call(tmapShape, c(tmg$tms, list(o = o, tmf = tmg$tmf)))
			tmg
		}), names = paste0("group", seq_len(length(tmo))), class = c("tmapObject", "list"))
	}
	
	if (dev) timing_add(s2 = "prep shape")
	
	list(tmo = tmo, aux = aux, cmp = cmp, o = o)
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

impute_comp = function(a, o) {
	cls = class(a)[1]
	
	ot = get_prefix_opt(class = cls, o = o)
	
	ca = class(a)
	
	# special case: position, in case c("left", "top") is used
	if (is.character(a$position)) a$position = str2pos(a$position)
	if (is.numeric(a$position)) a$position = num2pos(a$position)
	if (inherits(a$position, "tm_pos")) a$position = complete_options(a$position, o$component.position[[a$position$type]])
	
	
	a = complete_options(a, ot)
	
	class(a) = ca
	a
}


