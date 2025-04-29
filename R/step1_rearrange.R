step1_rearrange = function(tmel) {
	message_init()
	dev = getOption("tmap.devel.mode")

	# get options (mode specific)
	o = tmap_options_mode(mode.specific = FALSE)

	# reset symbols
	assign("shapeLib", list(), envir = .TMAP)
	assign("justLib", list(), envir = .TMAP)

	# find shape, (aesthetic) layer, facet, and other elements
	is_tms = vapply(tmel, inherits, "tm_shape", FUN.VALUE = logical(1))
	is_tml = vapply(tmel, inherits, "tm_layer", FUN.VALUE = logical(1))
	is_tmf = vapply(tmel, inherits, "tm_facets", FUN.VALUE = logical(1))
	is_other = !is_tml & !is_tms & !is_tmf

	is_aux = vapply(tmel, inherits, "tm_aux_layer", FUN.VALUE = logical(1))
	is_proxy = vapply(tmel, inherits, "tm_proxy", FUN.VALUE = logical(1))

	# find layer id numbers (needed to plot layers in correct order, which is not trivial due to the two layer types)
	lay_id = cumsum(is_tml | is_aux)
	lay_id[!is_tml & !is_aux] = 0


	if (any(is_proxy)) {
		prx = tmel[is_proxy]
		proxy_z = lapply(prx, function(px) px$zindex)
	} else {
		prx = list()
		proxy_z = numeric(0)
	}

	# remove layers from q (via tm_remove_layer)
	# and determine highest pane number (400 default)
	if (.TMAP$proxy) {
		q = .TMAP$q
		.TMAP$pane_ids = setdiff(unique(c(.TMAP$pane_ids, q$lid)), 0)
		q = q[!(q$lid %in% proxy_z), ]
		.TMAP$q = q
		if (any(q$lid != 0)) {
			.TMAP$start_pane_id = max(q$lid)
		} else {
			.TMAP$start_pane_id = 400
		}
	} else {
		.TMAP$start_pane_id = 400
		.TMAP$pane_ids = NULL
	}

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
	crs_opt_called = FALSE
	if (any(is_opt)) for (id in which(is_opt)) {
		o2 = oth[[id]]

		if ("crs" %in% o2$calls) {
			crs_opt_called = TRUE
			o$crs = "" # to prevent merging list(dimensions = ... , ...) with leaflet crs
		}

		cls = class(o2)[1]
		if (substr(cls, nchar(cls)-2,nchar(cls)) == "_v3") cls = substr(cls, 1, nchar(cls) - 3)
		if ("v3_tm_legend" %in% o2$calls) v3_tm_legend_general(fun = cls)

		# special case: position, in case c("left", "top") is used
		pids = grep(".position", names(o2), fixed = TRUE)

		if (length(pids) > 0L) {
			for (i in 1L:length(pids)) {
				if (is.character(o2[[pids[i]]]) || is.numeric(o2[[pids[i]]])) o2[[pids[i]]] = tm_pos_in(o2[[pids[i]]][1], o2[[pids[i]]][2])
			}
		}

		if ("style" %in% names(o2) && !is.na(o2$style)) { #() {
			check_style(o2$style)
			o = tmap_options_mode(style = o2$style, mode.specific = FALSE)
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
				l$lid = if (is.na(l$zindex)) i + .TMAP$start_pane_id else l$zindex
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
		is_main_defined = !is.na(ids[1])
		if (!is_main_defined) ids = 1L

		tms = tmo[[ids[1]]]$tms


		# determine whether to animate
		tmf$show_gif_ani = is.null(o$animate_disable) && (tmf$animate || tmf$trans_animate) # animate is multiple variables/facets, trans_animate for transition animation only

		#if (tmf$show_gif_ani) o$scale = o$scale * 2

	} else {
		tmo = NULL
		tmf = tm_facets()[[1]]
		tmf$calls = NULL
		tmf$fn = c(1, 1, 1)
		tmf$n = 1
		tmf$fl = list(NULL, NULL, NULL)
		tmf$type = "wrap"
		tmf$npp = 1
		ids = 0

	}

	# add basemaps
	if (o$basemap.show && !.TMAP$proxy) {
		if (!any(vapply(oth, inherits, "tm_basemap", FUN.VALUE = logical(1)))) {
			oth = c(oth, tm_basemap())
			oth_lay_id = c(oth_lay_id, 0L)
		}
	}

	# check for disabled aux layers (so far only working for tm_basemap/tm_tiles):
	is_aux = vapply(oth, inherits, "tm_aux_layer", FUN.VALUE = logical(1))
	if (any(is_aux)) {

		aux = mapply(function(l, i) {
			l$lid = if (is.na(l$zindex)) i + .TMAP$start_pane_id else l$zindex

			cls = class(l)[1]
			ot = get_prefix_opt(class = cls, o = o)
			l$args = complete_options(l$args, ot)
			l
		}, oth[is_aux], oth_lay_id[is_aux], SIMPLIFY = FALSE)

		# disable aux layers (so far only working for tm_basemap/tm_tiles):
		sel_aux = local({
			cls1 = vapply(aux, function(ai) {
				class(ai)[1]
			}, FUN.VALUE = character(1))
			dis = vapply(aux, function(ai) {
				"disable" %in% names(ai$args) && ai$args$disable
			}, FUN.VALUE = logical(1))
			clsu = unique(cls1)
			sel_aux = rep(TRUE, length(aux))
			for (cl in clsu) {
				ids = which(cls1 == cl)
				d = dis[ids]
				if (any(d)) {
					latest = tail(which(d), 1)
					ids_dis = 1:latest
					sel_aux[ids[ids_dis]] = FALSE
				}
			}
			sel_aux
		})
		if (!any(sel_aux)) {
			#is_aux = FALSE
			aux = list()
		} else if (!all(sel_aux)) {
			aux = aux[sel_aux]
		}
	} else {
		aux = list()
	}


	crs_step4 = o$crs

	crs_opt_defined = !is.na(crs_step4[1])

	# if basemaps are used AND tm_options(crs = ...) is not used AND tm_shape(crs = ... / is.main = TRUE) is not used:
	basemaps_defined = length(aux) && any(vapply(aux, inherits, c("tm_basemap", "tm_tiles"), FUN.VALUE = logical(1)))
	if ((is.na(crs_step4[1]) || identical(crs_step4, "auto")) && basemaps_defined && !crs_opt_called) {
		crs_step4 = o$crs_basemap
	}

	crs_step3 = if (any_data_layer) get_crs(tms, is_auto = identical(crs_step4, "auto"), crs_extra = o$crs_extra, crs_global = o$crs_global, basemaps_defined = basemaps_defined) else NA

	if (identical(crs_step4, "auto")) {
		if (is.na(crs_step3[1])) {
			# no data layer:
			crs_step4 = auto_crs(TRUE, crs_extra = o$crs_extra, crs_global = o$crs_global)
		} else {
			crs_step4 = crs_step3
		}
	}

	if (inherits(crs_step3, "leaflet_crs")) crs_step3 = leaflet2crs(crs_step3)

	if (inherits(crs_step4, "leaflet_crs")) {
		crs_leaflet = crs_step4
		crs_step4 = sf::st_crs(4326) #leaflet2crs(crs_leaflet)
	} else if (any_data_layer && (is.na(crs_step4[1]) || (is.numeric(crs_step4) && crs_step4 == 0))) {#  || (is_main_defined && !crs_opt_called))) {
		crs_step4 = if (inherits(crs_step3, "leaflet_crs")) leaflet2crs(crs_step3) else crs_step3
		crs_leaflet = leafletSimple
	} else if (is.na(crs_step4[1])) {
		crs_step4 = sf::st_crs(4326)
		crs_leaflet = crs2leaflet(get_option_class(crs_step4, "dimensions"))
	} else {
		crs_leaflet = crs2leaflet(get_option_class(crs_step4, "dimensions"))
	}

	if (any_data_layer) {
		main_class = get_class(tms)
	} else {
		main_class = "stars" # basemaps
	}

	if (dev) timing_add(s2 = "facet meta")

	#po(crs_step4, crs_step3, crs_leaflet)






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
	o$crs_step4 = crs_step4 # in step 3, when other shapes are transformed to this crs (prepare for plotting in step4)
	o$crs_leaflet = crs_leaflet
	o$crs_step3 = crs_step3 # step 3, transformation


	#po(crs_step3, crs_step4)

	o = c(o, tmf)
	# process shapes: put non-spatial data in data.table, keep spatial data separately


	# disable s2 in case earth.boundaries are drawn
	if (o$earth_boundary && sf::sf_use_s2()) {
		suppressMessages(sf::sf_use_s2(FALSE))
		.TMAP$set_s2 = TRUE
	}

	if (any_data_layer) {
		tmo = structure(lapply(tmo, function(tmg) {
			tmg$tms = do.call(tmapShape, c(tmg$tms, list(o = o, tmf = tmg$tmf)))
			tmg
		}), names = paste0("group", seq_len(length(tmo))), class = c("tmapObject", "list"))
	}

	if (dev) timing_add(s2 = "prep shape")

	if (any_data_layer && !is.null(tms$unit)) o$unit = tms$unit

	list(tmo = tmo, aux = aux, cmp = cmp, prx = prx, o = o)
}

# see above
get_main_ids = function(tmo) {
	is_main = vapply(tmo, function(tmg) {
		identical(tmg$tms$is.main, TRUE)
	}, FUN.VALUE = logical(1), USE.NAMES = FALSE)

	if (any(is_main)) which(is_main) else NA_integer_
}


get_crs = function(tms, is_auto, crs_extra, crs_global, basemaps_defined) {
	if (is.na(sf::st_crs(tms$shp))) return(sf::st_crs(NA))
	if (is.null(tms$crs)) {
		crs = sf::st_crs(tms$shp)
		is_ll = sf::st_is_longlat(crs)
		if (is_ll && is_auto) {
			auto_crs(tms$shp, crs_extra = crs_extra, crs_global = crs_global)
		} else {
			if (is_ll && !basemaps_defined) {
				if (inherits(tms$shp, "sf") && consider_global(tms$shp)) {
					message_crs_ll()
				}

			}
			crs
		}
	} else {
		crs = tms$crs
		if (identical(crs, "auto") || is_auto) {
			auto_crs(tms$shp, crs_extra = crs_extra, crs_global = crs_global)
		} else crs
	}
}

get_class = function(tms) {
	class(tms$shp)
}

str2pos = function(x) {
	if (length(x) == 1L) {
		if (tolower(x) %in% c("left", "right")) {
			args = list(pos.h = x)
		} else if (tolower(x) %in% c("top", "bottom")) {
			args = list(pos.v = x)
		} else if (tolower(x) %in% c("center", "centre")) {
			args = list(pos.h = "center", pos.v = "center")
		} else {
			stop("position: incorrect specification")
		}
	} else {
		if (tolower(x[1]) %in% c("top", "bottom")) {
			args = list(pos.h = x[2], pos.v = x[1])
		} else {
			args = list(pos.h = x[1], pos.v = x[2])
		}
	}
	do.call(tm_pos_in, args)
}

num2pos = function(x) {
	if (length(x) == 1L) stop("position: please provide two values: the horizontal and the vertical")
	tm_pos_in(x[1], x[2])
}

process_position = function(position, o) {
	if (is.null(position)) return(NULL)

	# special case: position, in case c("left", "top") is used
	if (is.character(position)) position = str2pos(position)
	if (is.numeric(position)) position = num2pos(position)
	if (inherits(position, "tm_pos")) {
		position = complete_options(position, o$component.position[[position$type]])
		if (position$type %in% c("autoin", "autoout")) message_pos_auto(position$type)
	} else {
		stop("position should be a tm_pos object")
	}

	# checking
	# with(position, {
	# 	if (cell.h %in% c(""))
	# })

	within.list(position, {
		if (exists("cell.h")) cell.h = check_h(cell.h, "cell")
		if (exists("cell.v")) cell.v = check_v(cell.v, "cell", h_is_num = is.numeric(cell.h))
		if (exists("pos.h")) pos.h = check_h(pos.h, "pos")
		if (exists("pos.v")) pos.v = check_v(pos.v, "pos", h_is_num = is.numeric(pos.h))
		if (exists("align.h")) align.h = check_h(align.h, "align")
		if (exists("align.v")) align.v = check_v(align.v, "align", h_is_num = is.numeric(align.h))
		if (exists("just.h")) just.h = check_h(just.h, "just")
		if (exists("just.v")) just.v = check_v(just.v, "just", h_is_num = is.numeric(just.h))
	})

}

process_padding = function(padding) {
	if (is.null(padding)) return(NULL)
	rep(padding, length.out = 4)
}

check_h = function(x, var) {
	if (is.numeric(x)) {
		x
	} else {
		y = tolower(x)
		is_upper = (x != y)
		y2 = if (y %in% c("center", "centre")) {
			"center"
		} else if (y %in% c("left", "right")) {
			y
		} else {
			stop("position arguments: incorrect ", var, ".h; it should be 'left', 'center', 'right' or a numeric value", call. = FALSE)
		}
		if (is_upper) toupper(y2) else y
	}
}

check_v = function(x, var, h_is_num) {
	if (is.numeric(x)) {
		if (!h_is_num) stop("position argument ", var, ".h and .v should both be numeric or character values", call. = FALSE)
		x
	} else {
		y = tolower(x)
		is_upper = (x != y)
		if (h_is_num) stop("position argument ", var, ".h and .v should both be numeric or character values", call. = FALSE)
		y2 = if (y %in% c("center", "centre")) {
			"center"
		} else if (y %in% c("top", "bottom")) {
			y
		} else {
			stop("position argument: incorrect ", var, ".v; it should be 'top', 'center', 'bottom' or a numeric value", call. = FALSE)
		}
		if (is_upper) toupper(y2) else y
	}
}
