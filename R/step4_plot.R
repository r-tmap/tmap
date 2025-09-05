pane_name = function(id) {
	paste0("tmap", sprintf("%03d", id))
}

process_components = function(cdt, o) {
	gs = tmap_graphics_name()


	# update added legends
	is_added_leg = vapply(cdt$comp, inherits, logical(1), "tm_add_legend")

	if (any(is_added_leg)) {
		cdt$comp[is_added_leg] = lapply(cdt$comp[is_added_leg], tmapAddedLegend, o = o)
	}

	# the option component.position is not a general fall-back option (in case the component is undefined for a mode),
	# but used for imputation of not-specified tm_pos arguments
	# in case there is no specific .position argument for the current mode specified, use the 'in' position for the time being
	# the missing methods for the component is catched later via the missing tmap<gs>CompPrepare method
	cdt$comp = lapply(cdt$comp, function(l) {
		if ("in" %in% names(l$position)) l$position = l$position[["in"]]
		l
	})

	cdt$class = sapply(cdt$comp, function(l) l$position$type)
	cdt$cell.h = sapply(cdt$comp, function(l) {x = l$position$cell.h; if (is.null(x)) NA else x})
	cdt$cell.v = sapply(cdt$comp, function(l) {x = l$position$cell.v; if (is.null(x)) NA else x})
	cdt$pos.h = sapply(cdt$comp, function(l) {x = l$position$pos.h; if (is.null(x)) NA else x})
	cdt$pos.v = sapply(cdt$comp, function(l) {x = l$position$pos.v; if (is.null(x)) NA else x})
	cdt$z = sapply(cdt$comp, function(l) {x = l$z; if (is.null(x)) NA_integer_ else x})
	cdt[, zauto := .I]

	if (gs != "Grid") {
		cdt[class == "out", class := "in"]
		cdt[class == "autoout", class := "in"]
	}

	funP = paste0("tmap", gs, "CompPrepare")
	funW = paste0("tmap", gs, "CompWidth")
	funH = paste0("tmap", gs, "CompHeight")

	cdt$comp = lapply(cdt$comp, function(comp) do.call(funP, list(comp = comp, o = o)))

	# split components into facets (if needed)
	is_multi = sapply(cdt$comp, inherits, what = "tm_multi_comp")

	if (any(is_multi)) {
		cdt_single = cdt[!is_multi, ]
		cdt_multi = cdt[is_multi, ]

		allby_temp = as.data.table(do.call(expand.grid, lapply(o$fn, seq_len)))
		names(allby_temp) = paste0("by", 1:3, "__")

		cdt_rows = data.table::rbindlist(lapply(seq_len(nrow(cdt_multi)), function(i) {
			if (cdt_multi$class[i] %in% c("out", "autoout")) {
				if (length(cdt_multi$comp[[i]]) > 1) {
					warning("Component is specified for multiple facets (e.g. title text), but since placement is outside, only the first is used", call. = FALSE)				}
				cdt_multi$comp[[i]] = cdt_multi$comp[[i]][[1]]
				cdt_multi[i, ]
			} else {
				cbind(allby_temp,
					  comp = rep(cdt_multi$comp[[i]], length.out = nrow(allby_temp)),
					  cdt_multi[i, setdiff(names(cdt_multi), c(names(allby_temp), "comp")), with = FALSE])
			}
		}))
		cdt = rbind(cdt_single, cdt_rows)
	}


	# filter components that are not shown (e.g. not implemented), which is determined in the Prepare step
	cdt$show = sapply(cdt$comp, function(l) l$show)
	cdt = cdt[cdt$show,][,show:=NULL]


	cdt$comp = lapply(cdt$comp, function(comp) do.call(funW, list(comp = comp, o = o)))
	cdt$comp = lapply(cdt$comp, function(comp) do.call(funH, list(comp = comp, o = o)))

	cdt[, ':='(facet_row = character(), facet_col = character())]
	cdt$stack_auto = vapply(cdt$comp, function(l) {
		s = l$stack
		length(s) > 1
	}, FUN.VALUE = logical(1))
	cdt$stack = vapply(cdt$comp, function(l) {
		s = l$stack
		if (length(s) > 1 && "manual" %in% names(s)) s["manual"] else s[1]
	}, FUN.VALUE = character(1))


	getLW = function(x) sapply(x, function(y) {
		yW = y$Win
		yW %||% 0
	})
	getLH = function(x) sapply(x, function(y) {
		yH = y$Hin
		yH %||% 0
	})
	# attempt to determine margins
	cdt[, legW := getLW(comp)]
	cdt[, legH := getLH(comp)]

	if (any(is.na(cdt$z))) {
		cdt[is.na(z), z := zauto]
	}
	cdt[, zauto:=NULL]
	if (nrow(cdt)>0L) {
		data.table::setorder(cdt, "z")
	}


	cdt
}

process_components2 = function(cdt, o) {
	gs = tmap_graphics_name()




	if (o$type != "grid" && o$n > 1) {
		#if (o$nrows == 1 && o$ncols == 1)
		if (o$nrows == 1) {
			# -use by2 and not by1 when they form a row
			cdt[, by2__ := by1__]
			cdt[, by1__ := NA]
		}
	}

	stacks = o$legend.stack


	# place legends inside if needed
	#if (o$ncols > 1 && o$nrows > 1) {
	if (o$type == "wrap") {
		# all free legends inside
		if (any((!is.na(cdt$by1) | !is.na(cdt$by2)) & cdt$class == "out")) cli::cli_inform("{.field {.fun tm_pos_out}} Legends per facets are placed outside. To align them with the facets, consider {.fun tm_facets_stack} or {.fun tmap_arrange}.")
		cdt[(!is.na(by1__) | !is.na(by2__)) & class == "autoout", ':='(class = "in")]
	} else if (o$type == "grid") {
		# all free-per-facet legends inside
		cdt[!is.na(by1__) & !is.na(by2__) & class == "autoout", ':='(class = "in")]
	}



	# update auto position (for 'all', 'rows', 'columns' legends)
	cdt[is.na(by1__) & is.na(by2__) & class == "autoout", ':='(cell.h = o$legend.position.all$cell.h, cell.v = o$legend.position.all$cell.v)]
	cdt[!is.na(by1__) & is.na(by2__) & class == "autoout", ':='(cell.h = o$legend.position.sides$cell.h, cell.v = "by")]
	cdt[is.na(by1__) & !is.na(by2__) & class == "autoout", ':='(cell.h = "by", cell.v = o$legend.position.sides$cell.v)]

	cdt[is.na(by1__) & is.na(by2__) & class == "autoout", ':='(stack = ifelse(stack_auto, ifelse(cell.h == "center", stacks["all_col"], ifelse(cell.v == "center", stacks["all_row"], stacks["all"])), stack))]
	cdt[!is.na(by1__) & is.na(by2__) & class == "autoout", ':='(stack = ifelse(stack_auto, stacks["per_row"], stack))]
	cdt[is.na(by1__) & !is.na(by2__) & class == "autoout", ':='(stack = ifelse(stack_auto, stacks["per_col"], stack))]


	cdt[class == "autoout", class := "out"]


	lai = which(cdt$class == "autoin")
	if (length(lai)) {
		cdt[class == "autoin", ":="(pos.h = ifelse(is.na(pos.h), o$legend.autoin.pos[1], pos.h),
									pos.v = ifelse(is.na(pos.v), o$legend.autoin.pos[2], pos.v),
									class = "in")]
	}

	vby = any(cdt$cell.v == "by" & !is.na(cdt$cell.v))
	hby = any(cdt$cell.h == "by" & !is.na(cdt$cell.h))

	toC = function(x) {
		paste(x, collapse = "_")
	}


	# # manual outside legends -2 is top or left, -1 is bottom or right
	# cdt[class %in% c("autoout", "out"), ':='(facet_row =
	# 		ifelse(cell.v == "center", ifelse(vby, "1", toC(1:o$nrows)),
	# 		ifelse(cell.v == "by", as.character(by1__),
	# 		ifelse(cell.v == "top", as.character(-2), as.character(-1)))),
	# 	facet_col =
	# 		ifelse(cell.h == "center", ifelse(hby, "1", toC(1:o$ncols)),
	# 		ifelse(cell.h == "by", as.character(by2__),
	# 		ifelse(cell.h == "left", as.character(-2), as.character(-1)))))]
	# manual outside legends -2 is top or left, -1 is bottom or right
	cdt[class %in% c("autoout", "out"), ':='(facet_row =
											 	ifelse(cell.v == "center", toC(1:o$nrows),
											 		   ifelse(cell.v == "by", as.character(by1__),
											 		   	   ifelse(cell.v == "top", as.character(-2), as.character(-1)))),
											 facet_col =
											 	ifelse(cell.h == "center", toC(1:o$ncols),
											 		   ifelse(cell.h == "by", as.character(by2__),
											 		   	   ifelse(cell.h == "left", as.character(-2), as.character(-1)))))]

	cdt
}

step4_plot = function(tm, vp, return.asp, show, in.shiny, knit, knit_opts, args) {
	tmx = tm$tmo
	o = tm$o
	aux = tm$aux
	cmp = tm$cmp
	prx = tm$prx

	# check if mode is the same as in step1
	# tmap_mode() may be executed in between (#1082)
	gs = tmap_graphics_name()

	if (o$name != gs) cli::cli_abort("tmap mode changed during execution; did you run {.code tmap_mode()} inside a shiny app?")

	bbo = o$bbox

	# shortcut if no data layers are used, but only a tm_shape
	if (length(tmx)) {
		bbx_def = vapply(o$main, function(mid) {
			"bbx" %in% names(tmx[[mid]])
		}, FUN.VALUE = logical(1))
		if (any(bbx_def)) {
			bbxs = lapply(o$main[which(bbx_def)], FUN = function(mid) {
				tmx[[mid]]$bbx
			})
			bbm = stm_merge_bbox(bbxs)
		} else {
			bbm = NULL
		}
	} else {
		bbm = NULL
	}

	# remove empty data layers
	any_groups = (length(tmx) > 0L)

	tmx = lapply(tmx, function(tmxi) {
		tmxi$layers = lapply(tmxi$layers, function(tml) {
			empt = vapply(tml$shpDT$shpTM, function(sdt) {
				length(sdt$shp) == 0L
			}, FUN.VALUE = logical(1))
			if (all(empt)) NULL else tml
		})
		empt = vapply(tmxi$layers, is.null, logical(1))

		if (all(empt)) {
			NULL
		} else {
			tmxi$layers = tmxi$layers[!empt]
			tmxi
		}
	})
	empt = vapply(tmx, is.null, logical(1))
	tmx = tmx[!empt]


	any_data_layer = (length(tmx) > 0L)

	# split tm in case of as.layers in tm_facets
	# TODO

	# function to subset data
	get_dt = function(dt, by1, by2, by3, remove_by = FALSE) {
		b = list(by1, by2, by3)
		bynames = intersect(names(dt), paste0("by", 1:3, "__"))
		byids = as.integer(substr(bynames, 3, 3))

		sel = rep(TRUE, nrow(dt))
		if (length(bynames)) {
			for (i in 1:length(bynames)) {
				sel = sel & (is.na(dt[[bynames[i]]]) | dt[[bynames[i]]] %in% b[[byids[i]]])
			}
		}
		if (remove_by) {
			nms = setdiff(names(dt), bynames)
			dt[which(sel), nms, with = FALSE]
		} else {
			dt[which(sel),]
		}
	}

	if (o$as.layers) {
		bys = do.call(expand.grid, lapply(o$fn, seq, from = 1))

		bys_labs = lapply(o$fl, function(x) if (is.null(x)) "" else x)
		labs = do.call(mapply, c(list(FUN = paste0, USE.NAMES = FALSE), mapply("[", bys_labs, bys, SIMPLIFY = FALSE, USE.NAMES = FALSE)))
		# to do: take labels from o$fl
		# assign to tml$group
		# disable multiple basemaps

		e = environment()
		any_radio = FALSE
		tmx = lapply(tmx, function(tmxi) {
			lys = mapply(function(tml, tml_name) {
				lys2 = lapply(1:nrow(bys), function(i) {
					tml$mapping_dt = get_dt(tml$mapping_dt, bys$Var1[i], bys$Var2[i], bys$Var3[i], remove_by = TRUE)
					tml$mapping_legend = lapply(tml$mapping_legend, function(ml) {
						ml = get_dt(ml, bys$Var1[i], bys$Var2[i], bys$Var3[i], remove_by = TRUE)
						fixed = (nrow(ml) == 1L && tml$group.control == "check")
						for (j in 1:nrow(ml)) {
							id_old = ml$legnr[j]
							leg_old = .TMAP$legs[[ml$legnr[j]]]
							leg_old$layerId = paste0("fixed", ml$legnr[j])
							leg_old$group = labs[i]
							ml$legnr[j] = legend_save(leg_old)
						}
						ml
					})
					tml$group = labs[i]
					if (tml$group.control == "radio") assign("any_radio", value = TRUE, envir = e)
					tml$lid = tml$lid + i
					tml
				})
				names(lys2) = paste0(tml_name, "_", sprintf("%02d", 1:length(lys2)))
				lys2
			}, tmxi$layers, names(tmxi$layers), SIMPLIFY = FALSE, USE.NAMES = FALSE)
			tmxi$layers = do.call(c, lys)
			tmxi
		})

		if (length(aux) && any_radio) {
			multi_tile = any(vapply(aux, function(ai) {
				(ai$mapping.fun == "Tiles" && length(ai$args$server) > 1)
			}, FUN.VALUE = logical(1L), USE.NAMES = FALSE))
			if (multi_tile) cli::cli_inform("When {.code as.layers = TRUE} with radio buttons, it may be easier to have only one basemap, e.g. {.code + tm_basemap({.str OpenStreetMap})}")
		}

		o$n = 1
		o$fl = list(NULL,NULL,NULL)
		o$fn = rep(1L, 3)
		o$ncols = 1
		o$nrows = 1

	}



	# get name of graphics engine (for function names e.g. tmapGridInit)
	gs = tmap_graphics_name()

	o = prepreprocess_meta(o, vp)

	# get legends from layer data and put them in "components data.table" (cdt)

	if (o$legend.only) {
		cmp_sel = vapply(cmp, function(cp) {
			inherits(cp, c("tm_legend", "tm_add_legend"))
		}, FUN.VALUE = logical(1))
		cdt_cmp = if (any(cmp_sel)) {
			data.table::rbindlist(lapply(cmp[cmp_sel], function(cp) {
				data.table::data.table(by1__ = NA_integer_,
									   by2__ = NA_integer_,
									   by3__ = NA_integer_,
									   comp = list(cp))
			}))
		} else {
			data.table::data.table(by1__ = integer(0),
								   by2__ = integer(0),
								   by3__ = integer(0),
								   comp = list())
		}
	} else {
		cdt_cmp = if (length(cmp)) {
			data.table::rbindlist(lapply(cmp, function(cp) {
				data.table::data.table(by1__ = NA_integer_,
									   by2__ = NA_integer_,
									   by3__ = NA_integer_,
									   comp = list(cp))
			}))
		} else {
			data.table::data.table(by1__ = integer(0),
								   by2__ = integer(0),
								   by3__ = integer(0),
								   comp = list())
		}
	}

	cdt = if (any_data_layer) {
		cdt_legs = step4_plot_collect_legends(tmx)
		cdt_crts = step4_plot_collect_charts(tmx)
		data.table::rbindlist(list(cdt_legs, cdt_crts, cdt_cmp))
	} else {
		cdt_cmp
	}


	## place components top left
	if (o$legend.only) {
		cdt$comp = lapply(cdt$comp, function(cc) {
			cc$position = complete_options(tm_pos_in("left", "top"), o$legend.position)
			cc
		})
	}


	if (gs == "Grid" && is.null(vp) && show) {
		is_inset_tmap = sapply(cdt$comp, inherits, what = "tm_inset_tmap")
		if (any(is_inset_tmap)) {
			grid::grid.newpage()
			newpage = FALSE
		} else {
			newpage = TRUE
		}
	} else {
		newpage = TRUE
	}

	if (nrow(cdt)) cdt = process_components(cdt, o)

	# determine panel type, inner margins, and automatic comp placement
	if (!o$legend.only) {
		o = preprocess_meta(o, cdt)

		# add shape unit (needed for e.g. tm_scale_bar)
		unit = ifelse(o$unit == "metric", "km", ifelse(o$unit == "imperial", "mi", o$unit))
		crs = get_option_class(o$crs_step4, "sf") #o$crs
		longlat = sf::st_is_longlat(crs)
	} else {
		o$npages = 1L
	}



	# function to get shape object
	get_shpTM = function(shpDT, by1, by2, by3) {
		b = list(by1, by2, by3)
		bynames = intersect(names(shpDT), paste0("by", 1:3, "__"))
		byids = as.integer(substr(bynames, 3, 3))

		sel = rep(TRUE, nrow(shpDT))
		if (length(bynames)) {
			for (i in 1L:length(bynames)) {
				sel = sel & shpDT[[bynames[i]]] %in% b[[byids[i]]]
			}
		}
		shpDT$shpTM[which(sel)]
	}


	# function to get bbox per facet, also take into account bbm (for groups without data-layers)
	get_bbox = function(by1, by2, by3) {
		bbxs = lapply(tmain, function(tmi) {
			shpTM = get_shpTM(tmi$shpDT, by1, by2, by3)
			mdt = get_dt(tmi$mapping_dt, by1, by2, by3)
			bbxs2 = lapply(shpTM, stm_bbox, tmapID = mdt$tmapID__, crs = crs)
			if (!is.null(bbm)) bbxs2 = c(list(bbm), bbxs2)
			bbx = stm_merge_bbox(bbxs2)
			if (is.na(bbx)) bbx else tmaptools::bb(bbx, asp.limit = 10)
		})
		bbm = stm_merge_bbox(bbxs)
		bbc = bbm

		bbe = bb_ext(bbc, o$inner.margins)

		bbe = crop_lat(bbe, crs, o$limit_latitude_3857)



		list(list(bbe))
	}

	# main group (that determines bounding box)
	# TODO take into account multiple main groups (see step1_rearrange and get_main_ids)


	if (o$legend.only) {
		d = NULL
	} else if (any_data_layer) {
		# create table with meta data for the facets (row, col id, bbox, asp)
		d = data.table::data.table(do.call(expand.grid, lapply(structure(o$nby, names = c("by1", "by2", "by3")), seq_len)))
		d[, i := seq_len(nrow(d))]
		grps = c("by1", "by2", "by3")[o$free.coords]

		grp_ids = as.integer(substr(names(tmx), 6, nchar(names(tmx))))
		mains_in_grp = intersect(o$main[!bbx_def], grp_ids)
		if (length(mains_in_grp) && !("inset" %in% names(o)) && !o$earth_bbox && is.null(bbo)) {
			lookup = match(mains_in_grp, grp_ids)
			tmain = unlist(unlist(tmx[lookup], recursive = FALSE, use.names = FALSE), recursive = FALSE, use.names = FALSE)
			d[, bbox:=do.call(get_bbox, as.list(.SD)), by = grps, .SDcols = c("by1", "by2", "by3")]
		} else {
			if (!is.null(bbo) || is.null(bbm)) {
				if (!is.null(bbo) && identical(bbo, "FULL") || (is.null(bbo) && is.null(bbm))) {
					bbm = full_bbox(crs)
				} else {
					if (inherits(bbo, "SpatExtent")) bbo = sf::st_bbox(bbo)
					bbm = tmaptools::bb(bbo, projection = crs)
				}
				if (o$earth_bbox) {
					bbm = bb_ext(bbm, o$inner.margins)
				}
			} else {
				bbm = sf::st_transform(bbm, crs = crs)
			}
			d[, bbox:=rep(list(bbm),nrow(d))]
		}
	} else {
		if (is.null(bbm)) {
			if (!is.null(bbo)) {
				bbm = tmaptools::bb(bbo, projection = crs)
			} else {
				bbm = full_bbox(crs)
			}
		} else {
			bbm = sf::st_transform(bbm, crs = crs)
		}

		bbm = crop_lat(bbm, crs, o$limit_latitude_3857)



		d = data.table::data.table(by1 = 1L, by2 = 1L, by3 = 1L, i = 1, bbox = list(bbm))
	}

	if (!o$legend.only) {
		d[, asp:=get_asp(bbox)]
		d = d[!is.na(asp)]


		if (o$type != "grid" && is.na(o$nrows) && is.na(o$ncols)) {
			# limit facets
			n_lim = limit_nx(o$n, o$facet.max)
			if (n_lim != o$n) {
				fn_lim = pmin(o$fn, n_lim)
				while(prod(fn_lim) > n_lim) {
					fn_lim[which.max(fn_lim)] = fn_lim[which.max(fn_lim)] - 1L
				}
				d = d[by1<= fn_lim[1] & by2<= fn_lim[2] & by3 <= fn_lim[3]]
				o$fl = mapply(function(a, b) a[1:b], o$fl, fn_lim, SIMPLIFY = FALSE)
				o$fn = fn_lim
				o$n = n_lim
			}
		}

		# not sure what this does:
		d[, bbox:=lapply(bbox, FUN = function(bbx) {
			if (!is.na(bbx) && !is.na(longlat) && longlat && !sf::st_is_longlat(bbx)) {
				sf::st_bbox(sf::st_transform(tmaptools::bb_poly(bbx), crs = 4326))
			} else {
				bbx
			}
		})]

		d[, units:=lapply(bbox, FUN = function(bbx) {
			if (is.na(bbx)) {
				list()
			} else {
				if (!is.na(bbx) && !is.na(longlat) && longlat) {
					latitude <- mean.default(bbx[c(2,4)])
					bbxll <- c(xmin=0, ymin=latitude, xmax=1, ymax=latitude)
					ad <- suppressWarnings({tmaptools::approx_distances(bbxll, projection=crs)})
					to <- as.numeric(units::set_units(ad$hdist, units::as_units(unit), mode = "standard"))
				} else {
					ad <- suppressWarnings({tmaptools::approx_distances(bbx, projection=crs)})

					if (is.na(crs)) {
						to <- ad$hdist
					} else {
						to <- as.numeric(units::set_units(units::set_units(1, attr(ad$hdist, "units")$numerator, mode = "standard"), units::as_units(unit), mode = "standard"))
					}
				}
				list(projection=crs, unit=unit, to=to, projected = !longlat)
			}
		})]

		# determine automatic position of inside comp
		if (!any(o$free.coords) && any(cdt$class == "autoin")) {
			shp = tmain[[1]]$shpDT$shpTM[[1]]$shp
			# TODO take into account multiple main shapes
			# TODO take use areas instead of coordinates for polygons
			if (inherits(shp, c("sf", "sfc"))) {
				bbx = d$bbox[[1]]
				co = sf::st_coordinates(sf::st_centroid(shp))

				xn = (co[,1]-bbx[1])/(bbx[3]-bbx[1])
				yn = (co[,2]-bbx[2])/(bbx[4]-bbx[2])
				cornerID = which.max(c(
					bl = min(sqrt((xn^2) + (yn^2)), na.rm = TRUE),
					tl = min(sqrt((xn^2) + ((1-yn)^2)), na.rm = TRUE),
					tr = min(sqrt(((1-xn)^2) + ((1-yn)^2)), na.rm = TRUE),
					br = min(sqrt(((xn-1)^2) + (yn^2)), na.rm = TRUE)))

				o$legend.autoin.pos = switch(names(cornerID), tl = c("left", "top"), tr = c("right", "top"), bl = c("left", "bottom"), br = c("right", "bottom"))
			} else {
				o$legend.autoin.pos = c("left", "top")
			}
		} else {
			o$legend.autoin.pos = c("left", "top")
		}
	}


	# calculate margins, number of rows and colums, etc.
	o = process_meta(o, d, cdt, aux)

	if (!o$legend.only) {
		# workaround to move panels to titles in view mode
		# TO DO: make this generic (e.g. component prep function?)
		if (gs != "Grid") {
			if (o$panel.type != "none") {
				cdt = rbindlist(c({if (nrow(cdt) == 0L) NULL else list(cdt)}, mapply(function(lab, i) {
					data.table::data.table(by1__ = i,
										   by2__ = NA_integer_,
										   by3__ = NA_integer_,
										   comp = list(impute_comp(tm_title(lab)[[1]], o)),
										   class=  "in",
										   cell.h = NA,
										   cell.v = NA,
										   pos.h = "left",
										   pos.v = "top",
										   z = 1,
										   facet_row = NA_character_,
										   facet_col = NA_character_,
										   stack_auto = TRUE,
										   stack = "vertical",
										   legW = 0,
										   legH = 0)
				}, o$panel.labels[[1]], seq_len(o$fn[1]), SIMPLIFY = FALSE)))
			}
			o$panel.type = "none"
		}

		o$ng = length(tmx)

		# determine row and col ids

		if (o$type == "grid") {
			d[, row := as.integer((i - 1) %% o$nrows + 1)]
			d[, col := as.integer((((i - 1) %/% o$nrows + 1) - 1) %% o$ncols + 1)]
			d[, page := as.integer(i - 1) %/% (o$nrows * o$ncols) + 1]
		} else {

			# wrap
			if (o$nby[3] == 1L) {
				if (!o$byrow) {
					d[, row := as.integer((i - 1) %% o$nrows + 1)]
					d[, col := as.integer((((i - 1) %/% o$nrows + 1) - 1) %% o$ncols + 1)]
				} else {
					d[, col := as.integer((i - 1) %% o$ncols + 1)]
					d[, row := as.integer((((i - 1) %/% o$ncols + 1) - 1) %% o$nrows + 1)]
				}
				d[, page := as.integer(i - 1) %/% (o$nrows * o$ncols) + 1]
			} else {
				d[, j:= rep(1L:o$npp, length.out = nrow(d))]
				if (!o$byrow) {
					d[, row := as.integer((j - 1) %% o$nrows + 1)]
					d[, col := as.integer((((j - 1) %/% o$nrows + 1) - 1) %% o$ncols + 1)]
				} else {
					d[, col := as.integer((j - 1) %% o$ncols + 1)]
					d[, row := as.integer((((j - 1) %/% o$ncols + 1) - 1) %% o$nrows + 1)]
				}
				d[, page := by3]
				d[, j:= NULL]
			}
		}

	}



	#####o$legend.autoin.pos = c("left", "top")

	# prepare function names
	FUNinit = paste0("tmap", gs, "Init")
	FUNaux = paste0("tmap", gs, "Aux")
	FUNrun = paste0("tmap", gs, "Run")
	FUNshape = paste0("tmap", gs, "Shape")
	FUNoverlay = paste0("tmap", gs, "Overlay")
	FUNwrap = paste0("tmap", gs, "Wrap")
	FUNxtab = paste0("tmap", gs, "Xtab")

	FUNxlab = paste0("tmap", gs, "Xlab")
	FUNylab = paste0("tmap", gs, "Ylab")


	FUNgridxlab = paste0("tmap", gs, "GridXLab")
	FUNgridylab = paste0("tmap", gs, "GridYLab")

	# for inset maps
	inset_frame_ids = if (nrow(cdt) == 0L) integer(0) else which(sapply(cdt$comp, function(cmp) "bbox" %in% names(cmp)))

	if (!o$legend.only) {
		# create table with bounding boxes (the only important property, apart from settings)
		db = data.table(bbox = unique(d$bbox[!is.na(d$asp)]))
		db[, i:=1L:nrow(db)]
		d[, bi:=db$i[match(d$bbox, db$bbox)]]

		## process components
		if (nrow(cdt)) cdt = process_components2(cdt, o)

		# init
		res = do.call(FUNinit, c(list(o = o, show = show, newpage = newpage, return.asp = return.asp, vp = vp, prx = prx), args))
		if (return.asp) {
			return(res)
		} else {
			.TMAP$geo_ref = get_geo_ref(bbx = db$bbox[[1]],
										crs = crs,
										inner_margins = o$inner.margins,
										dev_size = res$dev[1:2], map_size = res$map[1:2], offset = res$margins)
			.TMAP$animation = list(fps = o$fps, play = o$play)
		}

		## prepare aux layers
		if (length(aux)) {
			# prepare aux layers and return group label (in case it is not user specified)
			aux_group_def = mapply(function(a, id) {
				FUNaux_prep = paste0("tmap", gs, "AuxPrepare")
				a_args = structure(a$args, class = c(a$mapping.fun, "list"))

				do.call(FUNaux_prep, list(a = a_args, bs = db$bbox, id = id, o = o))
			}, aux, 1:length(aux))
			aux_group = mapply(function(a, agd) {
				ng = lengths(gregexpr("__", agd, fixed = TRUE))
				if (is.na(a$group)) agd else as.character(a$group)
			}, aux, aux_group_def, USE.NAMES = FALSE)

			aux_group.control = vapply(aux, function(a) {
				a$group.control
			}, FUN.VALUE = character(1))

			# find lid (layer plot id values) for aux layers
			aux_lid = vapply(aux, function(a) a$lid, FUN.VALUE = numeric(1))
		}

		if (!any_data_layer && !length(aux)) {
			message_nothing_to_show(any_groups)
			return(invisible(NULL))
		}



		# data frame for layer ids
		q = do.call(rbind, c(
			{if (any_data_layer) {
				lapply(1L:o$ng, function(ig) {
					tmxi = tmx[[ig]]
					nl = length(tmxi$layers)
					lid = vapply(tmxi$layers, function(l) {l$lid}, FUN.VALUE = numeric(1))
					group = vapply(tmxi$layers, function(l) {l$group}, FUN.VALUE = character(1))
					group.control = vapply(tmxi$layers, function(l) {l$group.control}, FUN.VALUE = character(1)) # used to determine control layer group (view mode)
					data.frame(gid = rep(ig, nl), glid = 1:nl, lid = lid, group = group, group.control = group.control, lid2 = rep(0, nl), pane = rep("", nl), new = rep(TRUE, nl), group.zoom_levels = I(rep(list(NA),nl)))
				})
			} else {
				NULL
			}},
			{if (length(aux)) list(data.frame(gid = 0, glid = 1L:length(aux), lid = aux_lid, group = aux_group, group.control = aux_group.control, lid2 = 0, pane = "", new = TRUE, group.zoom_levels = I(rep(list(NA),length(aux))))) else NULL}))

		#q$lid[q$lid != 0] = q$lid[q$lid != 0] + 400L

		if (.TMAP$proxy) {
			q0 = .TMAP$q
			q0$new = FALSE
			q = rbind(q0, q)
		}

		qnotnull = (q$lid != 0)
		if (any(qnotnull)) q$lid2[qnotnull] = rank(q$lid[qnotnull])

		q = q[order(q$lid2), ]
		q$pane = "tilePane"

		q$pane[q$lid2 > 0] = pane_name(q$lid[q$lid2 > 0])


		# via tm_group, control and zoom levels can be set
		# these are stored in tmap options view_group_{name}, and merged with q:
		og = o[substr(names(o), 1, 10) == "view_group"]
		ogdf = 	data.frame(name = vapply(og, "[[",FUN.VALUE = character(1), "name", USE.NAMES = FALSE),
						   control = vapply(og, function(x) {
						   	as.character(x[["control"]])
						   } , FUN.VALUE = character(1), USE.NAMES = FALSE),
						   zoom_levels = I(unname(lapply(og, "[[", "zoom_levels"))))
		q$group.control = mapply(function(gi, v) {
			id = which(ogdf$name == gi)
			if (!length(id)) return(v)
			v2 = ogdf$control[id]
			if (is.na(v2)) return(v) else return(v2)
		}, q$group, q$group.control, USE.NAMES = FALSE, SIMPLIFY = TRUE)
		q$group.zoom_levels = mapply(function(gi, v) {
			id = which(ogdf$name == gi)
			if (!length(id)) return(NA)
			return(ogdf$zoom_levels[[id]])
		}, q$group, q$group.control, USE.NAMES = FALSE, SIMPLIFY = FALSE)
		q$group.control[!is.na(q$group.zoom_levels)] = "none" # see leaflet::groupOptions



		# q data frame:
		# gid = tmap-group counter
		# glid = layer counter inside tmap-group
		# lid = possibly-user-defined layer order number
		# lid2 = same as lid, but 1,2,3,...
		# pane = pane name (for view mode)
		# group = group name (for selecting layers in view mode)
		# group.control = leaflet::addLayerControl: check, radio or none (view mode)
		# group.zoom_levels = leaflet::groupOptions: zoom levels displayed at

		do.call(FUNaux, list(o = o, q = q))


		# plot xtab headers
		if (o$panel.type == "xtab") {
			for (k in 1:o$npages) {
				labrows = o$panel.labels[[1]]
				labcols = o$panel.labels[[2]]
				if (length(labrows) == o$nrows) for (i in 1:o$nrows) do.call(FUNxtab, list(label = labrows[i], facet_row = i, facet_page = k, o = o))
				if (length(labcols) == o$ncols) for (j in 1:o$ncols) do.call(FUNxtab, list(label = labcols[j], facet_col = j, facet_page = k, o = o))

			}
		}


		# plot xlab and ylab
		if (o$xlab.show) {
			for (k in 1:o$npages) {
				do.call(FUNxlab, list(facet_page = k, o = o))
			}
		}
		if (o$ylab.show) {
			for (k in 1:o$npages) {
				do.call(FUNylab, list(facet_page = k, o = o))
			}
		}



		for (i in seq_len(nrow(d))) {
			bbx = d$bbox[[i]]
			if (o$panel.type == "wrap") do.call(FUNwrap, list(label = o$panel.labels[[o$panel.labels.dim]][d[[paste0("by", o$panel.labels.dim)]][i]], facet_row = d$row[i], facet_col = d$col[i], facet_page = d$page[i], o = o))
			if (is.na(d$asp[i])) next
			do.call(FUNshape, list(bbx = bbx, facet_row = d$row[i], facet_col = d$col[i], facet_page = d$page[i], o = o))

			# plot grid labels
			if (o$grid.show && !o$grid.labels.inside_frame) {
				if (o$grid.labels.show[2] && ((o$grid.labels.pos[1] == "left" && d$col[i] == 1) || (o$grid.labels.pos[1] == "right" && d$col[i] == o$ncols))) {
					do.call(FUNgridylab, list(bi = d$bi[i], bbx = bbx, facet_row = d$row[i], facet_col = d$col[i], facet_page = d$page[i], o = o))
				}
				if (o$grid.labels.show[1] && ((o$grid.labels.pos[2] == "top" && d$row[i] == 1) || (o$grid.labels.pos[2] == "bottom" && d$row[i] == o$nrows))) {
					do.call(FUNgridxlab, list(bi = d$bi[i], bbx = bbx, facet_row = d$row[i], facet_col = d$col[i], facet_page = d$page[i], o = o))
				}
			}

			for (qi in which(q$new)) {
				gid = q$gid[qi]
				glid = q$glid[qi]
				pane = q$pane[qi]
				group = q$group[qi]
				if (gid > 0) {
					# data layer
					bl = tmx[[gid]]$layers[[glid]]
					shpTM = get_shpTM(bl$shpDT, d$by1[i], d$by2[i], d$by3[i])[[1]]
					mdt = get_dt(bl$mapping_dt, d$by1[i], d$by2[i], d$by3[i])

					id = paste0("f", sprintf("%03d", i), "g", sprintf("%02d", gid), "l", sprintf("%02d", glid))

					if (nrow(mdt) != 0) {
						gp = bl$gp

						FUN = paste0("tmap", gs, "DataPlot")

						a = structure(bl$mapping_args, class = c(bl$mapping_fun, "list"))

						do.call(FUN, c(list(a = a, shpTM = shpTM, dt = mdt, pdt = bl$popup.data, popup.format = bl$popup.format, hdt = bl$hover.data, idt = bl$id.data, gp = gp, bbx = bbx, facet_col = d$col[i], facet_row = d$row[i], facet_page = d$page[i], id = id, pane = pane, group = group, o = o)))
					}

				} else {
					glid = q$glid[qi]


					# aux layer
					a = aux[[glid]]
					a_args = structure(a$args, class = c(a$mapping.fun, "list"))

					FUNaux_plot = paste0("tmap", gs, "AuxPlot")

					id = glid # to do: test!
					do.call(FUNaux_plot, list(a = a_args, bi = d$bi[i], bbx = bbx, facet_col = d$col[i], facet_row = d$row[i], facet_page = d$page[i], id = id, pane = pane, group = group, o = o))

				}



			}

			# inset map frame
			if (length(inset_frame_ids)) {
				sfc_bbxs = do.call(c, lapply(inset_frame_ids, function(iid) {
					ic = cdt$comp[[iid]]
					sf::st_transform(tmaptools::bb_poly(ic$bbox), crs = crs)
				}))

				collect_properties = function(comps, props) {
					res = lapply(props, function(p) {
						sapply(comps, function(cmp) {
							cmp[[p]]
						})
					})
					names(res) = props
					res
				}

				prp = collect_properties(cdt$comp[inset_frame_ids],
										 c("box_frame", "box_frame.color", "box_frame.alpha", "box_frame.lwd", "box_frame.lty", "box_bg", "box_bg.color", "box_bg.alpha"))
				calld = lapply(cdt$comp[inset_frame_ids], function(cmp) cmp$called)

				prp$box_frame[vapply(calld, function(cl) "box_frame.color" %in% cl, FUN.VALUE = logical(1))] = TRUE
				prp$box_bg[vapply(calld, function(cl) "box_bg.color" %in% cl, FUN.VALUE = logical(1))] = TRUE

				prp$box_bg.color[!prp$box_bg] =  "#00000000"
				prp$box_frame.color[!prp$box_frame] =  "#00000000"


				tb = tm_polygons()[[1]]


				gp = tb$gpar

				FUN = paste0("tmap", gs, "DataPlot")

				a = structure(tb$mapping.args, class = c("tm_data_polygons", "list"))

				shapepTM = shapeTM(sfc_bbxs, tmapID = seq_along(sfc_bbxs))

				shpTM = tmapTransPolygons(shapepTM, args = tb$trans.args, plot.order = tb$plot.order)

				mdt = data.table(tmapID__ = seq_along(sfc_bbxs), fill = prp$box_bg.color, col = prp$box_frame.color, lwd = prp$box_frame.lwd, lty = prp$box_frame.lty, fill_alpha = prp$box_bg.alpha, col_alpha = prp$box_frame.alpha, ord__ = 1L)

				do.call(FUN, c(list(a = a, shpTM = shpTM, dt = mdt, pdt = NULL, popup.format = list(), hdt = NULL, idt = NULL, gp = gp, bbx = bbx, facet_col = d$col[i], facet_row = d$row[i], facet_page = d$page[i], id = "inset_frame", pane = "tmap500", group = NA, o = o)))

			}
			do.call(FUNoverlay, list(bbx = bbx, facet_row = d$row[i], facet_col = d$col[i], facet_page = d$page[i], o = o))
		}

	} else {
		# init
		asp = do.call(FUNinit, list(o = o, return.asp = return.asp, vp = vp))
	}


	if (o$legend.only) {
		cdt[, facet_row := "1"]
		cdt[, facet_col := "1"]
		cdt[, page := 1L]
		cdt[, bbox := list()]
	} else {
		is_in = cdt$class == "in"
		#is_in = rep(TRUE, nrow(cdt))
		if (any(is_in)) {
			legs_in = lapply(which(is_in), function(i) {
				d2 = data.table::copy(d)
				legsi = cdt[i, ]
				if (o$type != "grid" && o$nrows == 1) {
					# reverse above
					d2[, by2 := by1]
					d2[, by1 := 1]
				}

				if (is.na(legsi$by1__)) d2[, by1:= NA]
				if (is.na(legsi$by2__)) d2[, by2:= NA]
				if (is.na(legsi$by3__)) d2[, by3:= NA]
				legsi = merge(legsi, d2[, c("by1", "by2", "by3", "row", "col", "page", "bbox", "units"), with = FALSE], by.x = c("by1__", "by2__", "by3__"), by.y = c("by1", "by2", "by3"))
				legsi[, ':='(facet_row = as.character(row), facet_col = as.character(col), row = NULL, col = NULL)]
				legsi
			})
		} else {
			legs_in = NULL
		}


		legs_out = copy(cdt[!is_in])
		if (nrow(legs_out) >= 1L) {
			legs_out[, page:=NA_integer_]
			legs_out[!is.na(by3__), page := by3__]

			by1_clip =  (length(which(legs_out$by1__ > o$nrows)) > 0L) && all(legs_out$facet_row == legs_out$by1__)
			by2_clip =  (length(which(legs_out$by2__ > o$ncols)) > 0L) && all(legs_out$facet_col == legs_out$by2__)
			if (by1_clip && by2_clip) {
				cli::cli_abort("multiple pages cannot be created over two facet dimensions")
			}
			if (by1_clip) {
				legs_out[!is.na(by1__), ':='(page = ((by1__-1L) %/% o$nrows) + 1L, facet_row = ((by1__-1L) %% o$nrows) + 1L)]
			} else if (by2_clip) {
				legs_out[!is.na(by2__), ':='(page = ((by2__-1L) %/% o$ncols) + 1L, facet_col = ((by2__-1L) %% o$ncols) + 1L)]
			}

			# ad-hoc method: take first bbox and units
			bbox_nb = d$bbox[1]
			attr(bbox_nb, "borrow") = list(col = d$col[1], row = d$row[1])
			legs_out[, bbox:=list(bbox_nb)]
			legs_out[, units:=list(d$units[1])]
		} else {
			legs_out$page = integer()
			legs_out$bbox = list()
			legs_out$units = list()
		}



		cdt = data.table::rbindlist(c(list(legs_out), legs_in))

		cdt$comp = mapply(function(cmp, bbx, u) {
			if (!("bbox" %in% names(cmp))) cmp$bbox = bbx
			cmp$units = u
			cmp
		}, cdt$comp, cdt$bbox, cdt$units, SIMPLIFY = FALSE)

		leg_dummy = tm_legend_hide()
		leg_dummy$active = FALSE
		leg_dummy$vneutral = NA

		leg_nr_dummy = legend_save(leg_dummy)
		crt_nr_dummy = chart_save(tm_chart_none())

		# inset ids
		inset_ids = if (nrow(cdt) == 0L) integer(0) else which(sapply(cdt$comp, inherits, "tm_inset_map"))


		# inset maps: prepare input for step4
		if (length(inset_ids)) {
			cdt$comp[inset_ids] = lapply(cdt$comp[inset_ids], function(comp) {
					tmo_i = tm$tmo
					o_i = tm$o
					aux_i = tm$aux
					cmp_i = list()
					prx_i = list()

					if ("crs" %in% names(comp)) {
						crs_i = comp$crs
						o_i$crs_step4 = crs_i
					} else {
						crs_i = o_i$crs_step4
					}
					o_i$outer.bg = FALSE
					o_i$frame = comp$main_frame
					o_i$frame.color = comp$main_frame.color
					o_i$frame.alpha = comp$main_frame.alpha
					o_i$frame.lwd = comp$main_frame.lwd
					o_i$frame.r = comp$main_frame.r


					# set legends to inactive
					tmo_i = lapply(tmo_i, function(tmg) {
						lapply(tmg, function(tml) {
							lapply(tml, function(tmi) {
								tmi$shpDT$shpTM = lapply(tmi$shpDT$shpTM, crs_reproject_shpTM, crs = crs_i, raster.warp = o$raster.warp)
								tmi$mapping_legend = lapply(tmi$mapping_legend, function(l) {
									l$legnr = leg_nr_dummy
									l$crtnr = crt_nr_dummy
									l
								})
								tmi$trans_legend = lapply(tmi$trans_legend, function(l) {
									l$legnr = leg_nr_dummy
									l$crtnr = crt_nr_dummy
									l
								})
								tmi
							})
						})
					})

				#	(tm, vp, return.asp, show, in.shiny, knit, args)
					comp$tm = list(tmo = tmo_i,
								   o = o_i,
								   aux = aux_i,
								   cmp = cmp_i,
								   prx = prx_i)
					comp
			})
		}
	}

	legfun = paste0("tmap", gs, "Comp")

	toI = function(x) {
		as.integer(strsplit(x, split = "_")[[1]])
	}

	if (nrow(cdt) > 0L) for (k in seq_len(o$npages)) {
		klegs = cdt[is.na(page) | (page == k), ] # was by3__ instead of page
		#klegs[, pos.h.id := pos.h][pos.h %in% c("left", "center", "right"), pos.h.id:="lower"][pos.h %in% c("LEFT", "CENTER", "RIGHT"), pos.h.id:="upper"]
		#klegs[, pos.v.id := pos.v][pos.v %in% c("top", "center", "bottom"), pos.v.id:="lower"][pos.v %in% c("TOP", "CENTER", "BOTTOM"), pos.v.id:="upper"]

		# find group ids
		klegs$id = paste(klegs$class, sapply(klegs$comp, function(comp) paste(comp$position$cell.h, comp$position$cell.v, comp$position$pos.h, comp$position$pos.v, comp$position$just.h, comp$position$just.v, sep = "_")), sep = "_")
		# just.id = NULL
		# klegs$just.id = sapply(klegs$comp, function(l) paste(l$position$just.h, l$position$just.v, sep = "."))
		# klegs[, id:=paste(pos.h.id, pos.v.id, just.id, class, sep = "__")]
		klegs[, do.call(legfun, args = list(comp = .SD$comp, o = o, facet_row = toI(.SD$facet_row[1]), facet_col = toI(.SD$facet_col[1]), facet_page = k, class = .SD$class[1], stack = .SD$stack, stack_auto = .SD$stack_auto, pos.h = .SD$pos.h, pos.v = .SD$pos.v, .SD$bbox)), by = list(facet_row, facet_col, id), .SDcols = c("comp", "facet_row", "facet_col", "class", "stack", "stack_auto", "pos.h", "pos.v", "bbox")]
	}

	if (show) save_last_map()

	if (.TMAP$in.shiny) {
		.TMAP$q = q
	}

	do.call(FUNrun, list(o = o, q = q, show = show, knit = knit, knit_opts = knit_opts, args))
}

# bb_ext reversing
bb_ext_rev = function(bbx, ext = c(0, 0, 0, 0)) {
	dx = (bbx[3] - bbx[1]) / (1 + ext[2] + ext[4])
	dy = (bbx[4] - bbx[2]) / (1 + ext[1] + ext[3])

	bbx[2] = bbx[2] + ext[1] * dy
	bbx[1] = bbx[1] + ext[2] * dx
	bbx[4] = bbx[4] - ext[3] * dy
	bbx[3] = bbx[3] - ext[4] * dx
	bbx
}

get_geo_ref = function(bbx, crs, inner_margins, dev_size, map_size, offset) {
	bbx_crop = bb_ext_rev(bbx, inner_margins)

	x1 = offset[2] / dev_size[1]
	x2 = (dev_size[1] - offset[4]) / dev_size[1]

	y1 = offset[1] / dev_size[2]
	y2 = (dev_size[2] - offset[3]) / dev_size[2]

	xy_crop = bb_ext_rev(c(x1, y1, x2, y2), inner_margins)
	names(xy_crop) = c("xmin", "ymin", "xmax", "ymax")
	list(crs = crs, bbx = bbx_crop, bbx_frame = bbx, rel_coords = xy_crop)
}
