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


	cdt$class = sapply(cdt$comp, function(l) l$position$type)
	cdt$cell.h = sapply(cdt$comp, function(l) {x = l$position$cell.h; if (is.null(x)) NA else x})
	cdt$cell.v = sapply(cdt$comp, function(l) {x = l$position$cell.v; if (is.null(x)) NA else x})
	cdt$pos.h = sapply(cdt$comp, function(l) {x = l$position$pos.h; if (is.null(x)) NA else x})
	cdt$pos.v = sapply(cdt$comp, function(l) {x = l$position$pos.v; if (is.null(x)) NA else x})
	cdt$z = sapply(cdt$comp, function(l) {x = l$z; if (is.null(x)) NA_integer_ else x})

	# to make sure legends positions are based on one-facet-per-page
	if (o$type == "page") {
		cdt[, by3__ := by1__]
		cdt[, by1__ := NA]
	}


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


	## do to: proper S3 methods
	#if (gs == "Grid") {
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
			cdt[is.na(z), z := seq(1L,(sum(is.na(z))))]
		}
		if (nrow(cdt)>0L) {
			data.table::setorder(cdt, "z")
		}
	# } else if (gs == "Leaflet") {
	#
	# }

	#cdt[, page := NA_integer_]


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
		cdt[!is.na(by1__) | !is.na(by2__) & class == "autoout", ':='(class = "in")]
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


		# cdt$comp = lapply(cdt$comp, function(l) {
		# 	if (is.na(l$position$pos.h)) o$legend.autoin.pos[1]
		# 	if (is.na(l$position$pos.v)) o$legend.autoin.pos[2]
		# 	#l$position[c("pos.h", "pos.v")] = as.list(o$legend.autoin.pos)
		# 	l
		# })
		# cdt[class == "autoin", ":="(pos.h = o$legend.autoin.pos[1], pos.v = o$legend.autoin.pos[2], class = "in")]
	}

	vby = any(cdt$cell.v == "by" & !is.na(cdt$cell.v))
	hby = any(cdt$cell.h == "by" & !is.na(cdt$cell.h))

	toC = function(x) {
		paste(x, collapse = "_")
	}


	# manual outside legends -2 is top or left, -1 is bottom or right
	cdt[class %in% c("autoout", "out"), ':='(facet_row =
			ifelse(cell.v == "center", ifelse(vby, "1", toC(1:o$nrows)),
			ifelse(cell.v == "by", as.character(by1__),
			ifelse(cell.v == "top", as.character(-2), as.character(-1)))),
		facet_col =
			ifelse(cell.h == "center", ifelse(hby, "1", toC(1:o$ncols)),
			ifelse(cell.h == "by", as.character(by2__),
			ifelse(cell.h == "left", as.character(-2), as.character(-1)))))]
	if (o$type == "page") {
		cdt[cell.v == "by", facet_row := "1"]
		cdt[cell.h == "by", facet_col := "1"]
	}

	if (o$type == "page") {
		cdt[, by1__ := by3__]
		cdt[, by3__ := NA]
	}


	cdt
}

step4_plot = function(tm, vp, return.asp, show, in.shiny, knit, args) {
	tmx = tm$tmo
	o = tm$o
	aux = tm$aux
	cmp = tm$cmp
	prx = tm$prx

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

	# get name of graphics engine (for function names e.g. tmapGridInit)
	gs = tmap_graphics_name()

	o = prepreprocess_meta(o, vp)

	# get legends from layer data and put them in "components data.table" (cdt)

	cdt_cmp = if (length(cmp) && !o$legend.only) {
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
			cc$position = l = complete_options(tm_pos_in("left", "top"), o$legend.position)
			cc
		})
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

	# function to subset data
	get_dt = function(dt, by1, by2, by3) {
		b = list(by1, by2, by3)
		bynames = intersect(names(dt), paste0("by", 1:3, "__"))
		byids = as.integer(substr(bynames, 3, 3))

		sel = rep(TRUE, nrow(dt))
		if (length(bynames)) {
			for (i in 1:length(bynames)) {
				sel = sel & dt[[bynames[i]]] %in% b[[byids[i]]]
			}
		}
		dt[which(sel),]
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
		list(list(bb_ext(stm_merge_bbox(bbxs), o$inner.margins)))
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
		if (length(mains_in_grp)) {
			lookup = match(mains_in_grp, grp_ids)
			tmain = unlist(unlist(tmx[lookup], recursive = FALSE, use.names = FALSE), recursive = FALSE, use.names = FALSE)
			d[, bbox:=do.call(get_bbox, as.list(.SD)), by = grps, .SDcols = c("by1", "by2", "by3")]
		} else {
			if (is.null(bbm)) {
				bbo = o$bbox
				if (!is.null(bbo)) {
					bbm = tmaptools::bb(bbo)
				} else {
					bbm = sf::st_bbox()
				}
			}
			d[, bbox:=rep(list(bbm),nrow(d))]
		}
	} else {
		if (is.null(bbm)) {
			bbo = o$bbox
			if (!is.null(bbo)) {
				bbm = tmaptools::bb(bbo)
			} else {
				bbm = sf::st_bbox()
			}
		}
		d = data.table::data.table(by1 = 1L, by2 = 1L, by3 = 1L, i = 1, bbox = list(bbm))
	}

	if (!o$legend.only) {
		d[, asp:=get_asp(bbox)]
		d = d[!is.na(asp)]


		if (!(o$type %in% c("grid", "page")) && !is.na(o$nrows) && !is.na(o$ncols)) {
			# limit facets
			n_lim = limit_nx(o$n)
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
		} else {
			# wrap
			if (o$facet.flip) {
				d[, row := as.integer((i - 1) %% o$nrows + 1)]
				d[, col := as.integer((((i - 1) %/% o$nrows + 1) - 1) %% o$ncols + 1)]
			} else {
				d[, col := as.integer((i - 1) %% o$ncols + 1)]
				d[, row := as.integer((((i - 1) %/% o$ncols + 1) - 1) %% o$nrows + 1)]
			}

		}
		d[, page := as.integer(i - 1) %/% (o$nrows * o$ncols) + 1]


		### facet.flip and reverse
		# if (o$facet.flip) {
		# 	labcols= o$panel.labels[[1]]
		# 	labrows = o$panel.labels[[2]]
		# 	nr = o$nrows
		# 	o$nrows = o$ncols
		# 	o$ncols = nr
		# } else {
		# 	labrows = o$panel.labels[[1]]
		# 	labcols = o$panel.labels[[2]]
		# }
		#


		# # reverse if specified (with '-' in front of row/col/page variable name in tm_facets)
		# if (o$rev1) {
		# 	labs1 = o$panel.labels[[1]]
		# 	d[, by1:=(1L+length(labs1)) - by1]
		# 	o$panel.labels[[1]] = structure(rev(labs1), showNA = attr(labs1, "showNA"))
		# }
		# if (o$rev2) {
		# 	labs2 = o$panel.labels[[2]]
		# 	d[, by2:=(1L+length(labs2)) - by2]
		# 	o$panel.labels[[2]] = rev(labs2)
		# }
		# if (o$rev3) {
		# 	d[, by3:=(1L+max(by3)) - by3]
		# }
		#
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

	if (!o$legend.only) {
		# create table with bounding boxes (the only important property, apart from settings)
		db = data.table(bbox = unique(d$bbox[!is.na(d$asp)]))
		db[, i:=1L:nrow(db)]
		d[, bi:=db$i[match(d$bbox, db$bbox)]]

		## process components
		if (nrow(cdt)) cdt = process_components2(cdt, o)

		# init
		asp = do.call(FUNinit, c(list(o = o, return.asp = return.asp, vp = vp, prx = prx), args))
		if (return.asp) return(asp)

		## prepare aux layers
		if (length(aux)) {
			# prepare aux layers and return group label (in case it is not user specified)
			aux_group_def = mapply(function(a, id) {
				FUNaux_prep = paste0("tmap", gs, a$mapping.fun, "Prep")
				do.call(FUNaux_prep, list(a = a$args, bs = db$bbox, id = id, o = o))
			}, aux, 1:length(aux))
			aux_group = mapply(function(a, agd) {
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
			if (o$panel.type == "wrap") do.call(FUNwrap, list(label = o$panel.labels[[1]][d$i[i]], facet_row = d$row[i], facet_col = d$col[i], facet_page = d$page[i], o = o))
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

						FUN = paste0("tmap", gs, bl$mapping_fun)

						do.call(FUN, c(list(shpTM = shpTM, dt = mdt, pdt = bl$popup.data, popup.format = bl$popup.format, hdt = bl$hover.data, idt = bl$id.data, gp = gp, bbx = bbx, facet_col = d$col[i], facet_row = d$row[i], facet_page = d$page[i], id = id, pane = pane, group = group, o = o), bl$mapping_args))
					}

				} else {
					glid = q$glid[qi]


					# aux layer
					a = aux[[glid]]
					FUNaux_plot = paste0("tmap", gs, a$mapping.fun)

					id = glid # to do: test!
					do.call(FUNaux_plot, list(bi = d$bi[i], bbx = bbx, facet_col = d$col[i], facet_row = d$row[i], facet_page = d$page[i], id = id, pane = pane, group = group, o = o))

				}
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
		legs_out[, page:=NA_integer_]

		# legs_out[, bbox:=list()]
		# legs_out[, units:=list()]

		# ad-hoc method: take first bbox and units
		bbox_nb = d$bbox[1]
		attr(bbox_nb, "borrow") = list(col = d$col[1], row = d$row[1])
		legs_out[, bbox:=list(bbox_nb)]
		legs_out[, units:=d$units[1]]


		cdt = data.table::rbindlist(c(list(legs_out), legs_in))

		cdt$comp = mapply(function(cmp, bbx, u) {
			cmp$bbox = bbx
			cmp$units = u
			cmp
		}, cdt$comp, cdt$bbox, cdt$units, SIMPLIFY = FALSE)

	}







	legfun = paste0("tmap", gs, "Comp")

	toI = function(x) {
		as.integer(strsplit(x, split = "_")[[1]])
	}

	if (o$type == "page") {
		cdt$page = cdt$by1__
	}

	if (nrow(cdt) > 0L) for (k in seq_len(o$npages)) {
		klegs = cdt[is.na(page) | (page == k), ] # was by3__ instead of page
		klegs[, pos.h.id := pos.h][pos.h %in% c("left", "center", "right"), pos.h.id:="lower"][pos.h %in% c("LEFT", "CENTER", "RIGHT"), pos.h.id:="upper"]
		klegs[, pos.v.id := pos.v][pos.v %in% c("top", "center", "bottom"), pos.v.id:="lower"][pos.v %in% c("TOP", "CENTER", "BOTTOM"), pos.v.id:="upper"]
		klegs[, id:=paste(pos.h.id, pos.v.id, sep = "__")]

		klegs[, do.call(legfun, args = list(comp = .SD$comp, o = o, facet_row = toI(.SD$facet_row[1]), facet_col = toI(.SD$facet_col[1]), facet_page = k, class = .SD$class[1], stack = .SD$stack, stack_auto = .SD$stack_auto, pos.h = .SD$pos.h, pos.v = .SD$pos.v, .SD$bbox)), by = list(facet_row, facet_col, id), .SDcols = c("comp", "facet_row", "facet_col", "class", "stack", "stack_auto", "pos.h", "pos.v", "bbox")]
	}

	if (show) save_last_map()

	if (.TMAP$in.shiny) {
		.TMAP$q = q
	}

	do.call(FUNrun, list(o = o, q = q, show = show, knit = knit, args))
}
