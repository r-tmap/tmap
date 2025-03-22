step2_data = function(tm) {
	dev = getOption("tmap.devel.mode")

	tmo = tm$tmo
	o = tm$o
	aux = tm$aux
	cmp = tm$cmp
	prx = tm$prx

	if (is.null(tmo)) {
		return(	list(tmo = NULL, aux = aux, cmp = cmp, prx = prx, o = o))
	}

	groupnames = paste0("group", seq_along(tmo))
	fl = list(1L, 1L, 1L)
	assign("fl", fl, envir = .TMAP)

	# to reset the legends (which are  temporarily stored in .TMAP environment)
	legends_init()
	charts_init()

	grps = lapply(tmo, function(tmg) {
		tmf = tmg$tmf
		dt = tmg$tms$dt

		if ("by1__" %in% names(dt) && o$rev1) dt[, by1__ := (o$fn[1]+1L)-by1__]
		if ("by2__" %in% names(dt) && o$rev2) dt[, by2__ := (o$fn[2]+1L)-by2__]
		if ("by3__" %in% names(dt) && o$rev3) dt[, by3__ := (o$fn[3]+1L)-by3__]


		if (o$facet.flip && !o$type %in% c("wrapstack", "wrap", "stack")) {
			if ("by2__" %in% names(dt)) {
				dt[, by2b__:= by2__]
				dt[, by2__ := NULL]
			}
			if ("by1__" %in% names(dt)) {
				dt[, by2__:= by1__]
			}
			if ("by2b__" %in% names(dt)) {
				dt[, by1__ := by2b__]
				dt[, by2b__ := NULL]
			}

			tmf = within(tmf, {
				b = ifelse(b == 1L, 2L, ifelse(b == 2L, 1L, b))
				v = ifelse(v == 1L, 2L, ifelse(v == 2L, 1L, v))
				by__ = ifelse(by__ == "by1__", "by2__", ifelse(by__ == "by2__", "by1__", by__))
				var__ = ifelse(var__ == "by1__", "by2__", ifelse(var__ == "by2__", "by1__", var__))
				gn = gn[c(2,1,3)]
				gl = gl[c(2,1,3)]
				#fl = fl[c(2,1,3)]
				#fn = fn[c(2,1,3)]
			})
		}

		shpvars = tmg$tms$smeta$vars

		# step2_data_grp_prepare
		#tmf_meta = step2_data_grp_prepare(tmg$tmf, grpvars, dt)

		layernames = paste0("layer", seq_along(tmg$tmls))
		lrs = lapply(tmg$tmls, function(tml) {
			#cat("step2_grp_lyr======================\n")

			gp = tml$gpar
			tp = tml$tpar

			plot.order = tml$plot.order

			group = if (is.na(tml$group)) tmg$tms$shp_name else as.character(tml$group)
			group.control = as.character(tml$group.control)

			.TMAP$popup.format = tml$popup.format

			# args will be passed on to the scale functions (in case needed)
			# they also will be used in step 3 (trans) and step 4 (mapping)
			trans = mapply(getdts, tml$trans.aes, names(tml$trans.aes), SIMPLIFY = FALSE, MoreArgs = list(p = tp, q = tmf, o = o, dt = dt, shpvars = shpvars, layer = tml$layer, group = group, mfun = tml$mapping.fun, args = tml$trans.args, plot.order = plot.order))

			mapping = mapply(getdts, tml$mapping.aes, names(tml$mapping.aes), SIMPLIFY = FALSE, MoreArgs = list(p = gp, q = tmf, o = o, dt = dt, shpvars = shpvars, layer = tml$layer, group = group, mfun = tml$mapping.fun, args = tml$mapping.args, plot.order = plot.order))

			dts_trans = cbind_dts(lapply(trans, function(x) x$dt), plot.order)
			trans_legend = lapply(trans, function(x) x$leg)

			dts_mapping = cbind_dts(lapply(mapping, function(x) x$dt), plot.order)
			mapping_legend = lapply(mapping, function(x) x$leg)
			if (dev) timing_add(s4 = "combine")
			if (dev) timing_add(s3 = paste0("layer ", tml$layer))


			if (!length(tml$popup.vars)) {
				popup.data = NULL
			} else {
				popup.data = copy(dt[, c(tml$popup.vars, "tmapID__"), with = FALSE])
				if (!is.null(names(tml$popup.vars))) popup.data = data.table::setnames(popup.data, old = unname(tml$popup.vars), new = names(tml$popup.vars))
			}
			hover.data = if (tml$hover == "") {
				NULL
			} else {
				data.table(hover = as.character(dt[[tml$hover]]), tmapID__ = dt$tmapID__)
			}
			id.data = if (tml$id == "") {
				NULL
			} else {
				data.table(id = as.character(dt[[tml$id]]), tmapID__ = dt$tmapID__)
			}

			format_called = attr(tml$popup.format, "called")
			if (length(format_called) > 0L) {
				.TMAP$popup.format[format_called] = tml$popup.format[format_called]
			}


			list(trans_dt = dts_trans,
				 trans_legend = trans_legend,
				 trans_fun = tml$trans.fun,
				 trans_args = tml$trans.args,
				 trans_apply = tml$trans.apply_to,
				 mapping_dt = dts_mapping,
				 mapping_legend = mapping_legend,
				 mapping_fun = tml$mapping.fun,
				 mapping_args = tml$mapping.args,
				 lid = tml$lid,
				 group = group,
				 group.control = group.control,
				 popup.data = popup.data,
				 popup.format = .TMAP$popup.format,
				 hover.data = hover.data,
				 id.data = id.data,
				 plot.order = plot.order, # passed on for step 3 non-data driven transformation
				 gp = gp,
				 tp = tp)
		})
		if (length(lrs)) names(lrs) = layernames

		shpDT = data.table(shpTM = list(tmg$tms$shpTM))
		if (dev) timing_add(s2 = "group")

		list(layers = lrs, shpDT = shpDT)
	})
	names(grps) = groupnames
	#attr(grps, "fl") = fl
	#attr(grps, "main") = attr(tmo, "main")
	#attr(grps, "crs") = attr(tmo, "crs")

	#tmf = get_tmf(lapply(tmo, function(tmoi) tmoi$tmf))



	# facet labels "_old' were obtained in step2, and still determine data levels
	# currently, facet labels are determined in step1, but they need to be the same as the data labels
	# o$fl_old = get("fl", envir = .TMAP)
	# o$fn_old = sapply(o$fl_old, function(f) {
	# 	if (is.character(f)) length(f) else f
	# })
	# o$fl_old = lapply(o$fl_old, function(f) {
	# 	if (is.character(f)) f else NULL
	# })
	#
	# if (!identical(o$fn, o$fn_old) || !identical(o$fl, o$fl_old)) {
	# 	po(o$fn_old, o$fl_old, o$fn, o$fl)
	# 	stop("fl not identical")
	# }



	#cat("fl old:\n")
	#print(o$fl_old)


	#o = c(o, tmf)

	# attr(grps, "is.wrap") = tmo[[1]]$tmf$is.wrap
	# attr(grps, "nrows") = tmo[[1]]$tmf$nrows
	# attr(grps, "ncols") = tmo[[1]]$tmf$ncols

	o = within(o, {
		if (rev1) fl[[1]][] = rev(fl[[1]][])
		if (rev2) fl[[2]][] = rev(fl[[2]][])
		if (rev3) fl[[3]][] = rev(fl[[3]][])
		if (facet.flip  && !type %in% c("wrapstack", "wrap", "stack")) {
			fl[] = fl[c(2,1,3)]
			fn = fn[c(2,1,3)]
		}
	})

	list(tmo = grps, aux = aux, cmp = cmp, prx = prx, o = o)
}
