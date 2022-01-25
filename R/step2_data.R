step2_data = function(tm) {
	dev = getOption("tmap.devel.mode")
	
	tmo = tm$tmo
	o = tm$o
	aux = tm$aux
	cmp = tm$cmp
	
	o = preprocess_meta_step2(o)
	
	groupnames = paste0("group", seq_along(tmo))
	fl = list(1L, 1L, 1L)
	assign("fl", fl, envir = .TMAP)
	
	# to reset the legends (which are  temporarily stored in .TMAP environment)
	legends_init()
	
	grps = lapply(tmo, function(tmg) {
		tmf = tmg$tmf
		dt = tmg$tms$dt
		
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
				fl = fl[c(2,1,3)]
				fn = fn[c(2,1,3)]
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
			
			#cat("step2_grp_lyr_trans_______________\n")
			
			trans = mapply(getdts, tml$trans.aes, names(tml$trans.aes), SIMPLIFY = FALSE, MoreArgs = list(p = tp, q = tmf, o = o, dt = dt, shpvars = shpvars, layer = tml$layer, plot.order = plot.order))
			
			#cat("step2_grp_lyr_mapping_____________\n")
			
			mapping = mapply(getdts, tml$mapping.aes, names(tml$mapping.aes), SIMPLIFY = FALSE, MoreArgs = list(p = gp, q = tmf, o = o, dt = dt, shpvars = shpvars, layer = tml$layer, plot.order = plot.order))

			dts_trans = cbind_dts(lapply(trans, function(x) x$dt), plot.order)
			trans_legend = lapply(trans, function(x) x$leg)
			
			dts_mapping = cbind_dts(lapply(mapping, function(x) x$dt), plot.order)
			mapping_legend = lapply(mapping, function(x) x$leg)
			if (dev) timing_add(s4 = "combine")
			if (dev) timing_add(s3 = paste0("layer ", tml$layer))
			
			list(trans_dt = dts_trans, 
				 trans_legend = trans_legend, 
				 trans_fun = tml$trans.fun,
				 trans_args = tml$trans.args,
				 trans_isglobal = tml$trans.isglobal,
				 mapping_dt = dts_mapping, 
				 mapping_legend = mapping_legend,
				 mapping_fun = tml$mapping.fun,
				 lid = tml$lid,
				 plot.order = plot.order, # passed on for step 3 non-data driven transformation
				 gp = gp,
				 tp = tp)
		})
		names(lrs) = layernames
		
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
	
	list(tmo = grps, aux = aux, cmp = cmp, o = o)
}
