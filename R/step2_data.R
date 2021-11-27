step2_data = function(tm) {
	dev = getOption("tmap.devel.mode")
	
	tmo = tm$tmo
	meta = tm$meta
	aux = tm$aux
	
	meta = preprocess_meta_step2(meta)
	
	groupnames = paste0("group", seq_along(tmo))
	fl = list(1L, 1L, 1L)
	assign("fl", fl, envir = .TMAP)
	
	# to reset the legends (which are  temporarily stored in .TMAP environment)
	legends_init()
	
	grps = lapply(tmo, function(tmg) {
		
		dt = tmg$tms$dt
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
			
			trans = mapply(getdts, tml$trans.aes, names(tml$trans.aes), SIMPLIFY = FALSE, MoreArgs = list(p = tp, q = tmg$tmf, o = meta, dt = dt, shpvars = shpvars, layer = tml$layer, plot.order = plot.order))
			
			#cat("step2_grp_lyr_mapping_____________\n")
			
			mapping = mapply(getdts, tml$mapping.aes, names(tml$mapping.aes), SIMPLIFY = FALSE, MoreArgs = list(p = gp, q = tmg$tmf, o = meta, dt = dt, shpvars = shpvars, layer = tml$layer, plot.order = plot.order))

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
	# meta$fl_old = get("fl", envir = .TMAP)
	# meta$fn_old = sapply(meta$fl_old, function(f) { 
	# 	if (is.character(f)) length(f) else f
	# })
	# meta$fl_old = lapply(meta$fl_old, function(f) {
	# 	if (is.character(f)) f else NULL
	# })
	# 
	# if (!identical(meta$fn, meta$fn_old) || !identical(meta$fl, meta$fl_old)) {
	# 	po(meta$fn_old, meta$fl_old, meta$fn, meta$fl)
	# 	stop("fl not identical")
	# }
	
	
	
	#cat("fl old:\n")
	#print(meta$fl_old)
	
	
	#meta = c(meta, tmf)
	
	# attr(grps, "is.wrap") = tmo[[1]]$tmf$is.wrap
	# attr(grps, "nrows") = tmo[[1]]$tmf$nrows
	# attr(grps, "ncols") = tmo[[1]]$tmf$ncols
	
	list(tmo = grps, aux = aux, meta = meta)
}
