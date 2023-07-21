tmapScaleBivariate = function(x1, x2, scale, legend, o, aes, layer, layer_args, sortRev, bypass_ord) {
	
	if (!(aes %in% c("fill", "color", "shape"))) stop("tm_scale_bivariate cannot be used for layer ", layer, ", aesthetic ", aes, call. = FALSE)
	
	scale = get_scale_defaults(scale, o, aes, layer, cls = "biv")
	

	dummy = legend
	dummy$show = FALSE
	
	

	res = mapply(function(x, s) {
		f = s$FUN
		s$FUN = NULL
		# update label.format
		s$label.format = process_label_format(scale$label.format, o$label.format)
		r = do.call(f, list(x1 = x, scale = s, legend = dummy, o = o, aes = aes, layer = layer, layer_args = layer_args, sortRev = sortRev, bypass_ord = bypass_ord, submit_legend = FALSE))
		
		leg = r$legend
		vv = leg$vvalues
		lab = leg$labels
		na = leg$na.show
		if (na) {
			vv = head(vv, -1)
			lab = head(lab, -1)
		}
		vid = match(r$vals, vv)
		n = length(lab)
		list(vv = vv, lab = lab, na = na, vid = vid, n = n, ids = r$ids, scale = leg$scale)
		
	}, list(x1, x2), scale[1:2], SIMPLIFY = FALSE)
	
	if (res[[1]]$scale == "continuous" || res[[2]]$scale == "continuous") stop("tm_scale_bivariate not implemented for continuous scales", call. = FALSE)
	
	
	n1 = res[[1]]$n # rows m
	n2 = res[[2]]$n # cols n
	
	vals = res[[1]]$vid + (res[[2]]$vid - 1L) * n1

	with(scale, {
		fun_check = paste0("tmapValuesCheck_", aes)
		
		are_valid = do.call(fun_check, args = list(x = values))
		if (!are_valid) stop("Incorrect values for layer ", layer, ", aesthetic ", aes, "; values should conform aes ", aes, call. = FALSE)
		
		fun_getBVV = paste0("tmapValuesBVV_", aes)
		VV = do.call(fun_getBVV, list(x = values, value.na = value.na, m = n1, n = n2, scale = values.scale * o$scale, rep = values.repeat, o = o))
		
		vvalues = VV$vvalues
		value.na = VV$value.na

		
		#pal = c4a(cols4all::.P$c4a$bivs$pu_gn_bivs, n = n2, m = n1)[n1:1,]
		#colorNA = c4a_na(cols4all::.P$c4a$bivs$pu_gn_bivs)
		
		#pal = as.vector(pal)
		
		
		pal = as.vector(vvalues[n1:1,])
		
		attr(pal, "biv") = TRUE
		attr(pal, "m") = n1
		attr(pal, "n") = n2
		
		vals = pal[vals]
		
		vals[is.na(vals)] = value.na
		
		ids = res[[1]]$ids + res[[2]]$ids
		
		na = res[[1]]$na || res[[2]]$na
		
		
		legend$labels = c(res[[1]]$lab, res[[2]]$lab)
		legend$nitems = n1 + na
		legend$dvalues = vals
		legend$vvalues = pal
		legend$vneutral = pal[4]
		legend$na.show = na
		
		if (length(legend$title) == 2L) legend$title = c(legend$title, "")
		#legend$title = "BIVARIATE"
		# 
		# legend = within(legend, {
		# 	nitems = length(labs)
		# 	labels = labs
		# 	dvalues = vals
		# 	vvalues = values
		# 	vneutral = value.neutral
		# 	na.show = get("na.show", envir = parent.env(environment()))
		# })
		
		if (bypass_ord) {
			format_aes_results(vals, legend = legend)
		} else {
			format_aes_results(vals, ids, legend)			
		}
		
	})
	
}
