tmapValuesCheck_fill = function(x) {
	palid = tmapPalId(x[1])
	if (!is.na(palid)) TRUE else all(valid_colors(x))
}

tmapValuesCheck_col = function(x) {
	tmapValuesCheck_fill(x)
}


tmapValuesCheck_shape = function(x) {
	# to do
	TRUE
}


tmapValuesCheck_size = function(x) {
	inherits(x, "tmapSeq") || (is.numeric(x) && (all(x>=0) || all(x<=0)))
}

tmapValuesCheck_lwd = function(x) {
	tmapValuesCheck_size(x)
}

tmapValuesCheck_col_alpha= function(x) {
	tmapValuesCheck_size(x)
}

tmapValuesCheck_fill_alpha = function(x) {
	tmapValuesCheck_size(x)
}

tmapValuesCheck_area = function(x) {
	tmapValuesCheck_size(x)
}



tmapValuesIsDiv_fill = function(x) {
	palid = tmapPalId(x[1])
	if (!is.na(palid)) {
		.tmap_pals$type[palid] == "div"
	} else {
		(palette_type(x) == "div")
	}
}

tmapValuesIsDiv_col = function(x) {
	tmapValuesIsDiv_fill(x)
}

tmapValuesIsDiv_size = function(x) {
	inherits(x, "tmapSeq") && (x$from < 0) && (x$to > 1) || (is.numeric(x) && (any(x < 0) && any(x> 0)))
}

tmapValuesIsDiv_lwd = function(x) {
	tmapValuesIsDiv_size(x)
}

tmapValuesIsDiv_col_alpha = function(x) {
	tmapValuesIsDiv_size(x)
}

tmapValuesIsDiv_fill_alpha = function(x) {
	tmapValuesIsDiv_size(x)
}

tmapValuesIsDiv_area = function(x) {
	tmapValuesIsDiv_size(x)
}

tmapValuesIsDiv_shape = function(x) {
	FALSE
}

tmapValuesContrast_fill = function(x, n, isdiv) {
	palid = tmapPalId(x[1])
	
	if (!is.na(palid)) {
		if (.tmap_pals$type[palid] %in% c("cat", "cyc", "biv")) {
			c(0, 1)
		} else if (isdiv) {
			default_contrast_div(n)	
		} else {
			default_contrast_seq(n)	
		}
	} else c(0, 1)
}

tmapValuesContrast_col = function(x, n, isdiv) {
	tmapValuesContrast_fill(x, n, isdiv)
}

tmapValuesContrast_shape = function(x, n, isdiv) {
	c(0, 1)
}

tmapValuesContrast_lty = function(x, n, isdiv) {
	c(0, 1)
}


tmapValuesContrast_size = function(x, n, isdiv) {
	c(.5/n, 1 - .5/n)
}

tmapValuesContrast_lwd = function(x, n, isdiv) {
	tmapValuesContrast_size(x, n, isdiv)
}

tmapValuesContrast_col_alpha = function(x, n, isdiv) {
	tmapValuesContrast_size(x, n, isdiv)
}

tmapValuesContrast_fill_alpha = function(x, n, isdiv) {
	tmapValuesContrast_size(x, n, isdiv)
}


tmapValuesContrast_area = function(x, n, isdiv) {
	tmapValuesContrast_size(x, n, isdiv)
}

tmapValuesVV_fill = function(x, isdiv, n, dvalues, are_breaks, midpoint, contrast, rep) {
	palid = tmapPalId(x[1])
	
	if (isdiv) {
		cat0 = (are_breaks != any(dvalues==midpoint))
		
		nneg = max(0L, sum(dvalues < midpoint) - cat0) # max 0L needed when midpoint is outside range (and cat0 is true)
		npos = max(0L, sum(dvalues > midpoint) - cat0)
		
		nmax = max(nneg, npos)
		
		ntot = 2L * nmax + cat0
		
		ids = (1L + max(0L, (npos-nneg))):(ntot - max(0L, (nneg-npos)))
	} else {
		ids = 1L:n
	}
	
	scale_ids = function(ids, n) {
		1 + ((ids - 1) / (n - 1)) * 100	
	} 
	
	map_ids = function(i, s, n) {
		di = i[2] - i[1]
		seq(i[1] + di * s[1], i[1] + di * s[2], length.out = n)
	}
	
	if (contrast[1] != 0 || contrast[2] != 1) {
		# expand palette tot 101 colors
		if (!is.na(palid)) {
			vvalues = tmap_get_palette(x, n = 101)
		} else {
			vvalues = grDevices::colorRampPalette(x)(101)
		}
		
		# expand ids and apply contrast
		if (isdiv) {
			ids_scaled = scale_ids(ids, ntot)
			
			
			ids_after_contrast = c({if (nneg > 0) head(map_ids(ids_scaled[c(1L, (nneg+cat0))], 
												1-rev(contrast), 
												n = nneg + cat0), 
										nneg) else NULL},
								   {if (cat0) ids_scaled[1L + nneg] else NULL},
								   if (npos > 0) tail(map_ids(ids_scaled[c(nneg+1, n)], 
								   			 contrast, 
								   			 n = npos + cat0), 
								   	 npos) else NULL)
		} else {
			ids_scaled = scale_ids(ids, n)
			ids_after_contrast = map_ids(ids_scaled[c(1L, n)], contrast, n)
		}
		
	} else {
		if (!is.na(palid) && isdiv) {
			vvalues = tmap_get_palette(x, n = n, rep = rep)
		} else if (!is.na(palid) && !isdiv) {
			vvalues = tmap_get_palette(x, n = n, rep = rep)
		} else if (length(x) != n && isdiv) {
			vvalues = grDevices::colorRampPalette(x)(n)
		} else if (length(x) != n && !isdiv) {
			vvalues = grDevices::colorRampPalette(x)(n)
		} else {
			if (!all(valid_colors(x))) stop("invalid colors", call. = FALSE)
			vvalues = col2hex(x)
		}		
		ids_after_contrast = ids
	}
	
	
	vvalues = vvalues[ids_after_contrast]
	
	if (isdiv) {
		if (cat0) {
			value.neutral = vvalues[1L + nneg]
		} else {
			value.neutral = grDevices::colorRampPalette(vvalues[c(nneg, nneg + 1L)])(1)
		}
	} else {
		value.neutral = vvalues[n/2]
	}
	
	list(vvalues = vvalues, value.neutral = value.neutral)
	
}

tmapValuesVV_col = function(...) {
	tmapValuesVV_fill(...)
	#do.call(tmapValuesVV_fill, args = list(...))
}

tmapValuesVV_shape = function(x, isdiv, n, dvalues, are_breaks, midpoint, contrast, rep) {
	list(vvalues = rep(x, length.out = n), value.neutral = x[1])
}

tmapValuesVV_lty = function(x, isdiv, n, dvalues, are_breaks, midpoint, contrast, rep) {
	list(vvalues = rep(x, length.out = n), value.neutral = x[1])
}


tmapValuesVV_size = function(x, isdiv, n, dvalues, are_breaks, midpoint, contrast, rep) {
	#break_mids = breaks[-(n+1)] + (breaks[-1] - breaks[-(n+1)]) / 2
	
	
	#vvalues = seq(x[1], x[2])
	vvalues = if (is.numeric(x) && length(x) == n) {
		if (contrast[1] !=0 || contrast[2] != 1) {
			warning("values.contrast not used because the individual values have been specified (instead of a sequence)", call. = FALSE)
		}
		x
	} else {
		if (is.numeric(x)) {
			x = tmap_seq(x[1], x[length(x)], power = "lin")
		}
		
		if (contrast[1] !=0 || contrast[2] != 1) {
			contrast_curved = tmapSeq(tmap_seq(from = contrast[1], to = contrast[2], power = x$power), n = 2, rescale = FALSE)
			x$from = x$from + contrast_curved[1] * (x$to - x$from)
			x$to = x$from + contrast_curved[2] * (x$to - x$from)
		}
		tmapSeq(x, n)
		
	}
	
	# (inherits(x, "tmapSeq")) {
	# 	tmapSeq(x, n)
	# } else if (length(x) == n) {
	# 	x 
	# } else {
	# 	seq(x[1], x[n], length.out = n)
	# }
	# 
	# if (isdiv) {
	# 	colpal =  seq(x[1], x[2], length.out = 1001)[map2divscaleID(breaks - midpoint, n=1001, contrast=contrast)]
	# } else {
	# 	#colpal =  seq(values[1], values[2], length.out = n) #seq(palette[1], palette[2], length.out = 1001)[map2seqscaleID(breaks, n=1001, contrast=contrast, breaks.specified=breaks.specified, show.warnings = show.warnings)]
	# 	colpal = seq(x[1], x[2], length.out = 1001)[map2seqscaleID(breaks, n = 1001, contrast = contrast)]
	# }
	# vvalues = colpal
	value.neutral = vvalues[round((n+1)/2)]
	
	
	list(vvalues = vvalues, value.neutral = value.neutral)
}

tmapValuesVV_lwd = function(...) {
	do.call(tmapValuesVV_size, args = list(...))
}

tmapValuesVV_col_alpha = function(...) {
	do.call(tmapValuesVV_size, args = list(...))
}

tmapValuesVV_fill_alpha = function(...) {
	do.call(tmapValuesVV_size, args = list(...))
}

tmapValuesVV_area = function(...) {
	do.call(tmapValuesVV_size, args = list(...))
}





tmap_seq = function(from = 0, to = 1, power = c("lin", "sqrt", "sqrt_perceptual", "quadratic")) {
	structure(as.list(environment()), class = "tmapSeq")
}

tmapSeq = function(s, n, rescale = TRUE) {
	with(s, {
		p = if (is.numeric(power)) power else switch(power, lin = 1, sqrt = 0.5, sqrt_perceptual = 0.5716, squadratic = 2)
		if (is.null(p)) p = as.nuermic
		r = seq(from = from, to = to, length.out = n) ^ p
		if (rescale) {
			(r - (r[1])) / (r[n] - r[1]) * (to - from) + from	
		} else {
			r
		}
	})
}





tmapValuesCVV_fill = function(x, n, contrast, rep) {
	
	
	# process values
	palid = tmapPalId(x[1])
	
	arecolors = !is.na(palid) || (is.na(palid) && valid_colors(x[1]))
	
	# if (arecolors) {
	values = if (!is.na(palid)) {
		tmap_get_palette(x, n, rep = rep, contrast = contrast)
	} else if (!rep && (length(x) < n)) {
		grDevices::colorRampPalette(x)(n)
	} else {
		rep(x, length.out=n)
	}
	
	list(vvalues = values, value.neutral = values[1])
	
	# } else if (arenumbers) {
	# 	values = if (length(scale$values) == 2) seq(scale$values[1], scale$values[2], length.out = n) else rep(scale$values, length.out = n)
	# } else {
	# 	values = rep(scale$values, length.out = n)
	# }
	
}

tmapValuesCVV_col = function(x, n, contrast, rep) {
	tmapValuesCVV_fill(x, n, contrast, rep)
}

tmapValuesCVV_size = function(x, n, contrast, rep) {
	tmapValuesVV_size(x = x, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, contrast = contrast, rep = rep)
}

tmapValuesCVV_lwd = function(x, n, contrast, rep) {
	tmapValuesVV_lwd(x = x, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, contrast = contrast, rep = rep)
}

tmapValuesCVV_col_alpha = function(x, n, contrast, rep) {
	tmapValuesVV_col_alpha(x = x, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, contrast = contrast, rep = rep)
}

tmapValuesCVV_fill_alpha = function(x, n, contrast, rep) {
	tmapValuesVV_fill_alpha(x = x, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, contrast = contrast, rep = rep)
}

tmapValuesCVV_area = function(x, n, contrast, rep) {
	tmapValuesVV_area(x = x, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, contrast = contrast, rep = rep)
}


tmapValuesCVV_shape = function(x, n, contrast, rep) {
	tmapValuesVV_shape(x = x, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, contrast = contrast, rep = rep)
}

tmapValuesCVV_lty = function(x, n, contrast, rep) {
	tmapValuesVV_lty(x = x, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, contrast = contrast, rep = rep)
}
