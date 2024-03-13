tmapValuesCheck_col = function(x) {
	((!is.null(getPalMeta(x[1])) && length(x) == 1L)  || all(valid_colors(x))) && !is.numeric(x)
}

tmapValuesCheck_fill = function(x) {
	tmapValuesCheck_col(x)
}

tmapValuesCheck_bgcol = function(x) {
	tmapValuesCheck_col(x)
}

tmapValuesCheck_shape = function(x) {
	isSymbol = function(s) {
		inherits(s, "grob") || any(vapply(s, inherits, FUN.VALUE = logical(1), "grob")) || ("iconUrl" %in% names(s))
	}

	if (all(is.numeric(x))) {
		TRUE
	} else if (is.list(x)) {
	 	if (isSymbol(x)) {
	 		TRUE
	 	} else {
	 		all(vapply(x, isSymbol, FUN.VALUE = logical(1)))
	 	}
	} else FALSE
}


tmapValuesCheck_size = function(x) {
	inherits(x, "tmapSeq") || (is.numeric(x) && (all(x>=0) || all(x<=0)))
}

tmapValuesCheck_lwd = function(x) {
	tmapValuesCheck_size(x)
}

tmapValuesCheck_lty = function(x) {
	# to do
	TRUE
}

tmapValuesCheck_xmod = tmapValuesCheck_ymod = function(x) {
	# to do
	TRUE
}

tmapValuesCheck_col_alpha= function(x) {
	tmapValuesCheck_size(x)
}

tmapValuesCheck_fill_alpha = function(x) {
	tmapValuesCheck_size(x)
}

tmapValuesCheck_bgcol_alpha= function(x) {
	tmapValuesCheck_size(x)
}

tmapValuesCheck_area = function(x) {
	tmapValuesCheck_size(x)
}

tmapValuesCheck_text = function(x) {
	is.character(x)
}

tmapValuesCheck_fontface = function(x) {
	(is.numeric(x) && (all(x %in% 1:5))) || (is.character(x) && (all(x %in% c("plain", "bold", "italic", "oblique", "bold.italic", "cyrillic", "cyrillic.oblique", "EUC"))))
}

tmapValuesIsDiv_fill = function(x) {
	m = getPalMeta(x[1])
	ispal = !is.null(m)
	
	if (ispal) {
		m$type == "div"
	} else {
		(palette_type(x) == "div")
	}
}

tmapValuesIsDiv_col = function(x) {
	tmapValuesIsDiv_fill(x)
}

tmapValuesIsDiv_bgcol = function(x) {
	tmapValuesIsDiv_fill(x)
}

tmapValuesIsDiv_size = function(x) {
	inherits(x, "tmapSeq") && (x$from < 0) && (x$to > 1) || (is.numeric(x) && (any(x < 0) && any(x> 0)))
}

tmapValuesIsDiv_lwd = function(x) {
	tmapValuesIsDiv_size(x)
}

tmapValuesIsDiv_lty = function(x) {
	FALSE
}

tmapValuesIsDiv_col_alpha = function(x) {
	tmapValuesIsDiv_size(x)
}

tmapValuesIsDiv_fill_alpha = function(x) {
	tmapValuesIsDiv_size(x)
}

tmapValuesIsDiv_bgcol_alpha = function(x) {
	tmapValuesIsDiv_size(x)
}

tmapValuesIsDiv_area = function(x) {
	tmapValuesIsDiv_size(x)
}

tmapValuesIsDiv_xmod = tmapValuesIsDiv_ymod = function(x) {
	tmapValuesIsDiv_size(x)
}


tmapValuesIsDiv_shape = function(x) {
	FALSE
}

tmapValuesIsDiv_text = function(x) {
	FALSE
}

tmapValuesIsDiv_fontface = function(x) {
	FALSE
}


tmapValuesRange_fill = function(x, n, isdiv) {
	m = getPalMeta(x[1])
	
	if (!is.null(m)) {
		NA # in c4a palette definition
	} else c(0, 1)
}

tmapValuesRange_col = function(x, n, isdiv) {
	tmapValuesRange_fill(x, n, isdiv)
}

tmapValuesRange_bgcol = function(x, n, isdiv) {
	tmapValuesRange_fill(x, n, isdiv)
}

tmapValuesRange_shape = function(x, n, isdiv) {
	c(0, 1)
}

tmapValuesRange_lty = function(x, n, isdiv) {
	c(0, 1)
}




tmapValuesRange_size = function(x, n, isdiv) {
	#print(c(.5/n, 1 - .5/n))
	c(.5/n, 1 - .5/n)
}

tmapValuesRange_lwd = function(x, n, isdiv) {
	tmapValuesRange_size(x, n, isdiv)
}

tmapValuesRange_col_alpha = function(x, n, isdiv) {
	tmapValuesRange_size(x, n, isdiv)
}

tmapValuesRange_fill_alpha = function(x, n, isdiv) {
	tmapValuesRange_size(x, n, isdiv)
}

tmapValuesRange_bgcol_alpha = function(x, n, isdiv) {
	tmapValuesRange_size(x, n, isdiv)
}

tmapValuesRange_area = function(x, n, isdiv) {
	tmapValuesRange_size(x, n, isdiv)
}

tmapValuesRange_xmod = tmapValuesRange_ymod = function(x, n, isdiv) {
	tmapValuesRange_size(x, n, isdiv)
}


tmapValuesRange_text = function(x, n, isdiv) {
	c(0, 1)
}

tmapValuesRange_fontface = function(x, n, isdiv) {
	c(0, 1)
}

tmapValuesVV_fill = function(x, value.na, isdiv, n, dvalues, are_breaks, midpoint, range, scale, rep, o) {
	#palid = tmapPalId(x[1])
	
	m = getPalMeta(x[1])
	

	scale_ids = function(ids, n) {
		1 + ((ids - 1) / (n - 1)) * 100	
	} 
	
	map_ids = function(i, s, n) {
		di = i[2] - i[1]
		seq(i[1] + di * s[1], i[1] + di * s[2], length.out = n)
	}
	
	
	if (isdiv) {
		cat0 = (are_breaks != any(dvalues==midpoint))
		
		nneg = max(0L, sum(dvalues < midpoint) - cat0) # max 0L needed when midpoint is outside range (and cat0 is true)
		npos = max(0L, sum(dvalues > midpoint) - cat0)
		
		nmax = max(nneg, npos)
		
		ntot = 2L * nmax + cat0
		
		ids = (1L + max(0L, (npos-nneg))):(ntot - max(0L, (nneg-npos)))
	} else {
		ntot = n
		ids = 1L:n
	}
	
	if (!is.null(m)) {
		vvalues = getPal(x, n = ntot, range = range)[ids]
	} else {
		pal =colorRampPalette(x)
		if (is.na(range[1])) range = c(0, 1)
		if (range[1] != 0 || range[2] != 1) {
			ids_scaled = scale_ids(ids, ntot)
			if (isdiv) {
				ids_after_range = c({if (nneg > 0) head(map_ids(ids_scaled[c(1L, (nneg+cat0))], 
																1-rev(range), 
																n = nneg + cat0), 
														nneg) else NULL},
									{if (cat0) ids_scaled[1L + nneg] else NULL},
									if (npos > 0) tail(map_ids(ids_scaled[c(nneg+1, n)], 
															   range, 
															   n = npos + cat0), 
													   npos) else NULL)
			} else {
				ids_after_range = map_ids(ids_scaled[c(1L, ntot)], range, ntot)
			}
			vvalues = grDevices::colorRampPalette(x)(101)[ids_after_range]
		} else {
			vvalues = grDevices::colorRampPalette(x)(ntot)[ids]
		}
	}
	
	
	if (isdiv) {
		if (cat0) {
			value.neutral = vvalues[1L + nneg]
		} else {
			value.neutral = vvalues[1L + nneg] # first positive
		}
	} else {
		value.neutral = vvalues[n/2]
	}
	vvalues = do.call(process_color, c(list(col = vvalues), o$pc))
	value.neutral = do.call(process_color, c(list(col = value.neutral), o$pc))
	value.na = do.call(process_color, c(list(col = value.na), o$pc))
	list(vvalues = vvalues, value.neutral = value.neutral, value.na = value.na)
	
}

tmapValuesVV_col = function(...) {
	tmapValuesVV_fill(...)
}

tmapValuesVV_bgcol = function(...) {
	tmapValuesVV_fill(...)
}

tmapValuesVV_shape = function(x, value.na, isdiv, n, dvalues, are_breaks, midpoint, range, scale, rep, o) {
	list(vvalues = rep(x, length.out = n), value.neutral = x[1], value.na = value.na)
}

tmapValuesVV_lty = function(x, value.na, isdiv, n, dvalues, are_breaks, midpoint, range, scale, rep, o) {
	list(vvalues = rep(x, length.out = n), value.neutral = x[1], value.na = value.na)
}


tmapValuesVV_size = function(x, value.na, isdiv, n, dvalues, are_breaks, midpoint, range, scale, rep, o) {
	#break_mids = breaks[-(n+1)] + (breaks[-1] - breaks[-(n+1)]) / 2
	
	
	#vvalues = seq(x[1], x[2])
	vvalues = if (is.numeric(x) && length(x) == n) {
		if (range[1] !=0 || range[2] != 1) {
			warning("values.range not used because the individual values have been specified (instead of a sequence)", call. = FALSE)
		}
		x
	} else {
		if (is.numeric(x)) {
			x = tmap_seq(x[1], x[length(x)], power = "lin")
		}
		
		if (range[1] !=0 || range[2] != 1) {
			range_curved = tmapSeq(tmap_seq(from = range[1], to = range[2], power = x$power), n = 2, rescale = FALSE)
			x$from = x$from + range_curved[1] * (x$to - x$from)
			x$to = x$from + range_curved[2] * (x$to - x$from)
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
	# 	colpal =  seq(x[1], x[2], length.out = 1001)[map2divscaleID(breaks - midpoint, n=1001, range=range)]
	# } else {
	# 	#colpal =  seq(values[1], values[2], length.out = n) #seq(palette[1], palette[2], length.out = 1001)[map2seqscaleID(breaks, n=1001, range=range, breaks.specified=breaks.specified, show.warnings = show.warnings)]
	# 	colpal = seq(x[1], x[2], length.out = 1001)[map2seqscaleID(breaks, n = 1001, range = range)]
	# }
	# vvalues = colpal
	value.neutral = vvalues[round((n+1)/2)]
	
	
	list(vvalues = vvalues * scale, value.neutral = value.neutral * scale, value.na = value.na * scale)
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

tmapValuesVV_bgcol_alpha = function(...) {
	do.call(tmapValuesVV_size, args = list(...))
}

tmapValuesVV_area = function(...) {
	do.call(tmapValuesVV_size, args = list(...))
}

tmapValuesVV_xmod = tmapValuesVV_ymod = function(...) {
	do.call(tmapValuesVV_size, args = list(...))
}

tmapValuesVV_text = function(x, value.na, isdiv, n, dvalues, are_breaks, midpoint, range, scale, rep, o) {
	list(vvalues = rep(x, length.out = n), value.neutral = x[1], value.na = value.na)
}

tmapValuesVV_fontface = function(x, value.na, isdiv, n, dvalues, are_breaks, midpoint, range, scale, rep, o) {
	list(vvalues = rep(x, length.out = n), value.neutral = x[1], value.na = value.na)
}

# for symbols, which need to be 'submitted' (content replaced by an integer)
tmapValuesSubmit_col = function(x, args) x
tmapValuesSubmit_fill = function(x, args) x
tmapValuesSubmit_bgcol = function(x, args) x
tmapValuesSubmit_size = function(x, args) x
tmapValuesSubmit_xmod = function(x, args) x
tmapValuesSubmit_ymod = function(x, args) x
tmapValuesSubmit_lwd = function(x, args) x
tmapValuesSubmit_lty = function(x, args) x
tmapValuesSubmit_shape = function(x, args) {
	if (is.list(x)) {
		gs = tmap_graphics_name()
		fun = paste0("submit_symbols_", gs)
		
		# symbols just specification: 
		# copy-pasted from v3, but not the best place
		# improvement of just needed (-> trans?)
		args = within(args, {
			if (anyNA(just)) {
				just = c(.5, .5)
				just.override = FALSE
			} else {
				just = c(ifelse(is_num_string(just[1]), as.numeric(just[1]), ifelse(just[1]=="left", 1, ifelse(just[1]=="right", 0, .5))),
						  ifelse(is_num_string(just[2]), as.numeric(just[2]), ifelse(just[2]=="bottom", 1, ifelse(just[2]=="top", 0, .5))))
				just.override = TRUE
			}
		})
		
		
		
		do.call(fun, args = list(x, args))
	} else {
		x
	}
}
tmapValuesSubmit_col_alpha = function(x, args) x
tmapValuesSubmit_fill_alpha = function(x, args) x
tmapValuesSubmit_bgcol_alpha = function(x, args) x
tmapValuesSubmit_text = function(x, args) x
tmapValuesSubmit_fontface = function(x, args) x


tmapValuesScale_col = function(x, scale) x
tmapValuesScale_fill = function(x, scale) x
tmapValuesScale_bgcol = function(x, scale) x
tmapValuesScale_size = function(x, scale) x * scale
tmapValuesScale_lwd = function(x, scale) x * scale
tmapValuesScale_lty = function(x, scale) x
tmapValuesScale_shape = function(x, scale) x
tmapValuesScale_col_alpha = function(x, scale) x
tmapValuesScale_fill_alpha = function(x, scale) x
tmapValuesScale_bgcol_alpha = function(x, scale) x
tmapValuesScale_text = function(x, scale) x
tmapValuesScale_fontface = function(x, scale) x
tmapValuesScale_xmod = tmapValuesScale_ymod = function(x, scale) x

tmapValuesColorize_col = function(x, pc) do.call(process_color, c(list(col = x), pc))
tmapValuesColorize_fill = function(x, pc) do.call(process_color, c(list(col = x), pc))
tmapValuesColorize_bgcol = function(x, pc) do.call(process_color, c(list(col = x), pc))
tmapValuesColorize_size = function(x, pc) x
tmapValuesColorize_lwd = function(x, pc) x
tmapValuesColorize_lty = function(x, pc) x
tmapValuesColorize_shape = function(x, pc) x
tmapValuesColorize_col_alpha = function(x, pc) x
tmapValuesColorize_fill_alpha = function(x, pc) x
tmapValuesColorize_bgcol_alpha = function(x, pc) x
tmapValuesColorize_text = function(x, pc) x
tmapValuesColorize_fontface = function(x, pc) x
tmapValuesColorize_xmod = tmapValuesColorize_ymod = function(x, pc) x



tmap_seq = function(from = 0, to = 1, power = c("lin", "sqrt", "sqrt_perceptual", "quadratic")) {
	structure(as.list(environment()), class = "tmapSeq")
}

tmapSeq = function(s, n, rescale = TRUE) {
	with(s, {
		p = if (is.numeric(power)) power else switch(power, lin = 1, sqrt = 0.5, sqrt_perceptual = 0.5716, quadratic = 2)
		#if (is.null(p)) p = as.numeric()
		r = seq(from = from, to = to, length.out = n) ^ p
		if (rescale) {
			(r - (r[1])) / (r[n] - r[1]) * (to - from) + from	
		} else {
			r
		}
	})
}





tmapValuesCVV_fill = function(x, value.na, n, range, scale, rep, o) {
	
	
	# process values
	#palid = tmapPalId(x[1])
	
	arecolors = valid_colors(x[1])
	m = getPalMeta(x[1])
	
	ispalette = !is.null(m) && !arecolors # the latter in case of ambiguity (e.g. "blue")
	
	values = if (!ispalette && !arecolors) {
		rep(x, length.out = n) 
	} else if (ispalette) {
		getPal(x, n, rep = rep, range = range)
	} else if (!rep && (length(x) < n)) {
		grDevices::colorRampPalette(x)(n)
	} else {
		rep(x, length.out=n)
	}
	
	nms = names(values)
	values = do.call(process_color, c(list(col = values), o$pc))
	names(values) = nms
	value.neutral = do.call(process_color, c(list(col = values[1]), o$pc))
	value.na = do.call(process_color, c(list(col = value.na), o$pc))
	
	
	list(vvalues = values, value.neutral = value.neutral, value.na = value.na)
	
	# } else if (arenumbers) {
	# 	values = if (length(scale$values) == 2) seq(scale$values[1], scale$values[2], length.out = n) else rep(scale$values, length.out = n)
	# } else {
	# 	values = rep(scale$values, length.out = n)
	# }
	
}


# categorical 
tmapValuesCVV_col = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesCVV_fill(x, value.na, n, range, scale, rep, o)
}

tmapValuesCVV_bgcol = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesCVV_fill(x, value.na, n, range, scale, rep, o)
}

tmapValuesCVV_size = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_size(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

tmapValuesCVV_lwd = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_lwd(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

tmapValuesCVV_col_alpha = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_col_alpha(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

tmapValuesCVV_fill_alpha = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_fill_alpha(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

tmapValuesCVV_bgcol_alpha = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_bgcol_alpha(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}
tmapValuesCVV_area = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_area(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

tmapValuesCVV_xmod = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_xmod(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

tmapValuesCVV_ymod = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_ymod(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

tmapValuesCVV_shape = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_shape(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

tmapValuesCVV_lty = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_lty(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

tmapValuesCVV_text = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_text(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

tmapValuesCVV_fontface = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_fontface(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

# bivariate visual variables 
tmapValuesBVV_fill = function(x, value.na, m, n, scale, rep, o) {
	#palid = tmapPalId(x[1])
	
	meta = getPalMeta(x[1])
	
	if (!is.null(meta)) {
		values = getPalBiv(x, m = m, n = n)
	} else if (!is.matrix(x)) {
		values = matrix(grDevices::colorRampPalette(x)(m*n), nrow = m)
	} else if (is.matrix(x)) {
		if (ncol(x) != n) {
			x = do.call(rbind, apply(x, function(xi) {
				grDevices::colorRampPalette(xi)(n)
			}, MARGIN = 1, simplify = FALSE))
		}
		
		if (nrow(x) != m) {
			x = do.call(cbind, apply(x, function(xi) {
				grDevices::colorRampPalette(xi)(m)
			}, MARGIN = 2, simplify = FALSE))
		}
		values = x
	}
	
	values[] = do.call(process_color, c(list(col = values), o$pc))
	value.neutral = do.call(process_color, c(list(col = values[1]), o$pc))
	value.na = do.call(process_color, c(list(col = value.na), o$pc))
	
	
	list(vvalues = values, value.neutral = value.neutral, value.na = value.na)
	
}

tmapValuesBVV_col = function(x, value.na, m, n, scale, rep, o) {
	tmapValuesBVV_fill(x, value.na, m, n, scale, rep, o)
}

tmapValuesBVV_bgcol = function(x, value.na, m, n, scale, rep, o) {
	tmapValuesBVV_fill(x, value.na, m, n, scale, rep, o)
}
