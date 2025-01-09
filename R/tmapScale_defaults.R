#' @export
#' @rdname tmap_internal
tmapValuesCheck_col = function(x, is_var = TRUE) {
	isnum = is.numeric(x)
	if (isnum) {
		structure(FALSE,
				  info = {if (is_var) "Variable should be data varible name or color name" else " Values should be numeric (between -50 and 50)."}
		)
	} else {
		is_c4a = !is.null(getPalMeta(x[1])) && length(x) == 1L && !valid_colors(x[1])
		if (is_c4a) {
			if (is_var) {
				structure(FALSE,
						  info = " Variable should be a data variable name or a single color (not a color palette).")
			} else {
				TRUE
			}
		} else {
			all_cols = all(valid_colors(x))
			if (!all_cols) {
				structure(FALSE,
						  info = if (is_var) " Variable should a data variable name or a single color." else " Values should be color names or a color palette (run  cols4all::c4a_palettes() for available ones.")
			} else {
				TRUE
			}
		}
	}
}

#' @export
#' @rdname tmap_internal
tmapValuesCheck_fill = function(x, is_var = TRUE) {
	tmapValuesCheck_col(x, is_var)
}

#' @export
#' @rdname tmap_internal
tmapValuesCheck_bgcol = function(x, is_var = TRUE) {
	tmapValuesCheck_col(x, is_var)
}

isSymbol = function(s) {
	inherits(s, "grob") || any(vapply(s, inherits, FUN.VALUE = logical(1), "grob")) || ("iconUrl" %in% names(s))
}
isUrl <- function(string) {
	any(grepl("(https?|ftp)://[^\\s/$.?#].[^\\s]*", string))
}

isLocal = function(string) {
	file.exists(string)
}

#' @export
#' @rdname tmap_internal
tmapValuesCheck_shape = function(x, is_var = TRUE) {

	if (inherits(x, "tmap_icons")) {
		TRUE
	} else if (all(is.numeric(x))) {
		TRUE
	} else if (is.list(x)) {
	 	if (isSymbol(x)) {
	 		TRUE
	 	} else {
	 		all(vapply(x, isSymbol, FUN.VALUE = logical(1)) | vapply(x, is.numeric, FUN.VALUE = logical(1)))
	 	}
	} else if (all(is.character(x))) {
		if (all(isUrl(x) | isLocal(x))) {
			TRUE
		} else {
			structure(FALSE, info = {if (is_var) " Variable should be a data variable name or a symbol (see {.help [tm_symbols](tmap::tm_symbols)} - details section)." else "  Values should be symbols (see {.help [tm_symbols](tmap::tm_symbols)} - details section)."})
		}
	}
}



#' @export
#' @rdname tmap_internal
tmapValuesCheck_size = function(x, is_var = TRUE) {
	res = inherits(x, "tmapSeq") || (is.numeric(x) && (all(x>=0) || all(x<=0)))

	if (!res) {
		structure(FALSE, info = {if (is_var) " Variable should be a data variable name or a numeric value." else "  Values should be numeric."})
	} else {
		TRUE
	}
}


#' @export
#' @rdname tmap_internal
tmapValuesCheck_area = function(x, is_var = TRUE) {
	tmapValuesCheck_size(x, is_var)
}

#' @export
#' @rdname tmap_internal
tmapValuesCheck_lwd = function(x, is_var = TRUE) {
	tmapValuesCheck_size(x, is_var)
}

#' @export
#' @rdname tmap_internal
tmapValuesCheck_lty = function(x, is_var = TRUE) {
	if (all(x %in% c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")) ||
		(is.numeric(x) && all(x %in% 0:6)) ||
		(all(grepl("^[1-9A-F]{1,8}$", x)))) {
		TRUE
	} else {
		structure(FALSE, info = {if (is_var) " Variable should be a data variable name or a line type (see documentation of graphics::par - lty)." else "  Values should be a line type (see documentation of graphics::par - lty). numeric."})
	}
}

#' @export
#' @rdname tmap_internal
tmapValuesCheck_xmod = tmapValuesCheck_ymod = function(x, is_var = TRUE) {
	if (!is.numeric(x)) {
		structure(FALSE,
				  info = {if (is_var) " It is neither a data variable name nor a numeric (between -50 and 50)." else " Values should be numeric (between -50 and 50)."}
				  )
	} else {
		res = all(x >= -50 & x <= 50)
		if (!res) {
			structure(FALSE,
					  info = {if (is_var) " Variable should be in the [-50,50] range." else " Values found that are outside the [-50,50] range. Note that the default scale for xmod and ymod is tm_scale_asis."})
		} else {
			TRUE
		}
	}
}

#' @export
#' @rdname tmap_internal
tmapValuesCheck_angle = function(x, is_var = TRUE) {
	# to do
	if (!is.numeric(x)) {
		structure(FALSE, info = {if (is_var) " Variable should be a data variable name or a numeric value." else "  Values should be numeric."})
	} else {
		TRUE
	}
}

#' @export
#' @rdname tmap_internal
tmapValuesCheck_col_alpha= function(x, is_var = TRUE) {
	tmapValuesCheck_size(x, is_var)
}

#' @export
#' @rdname tmap_internal
tmapValuesCheck_fill_alpha = function(x, is_var = TRUE) {
	tmapValuesCheck_size(x, is_var)
}

#' @export
#' @rdname tmap_internal
tmapValuesCheck_bgcol_alpha= function(x, is_var = TRUE) {
	tmapValuesCheck_size(x, is_var)
}

#' @export
#' @rdname tmap_internal
tmapValuesCheck_area = function(x, is_var = TRUE) {
	tmapValuesCheck_size(x, is_var)
}

#' @export
#' @rdname tmap_internal
tmapValuesCheck_text = function(x, is_var = TRUE) {
	TRUE
}

#' @export
#' @rdname tmap_internal
tmapValuesCheck_fontface = function(x, is_var = TRUE) {

	if 	((is.numeric(x) && (all(x %in% 1:5))) || (is.character(x) && (all(x %in% c("plain", "bold", "italic", "oblique", "bold.italic", "cyrillic", "cyrillic.oblique", "EUC"))))) {
		TRUE
	} else {
		structure(FALSE, info = {if (is_var) " Variable should be a data variable name or a font face (see documentation of graphics::par font)." else "  Values should be a font face (see documentation of graphics::par - font). numeric."})
	}
}

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_fill = function(x) {
	m = getPalMeta(x[1])
	ispal = !is.null(m)

	if (ispal) {
		m$type == "div"
	} else {
		(palette_type(x) == "div")
	}
}

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_col = function(x) {
	tmapValuesIsDiv_fill(x)
}

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_bgcol = function(x) {
	tmapValuesIsDiv_fill(x)
}

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_size = function(x) {
	inherits(x, "tmapSeq") && (x$from < 0) && (x$to > 1) || (is.numeric(x) && (any(x < 0) && any(x> 0)))
}

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_area = function(x) {
	tmapValuesIsDiv_size(x)
}

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_lwd = function(x) {
	tmapValuesIsDiv_size(x)
}

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_lty = function(x) {
	FALSE
}

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_col_alpha = function(x) {
	tmapValuesIsDiv_size(x)
}

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_fill_alpha = function(x) {
	tmapValuesIsDiv_size(x)
}

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_bgcol_alpha = function(x) {
	tmapValuesIsDiv_size(x)
}

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_area = function(x) {
	tmapValuesIsDiv_size(x)
}

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_xmod = tmapValuesIsDiv_ymod = function(x) {
	tmapValuesIsDiv_size(x)
}

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_angle = function(x) {
	FALSE
}

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_shape = function(x) {
	FALSE
}

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_text = function(x) {
	FALSE
}

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_fontface = function(x) {
	FALSE
}


#' @export
#' @rdname tmap_internal
tmapValuesRange_fill = function(x, n, isdiv) {
	m = getPalMeta(x[1])

	if (!is.null(m)) {
		NA # in c4a palette definition
	} else c(0, 1)
}

#' @export
#' @rdname tmap_internal
tmapValuesRange_col = function(x, n, isdiv) {
	tmapValuesRange_fill(x, n, isdiv)
}

#' @export
#' @rdname tmap_internal
tmapValuesRange_bgcol = function(x, n, isdiv) {
	tmapValuesRange_fill(x, n, isdiv)
}

#' @export
#' @rdname tmap_internal
tmapValuesRange_shape = function(x, n, isdiv) {
	c(0, 1)
}

#' @export
#' @rdname tmap_internal
tmapValuesRange_lty = function(x, n, isdiv) {
	c(0, 1)
}


#' @export
#' @rdname tmap_internal
tmapValuesRange_size = function(x, n, isdiv) {
	#print(c(.5/n, 1 - .5/n))
#	c(.5/n, 1 - .5/n)
	c(0, 1)
}

#' @export
#' @rdname tmap_internal
tmapValuesRange_area = function(x, n, isdiv) {
	tmapValuesRange_size(x, n, isdiv)
}

#' @export
#' @rdname tmap_internal
tmapValuesRange_lwd = function(x, n, isdiv) {
	tmapValuesRange_size(x, n, isdiv)
}

#' @export
#' @rdname tmap_internal
tmapValuesRange_col_alpha = function(x, n, isdiv) {
	tmapValuesRange_size(x, n, isdiv)
}

#' @export
#' @rdname tmap_internal
tmapValuesRange_fill_alpha = function(x, n, isdiv) {
	tmapValuesRange_size(x, n, isdiv)
}

#' @export
#' @rdname tmap_internal
tmapValuesRange_bgcol_alpha = function(x, n, isdiv) {
	tmapValuesRange_size(x, n, isdiv)
}

#' @export
#' @rdname tmap_internal
tmapValuesRange_area = function(x, n, isdiv) {
	tmapValuesRange_size(x, n, isdiv)
}

#' @export
#' @rdname tmap_internal
tmapValuesRange_xmod = tmapValuesRange_ymod = function(x, n, isdiv) {
	tmapValuesRange_size(x, n, isdiv)
}

#' @export
#' @rdname tmap_internal
tmapValuesRange_angle = function(x, n, isdiv) {
	c(0, 1)
}

#' @export
#' @rdname tmap_internal
tmapValuesRange_text = function(x, n, isdiv) {
	c(0, 1)
}

#' @export
#' @rdname tmap_internal
tmapValuesRange_fontface = function(x, n, isdiv) {
	c(0, 1)
}

#' @export
#' @rdname tmap_internal
tmapValuesVV_fill = function(x, value.na, isdiv, n, dvalues, are_breaks, midpoint, range, scale, rep, o, aes = "fill") {

	#palid = tmapPalId(x[1])
	if (x[1] %in% c("seq", "div", "unord", "ord", "biv")) {
		# cols4all can also take "div", but does not take into account tmap style
		x = getAesOption("values.var", o, aes = aes, layer = NA, cls = x[1])
	}

	m = if (length(x) > 1) NULL else getPalMeta(x[1])


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
		if (x[1] != tolower(x[1])) message_c4a(x[1], info = m)
		vvalues = getPal(m$fullname, n = ntot, range = range, reversed = m$reverse)[ids]
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
			vvalues = grDevices::colorRampPalette(x)(o$continuous.nclasses)[ids_after_range]
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

#' @export
#' @rdname tmap_internal
tmapValuesVV_col = function(...) {
	tmapValuesVV_fill(..., aes = "col")
}

#' @export
#' @rdname tmap_internal
tmapValuesVV_bgcol = function(...) {
	tmapValuesVV_fill(..., aes = "bgcol")
}

#' @export
#' @rdname tmap_internal
tmapValuesVV_shape = function(x, value.na, isdiv, n, dvalues, are_breaks, midpoint, range, scale, rep, o) {
	if (inherits(x, "tmap_icons") && "iconUrl" %in% names(x)) x = split_icon(x)
	list(vvalues = rep(x, length.out = n), value.neutral = x[1], value.na = value.na)
}

#' @export
#' @rdname tmap_internal
tmapValuesVV_lty = function(x, value.na, isdiv, n, dvalues, are_breaks, midpoint, range, scale, rep, o) {
	list(vvalues = rep(x, length.out = n), value.neutral = x[1], value.na = value.na)
}


#' @export
#' @rdname tmap_internal
tmapValuesVV_size = function(x, value.na, isdiv, n, dvalues, are_breaks, midpoint, range, scale, rep, o) {
	vvalues = if (is.numeric(x) && length(x) == n) {
		if (range[1] !=0 || range[2] != 1) {
			warning("values.range not used because the individual values have been specified (instead of a sequence)", call. = FALSE)
		}
		x
	} else {
		if (is.numeric(x)) {
			x = tm_seq(x[1], x[length(x)], power = "lin")
		}

		if (range[1] !=0 || range[2] != 1) {
			p = if (is.numeric(x$power)) x$power else switch(x$power, lin = 1, sqrt = 0.5, sqrt_perceptual = 0.5716, quadratic = 2)
			x$from = range[1] ^ (1/p)
			x$to = range[2] ^ (1/p)
		}
		tmapSeq(x, n)

	}
	value.neutral = vvalues[round((n+1)/2)]
	list(vvalues = vvalues * scale, value.neutral = value.neutral * scale, value.na = value.na * scale)
}



#' @export
#' @rdname tmap_internal
tmapValuesVV_area = function(...) {
	do.call(tmapValuesVV_size, args = list(...))
}

#' @export
#' @rdname tmap_internal
tmapValuesVV_lwd = function(...) {
	do.call(tmapValuesVV_size, args = list(...))
}

#' @export
#' @rdname tmap_internal
tmapValuesVV_col_alpha = function(...) {
	do.call(tmapValuesVV_size, args = list(...))
}

#' @export
#' @rdname tmap_internal
tmapValuesVV_fill_alpha = function(...) {
	do.call(tmapValuesVV_size, args = list(...))
}

#' @export
#' @rdname tmap_internal
tmapValuesVV_bgcol_alpha = function(...) {
	do.call(tmapValuesVV_size, args = list(...))
}

#' @export
#' @rdname tmap_internal
tmapValuesVV_area = function(...) {
	do.call(tmapValuesVV_size, args = list(...))
}

#' @export
#' @rdname tmap_internal
tmapValuesVV_xmod = tmapValuesVV_ymod = function(...) {
	do.call(tmapValuesVV_size, args = list(...))
}


#' @export
#' @rdname tmap_internal
tmapValuesVV_angle = function(...) {
	do.call(tmapValuesVV_size, args = list(...))
}

#' @export
#' @rdname tmap_internal
tmapValuesVV_text = function(x, value.na, isdiv, n, dvalues, are_breaks, midpoint, range, scale, rep, o) {
	list(vvalues = rep(x, length.out = n), value.neutral = x[1], value.na = value.na)
}

#' @export
#' @rdname tmap_internal
tmapValuesVV_fontface = function(x, value.na, isdiv, n, dvalues, are_breaks, midpoint, range, scale, rep, o) {
	list(vvalues = rep(x, length.out = n), value.neutral = x[1], value.na = value.na)
}

# for symbols, which need to be 'submitted' (content replaced by an integer)
#' @export
#' @rdname tmap_internal
tmapValuesSubmit_col = function(x, args) x
#' @export
#' @rdname tmap_internal
tmapValuesSubmit_fill = function(x, args) x
#' @export
#' @rdname tmap_internal
tmapValuesSubmit_bgcol = function(x, args) x
#' @export
#' @rdname tmap_internal
tmapValuesSubmit_size = function(x, args) x
#' @export
#' @rdname tmap_internal
tmapValuesSubmit_area = function(x, args) x
#' @export
#' @rdname tmap_internal
tmapValuesSubmit_xmod = function(x, args) x
#' @export
#' @rdname tmap_internal
tmapValuesSubmit_ymod = function(x, args) x
#' @export
#' @rdname tmap_internal
tmapValuesSubmit_angle = function(x, args) x
#' @export
#' @rdname tmap_internal
tmapValuesSubmit_lwd = function(x, args) x
#' @export
#' @rdname tmap_internal
tmapValuesSubmit_lty = function(x, args) x
#' @export
#' @rdname tmap_internal
tmapValuesSubmit_shape = function(x, args) {
	if (all(is.na(x))) return(x)
	if (!inherits(x, c("tmapStandard", "tmapSpecial"))) x = tmapVV(x)
	if (inherits(x, "tmapStandard")) {
		xvec = unlist(x, use.names = FALSE)
		if (is.character(xvec) && all(isUrl(xvec) | isLocal(xvec))) {
			x = tmapVV(tmap_icons(xvec, merge = FALSE))
		}
	}

	if (inherits(x, "tmapSpecial")) {
		isGrid = (tmap_graphics_name() == "Grid")


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

		do.call(submit_symbols, args = list(x = x, grid = isGrid, args = args))
	} else {
		unlist(x)
	}
}

#' @export
#' @rdname tmap_internal
tmapValuesSubmit_col_alpha = function(x, args) x
#' @export
#' @rdname tmap_internal
tmapValuesSubmit_fill_alpha = function(x, args) x
#' @export
#' @rdname tmap_internal
tmapValuesSubmit_bgcol_alpha = function(x, args) x
#' @export
#' @rdname tmap_internal
tmapValuesSubmit_text = function(x, args) x
#' @export
#' @rdname tmap_internal
tmapValuesSubmit_fontface = function(x, args) x


#' @export
#' @rdname tmap_internal
tmapValuesScale_col = function(x, scale) x
#' @export
#' @rdname tmap_internal
tmapValuesScale_fill = function(x, scale) x
#' @export
#' @rdname tmap_internal
tmapValuesScale_bgcol = function(x, scale) x
#' @export
#' @rdname tmap_internal
tmapValuesScale_size = function(x, scale) x * scale
#' @export
#' @rdname tmap_internal
tmapValuesScale_area = function(x, scale) x
#' @export
#' @rdname tmap_internal
tmapValuesScale_lwd = function(x, scale) x * scale
#' @export
#' @rdname tmap_internal
tmapValuesScale_lty = function(x, scale) x
#' @export
#' @rdname tmap_internal
tmapValuesScale_shape = function(x, scale) x
#' @export
#' @rdname tmap_internal
tmapValuesScale_col_alpha = function(x, scale) x
#' @export
#' @rdname tmap_internal
tmapValuesScale_fill_alpha = function(x, scale) x
#' @export
#' @rdname tmap_internal
tmapValuesScale_bgcol_alpha = function(x, scale) x
#' @export
#' @rdname tmap_internal
tmapValuesScale_text = function(x, scale) x
#' @export
#' @rdname tmap_internal
tmapValuesScale_fontface = function(x, scale) x
#' @export
#' @rdname tmap_internal
tmapValuesScale_xmod = tmapValuesScale_ymod = function(x, scale) x
#' @export
#' @rdname tmap_internal
tmapValuesScale_angle = function(x, scale) x

#' @export
#' @rdname tmap_internal
tmapValuesColorize_col = function(x, pc) do.call(process_color, c(list(col = x), pc))
#' @export
#' @rdname tmap_internal
tmapValuesColorize_fill = function(x, pc) do.call(process_color, c(list(col = x), pc))
#' @export
#' @rdname tmap_internal
tmapValuesColorize_bgcol = function(x, pc) do.call(process_color, c(list(col = x), pc))
#' @export
#' @rdname tmap_internal
tmapValuesColorize_size = function(x, pc) x
#' @export
#' @rdname tmap_internal
tmapValuesColorize_area = function(x, pc) x
#' @export
#' @rdname tmap_internal
tmapValuesColorize_lwd = function(x, pc) x
#' @export
#' @rdname tmap_internal
tmapValuesColorize_lty = function(x, pc) x
#' @export
#' @rdname tmap_internal
tmapValuesColorize_shape = function(x, pc) x
#' @export
#' @rdname tmap_internal
tmapValuesColorize_col_alpha = function(x, pc) x
#' @export
#' @rdname tmap_internal
tmapValuesColorize_fill_alpha = function(x, pc) x
#' @export
#' @rdname tmap_internal
tmapValuesColorize_bgcol_alpha = function(x, pc) x
#' @export
#' @rdname tmap_internal
tmapValuesColorize_text = function(x, pc) x
#' @export
#' @rdname tmap_internal
tmapValuesColorize_fontface = function(x, pc) x
#' @export
#' @rdname tmap_internal
tmapValuesColorize_xmod = tmapValuesColorize_ymod = function(x, pc) x
#' @export
#' @rdname tmap_internal
tmapValuesColorize_angle = function(x, pc) x


#' Specify a numeric sequence
#'
#' Specify a numeric sequence, for numeric scales like [tm_scale_continuous()]. This function is needed when there is a non-linear relationship between the numeric data values and the visual variables. E.g. to make relationship with the area of bubbles linear, the square root of input variables should be used to calculate the radius of the bubbles.
#'
#' The perceived area of larger symbols is often underestimated. Flannery (1971) experimentally derived a method to compensate this for symbols. This compensation is obtained by using the power exponent of 0.5716 instead of 0.5, or by setting `power` to `"sqrt_perceptual"`
#'
#' @param from,to The numeric range, default 0 and 1 respectively
#' @param power The power component, or one of `"lin"`, `"sqrt"`, `"sqrt_perceptual"`, `"quadratic"`, which correspond to 1, 0.5, 0.5716, 2 respectively. See details.
#' @export
tm_seq = function(from = 0, to = 1, power = c("lin", "sqrt", "sqrt_perceptual", "quadratic")) {
	structure(as.list(environment()), class = "tmapSeq")
}

# x is vector, rng is its range
norm_vector = function(x, rng) {
	(x - rng[1]) / diff(rng)
}

scale_vector = function(x, new_rng) {
	(x + new_rng[1]) * diff(new_rng)
}

#' @export
#' @rdname tmap_internal
tmapSeq = function(s, n = NULL) {
	if (is.null(n) && is.null(s$values)) stop("One of n or s$values should be provided")
	if (is.null(s$values)) s["values"] = list(NULL)
	with(s, {
		p = if (is.numeric(power)) power else switch(power, lin = 1, sqrt = 0.5, sqrt_perceptual = 0.5716, quadratic = 2)
		r = seq(from = from, to = to, length.out = n) ^ p
	})
}

#' @export
#' @rdname tmap_internal
transform_values = function(x, lim, rng, power, scale, include.neutral = TRUE) {
	p = if (is.numeric(power)) power else switch(power, lin = 1, sqrt = 0.5, sqrt_perceptual = 0.5716, quadratic = 2)
	if (p != 1) rng = rng ^ (1/p)

	x2 = norm_vector(x, lim)
	x3 = if (rng[1] != 0 || rng[2] != 1) scale_vector(x2, rng) else x2

	if (include.neutral) neutral = mean(rng)
	if (p != 1) {
		x3 = x3 ^ p
		if (include.neutral) neutral = neutral ^ p
	}
	x4 = x3 * scale
	if (include.neutral) neutral = neutral * scale

	if (include.neutral) {
		list(x = x4,
			 neutral = neutral)
	} else {
		x4
	}
}


#' @export
#' @rdname tmap_internal
tmapValuesCVV_fill = function(x, value.na, n, range, scale, rep, o) {


	# process values
	#palid = tmapPalId(x[1])

	arecolors = valid_colors(x[1])
	m = getPalMeta(x[1])

	ispalette = !is.null(m) && !arecolors # the latter in case of ambiguity (e.g. "blue")

	if (ispalette) if (x[1] != tolower(x[1])) message_c4a(x[1], info = m)


	values = if (!ispalette && !arecolors) {
		rep(x, length.out = n)
	} else if (ispalette) {
		getPal(m$fullname, n, rep = rep, range = range, reversed = m$reverse)
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


#' @export
#' @rdname tmap_internal
tmapValuesCVV_col = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesCVV_fill(x, value.na, n, range, scale, rep, o)
}

#' @export
#' @rdname tmap_internal
tmapValuesCVV_bgcol = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesCVV_fill(x, value.na, n, range, scale, rep, o)
}

#' @export
#' @rdname tmap_internal
tmapValuesCVV_size = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_size(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

#' @export
#' @rdname tmap_internal
tmapValuesCVV_area = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_area(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

#' @export
#' @rdname tmap_internal
tmapValuesCVV_lwd = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_lwd(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

#' @export
#' @rdname tmap_internal
tmapValuesCVV_col_alpha = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_col_alpha(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

#' @export
#' @rdname tmap_internal
tmapValuesCVV_fill_alpha = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_fill_alpha(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

#' @export
#' @rdname tmap_internal
tmapValuesCVV_bgcol_alpha = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_bgcol_alpha(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

#' @export
#' @rdname tmap_internal
tmapValuesCVV_area = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_area(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

#' @export
#' @rdname tmap_internal
tmapValuesCVV_xmod = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_xmod(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

#' @export
#' @rdname tmap_internal
tmapValuesCVV_ymod = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_ymod(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

#' @export
#' @rdname tmap_internal
tmapValuesCVV_angle = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_angle(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

#' @export
#' @rdname tmap_internal
tmapValuesCVV_shape = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_shape(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

#' @export
#' @rdname tmap_internal
tmapValuesCVV_lty = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_lty(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

#' @export
#' @rdname tmap_internal
tmapValuesCVV_text = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_text(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

#' @export
#' @rdname tmap_internal
tmapValuesCVV_fontface = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_fontface(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

# bivariate visual variables
#' @export
#' @rdname tmap_internal
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

#' @export
#' @rdname tmap_internal
tmapValuesBVV_col = function(x, value.na, m, n, scale, rep, o) {
	tmapValuesBVV_fill(x, value.na, m, n, scale, rep, o)
}

#' @export
#' @rdname tmap_internal
tmapValuesBVV_bgcol = function(x, value.na, m, n, scale, rep, o) {
	tmapValuesBVV_fill(x, value.na, m, n, scale, rep, o)
}


#' @export
#' @rdname tmap_internal
tmapValuesCheck_num = tmapValuesCheck_size
#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_num = tmapValuesIsDiv_size
#' @export
#' @rdname tmap_internal
tmapValuesRange_num = tmapValuesRange_size
#' @export
#' @rdname tmap_internal
tmapValuesVV_num = tmapValuesVV_size
#' @export
#' @rdname tmap_internal
tmapValuesSubmit_num = tmapValuesSubmit_size
#' @export
#' @rdname tmap_internal
tmapValuesScale_num = tmapValuesScale_size
#' @export
#' @rdname tmap_internal
tmapValuesColorize_num = tmapValuesColorize_size
#' @export
#' @rdname tmap_internal
tmapValuesCVV_num = tmapValuesCVV_size

#' @export
#' @rdname tmap_internal
tmapValuesCheck_skip = function(x, is_var) TRUE
#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_skip = function(x) FALSE
#' @export
#' @rdname tmap_internal
tmapValuesRange_skip = function(x, n, isdiv) c(0, 1)
#' @export
#' @rdname tmap_internal
tmapValuesVV_skip = function(x, value.na, isdiv, n, dvalues, are_breaks, midpoint, range, scale, rep, o) {
	list(vvalues = rep(x, length.out = n), value.neutral = x[1], value.na = value.na)
}
#' @export
#' @rdname tmap_internal
tmapValuesSubmit_skip = function(x, args) x
#' @export
#' @rdname tmap_internal
tmapValuesScale_skip = function(x, scale) x
#' @export
#' @rdname tmap_internal
tmapValuesColorize_skip = function(x, pc) x
#' @export
#' @rdname tmap_internal
tmapValuesCVV_skip = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_shape(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}
