# Map variables for fill patterns:
# - pattern: the pattern motif (see tm_patterns())
# - pattern_col: ink color (reuses the col functions)
# - pattern_size: line width or dot size (reuses the lwd functions)
# - pattern_density: density of the pattern elements, 0-1 (reuses the fill_alpha functions)

# pattern

#' @export
#' @rdname tmap_internal
tmapValuesCheck_pattern = function(x, is_var = TRUE) {
	if (pattern_valid(x)) {
		TRUE
	} else {
		structure(FALSE, info = {if (is_var) " Variable should be a data variable name or a pattern (see {.fun tm_patterns})." else "  Values should be patterns: pattern types or {.fun tm_patterns} specifications."})
	}
}

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_pattern = function(x) {
	FALSE
}

#' @export
#' @rdname tmap_internal
tmapValuesRange_pattern = function(x, n, isdiv) {
	c(0, 1)
}

#' @export
#' @rdname tmap_internal
tmapValuesVV_pattern = function(x, value.na, isdiv, n, dvalues, are_breaks, midpoint, range, scale, rep, o) {
	x = pattern_norm(x)
	if (length(x) < n && length(x) > 1) {
		cli::cli_warn("{.field [pattern]} the number of patterns ({length(x)}) is smaller than the number of classes ({n}), so patterns are recycled")
	}
	list(vvalues = rep(unname(x), length.out = n), value.neutral = unname(x[1]), value.na = pattern_norm(value.na))
}

#' @export
#' @rdname tmap_internal
tmapValuesCVV_pattern = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_pattern(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

#' @export
#' @rdname tmap_internal
tmapValuesSubmit_pattern = function(x, args) x

#' @export
#' @rdname tmap_internal
tmapValuesScale_pattern = function(x, scale) pattern_norm(x)

#' @export
#' @rdname tmap_internal
tmapValuesColorize_pattern = function(x, pc) x

# pattern_col

#' @export
#' @rdname tmap_internal
tmapValuesCheck_pattern_col = function(x, is_var = TRUE) tmapValuesCheck_col(x, is_var = is_var)

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_pattern_col = function(x) tmapValuesIsDiv_col(x)

#' @export
#' @rdname tmap_internal
tmapValuesRange_pattern_col = function(x, n, isdiv) tmapValuesRange_col(x, n, isdiv)

#' @export
#' @rdname tmap_internal
tmapValuesVV_pattern_col = function(...) {
	tmapValuesVV_fill(..., aes = "pattern_col")
}

#' @export
#' @rdname tmap_internal
tmapValuesCVV_pattern_col = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesCVV_fill(x, value.na, n, range, scale, rep, o)
}

#' @export
#' @rdname tmap_internal
tmapValuesSubmit_pattern_col = function(x, args) x

#' @export
#' @rdname tmap_internal
tmapValuesScale_pattern_col = function(x, scale) x

#' @export
#' @rdname tmap_internal
tmapValuesColorize_pattern_col = function(x, pc) do.call(process_color, c(list(col = x), pc))

# pattern_size

#' @export
#' @rdname tmap_internal
tmapValuesCheck_pattern_size = function(x, is_var = TRUE) tmapValuesCheck_lwd(x, is_var = is_var)

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_pattern_size = function(x) tmapValuesIsDiv_lwd(x)

#' @export
#' @rdname tmap_internal
tmapValuesRange_pattern_size = function(x, n, isdiv) tmapValuesRange_lwd(x, n, isdiv)

#' @export
#' @rdname tmap_internal
tmapValuesVV_pattern_size = function(...) {
	do.call(tmapValuesVV_size, args = list(...))
}

#' @export
#' @rdname tmap_internal
tmapValuesCVV_pattern_size = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_pattern_size(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

#' @export
#' @rdname tmap_internal
tmapValuesSubmit_pattern_size = function(x, args) x

#' @export
#' @rdname tmap_internal
tmapValuesScale_pattern_size = function(x, scale) x * scale

#' @export
#' @rdname tmap_internal
tmapValuesColorize_pattern_size = function(x, pc) x

# pattern_density

#' @export
#' @rdname tmap_internal
tmapValuesCheck_pattern_density = function(x, is_var = TRUE) tmapValuesCheck_fill_alpha(x, is_var = is_var)

#' @export
#' @rdname tmap_internal
tmapValuesIsDiv_pattern_density = function(x) tmapValuesIsDiv_fill_alpha(x)

#' @export
#' @rdname tmap_internal
tmapValuesRange_pattern_density = function(x, n, isdiv) tmapValuesRange_fill_alpha(x, n, isdiv)

#' @export
#' @rdname tmap_internal
tmapValuesVV_pattern_density = function(...) {
	do.call(tmapValuesVV_size, args = list(...))
}

#' @export
#' @rdname tmap_internal
tmapValuesCVV_pattern_density = function(x, value.na, n, range, scale, rep, o) {
	tmapValuesVV_pattern_density(x = x, value.na = value.na, isdiv = FALSE, n = n, dvalues = NA, are_breaks = FALSE, midpoint = NA, range = range, scale = scale, rep = rep)
}

#' @export
#' @rdname tmap_internal
tmapValuesSubmit_pattern_density = function(x, args) x

#' @export
#' @rdname tmap_internal
tmapValuesScale_pattern_density = function(x, scale) x

#' @export
#' @rdname tmap_internal
tmapValuesColorize_pattern_density = function(x, pc) x
