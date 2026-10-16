# Map variables for fill patterns:
# - pattern: the pattern type (to do: full set of value functions, see sandbox/patterns_plan.md step 2)
# - pattern_col: ink color (reuses the col functions)
# - pattern_size: line width or dot size (reuses the lwd functions)
# - pattern_density: density of the pattern elements, 0-1 (reuses the fill_alpha functions)

# pattern

#' @export
#' @rdname tmap_internal
tmapValuesCheck_pattern = function(x, is_var = TRUE) {
	if (is.character(x) && all(x %in% c("none", "hatch", "crosshatch", "dots", "noise"))) {
		TRUE
	} else {
		structure(FALSE, info = {if (is_var) " Variable should be a data variable name or a pattern type (\"none\", \"hatch\", \"crosshatch\", \"dots\", or \"noise\")." else "  Values should be pattern types (\"none\", \"hatch\", \"crosshatch\", \"dots\", or \"noise\")."})
	}
}

#' @export
#' @rdname tmap_internal
tmapValuesScale_pattern = function(x, scale) x

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
