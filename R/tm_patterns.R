# Pattern types and their parameters (with defaults). Parameters are relative to the
# spacing between pattern elements (which is set by the map variable pattern_density),
# except angle (degrees, counter-clockwise from horizontal) and random (0-1).
.pattern_types = list(
	none = list(),
	hatch = list(angle = 45),
	crosshatch = list(angle = 0),
	dots = list(random = 0),
	dashes = list(angle = 0, length = 0.5),
	bricks = list(angle = 0, length = 2),
	herringbone = list(angle = 0, length = 2),
	hexagons = list(angle = 0),
	waves = list(angle = 0, amplitude = 0.25),
	zigzag = list(angle = 0, amplitude = 0.25),
	circles = list(random = 0),
	crosses = list(random = 0),
	noise = list())

# Shortcuts: pattern types with specific parameter values
.pattern_shortcuts = list(
	hatch_up = list(type = "hatch", angle = 45),
	hatch_down = list(type = "hatch", angle = 135),
	hatch_horizontal = list(type = "hatch", angle = 0),
	hatch_vertical = list(type = "hatch", angle = 90),
	crosshatch_diagonal = list(type = "crosshatch", angle = 45),
	stipple = list(type = "dots", random = 0.7))

#' Specify fill patterns
#'
#' Specify fill patterns for the map variable `pattern` (see [tm_polygons()]), either as
#' constant value or as `values` of a scale, e.g. [tm_scale_categorical()].
#' The arguments are vectorized, so one call can specify several patterns.
#' A plain character vector of pattern types and shortcuts can also be used instead,
#' as well as a list of `tm_patterns()` calls.
#'
#' Pattern types:
#'
#' | Type | Parameters |
#' |---|---|
#' | `"none"` | -- |
#' | `"hatch"` | `angle` (default 45) |
#' | `"crosshatch"` | `angle` (default 0) |
#' | `"dots"` | `random` (default 0) |
#' | `"dashes"` | `angle` (default 0), `length` (default 0.5) |
#' | `"bricks"` | `angle` (default 0), `length` (default 2) |
#' | `"herringbone"` | `angle` (default 0), `length` (default 2) |
#' | `"hexagons"` | `angle` (default 0) |
#' | `"waves"` | `angle` (default 0), `amplitude` (default 0.25) |
#' | `"zigzag"` | `angle` (default 0), `amplitude` (default 0.25) |
#' | `"circles"` | `random` (default 0) |
#' | `"crosses"` | `random` (default 0) |
#' | `"noise"` | -- |
#'
#' Shortcuts: `"hatch_up"` (same as `"hatch"`), `"hatch_down"` (angle 135),
#' `"hatch_horizontal"` (angle 0), `"hatch_vertical"` (angle 90),
#' `"crosshatch_diagonal"` (crosshatch with angle 45), and `"stipple"` (dots with `random = 0.7`).
#'
#' The spacing between the pattern elements is determined by the map variable
#' `pattern_density`, the line width or dot size by `pattern_size`, and the color by `pattern_col`.
#' The parameters `length` and `amplitude` are relative to that spacing, so the whole
#' motif is scaled with the density.
#'
#' @param type Pattern type or shortcut. See details.
#' @param angle Angle of the motif in degrees, counter-clockwise from horizontal. For `"hatch"`, 0 means horizontal lines and 45 lines from lower left to upper right.
#' @param random Randomness of the placement of dots, circles, or crosses, between 0 (regular grid) and 1 (maximal random displacement within the grid cell). Elements do not clump, since each grid cell contains one element.
#' @param length Length of dashes, bricks, or herringbone blocks, relative to the spacing.
#' @param amplitude Amplitude of waves and zigzag lines, relative to the spacing.
#' @return A character vector of class `tmap_patterns` with one pattern specification per element.
#' @example ./examples/tm_patterns.R
#' @seealso [tm_polygons()]
#' @export
tm_patterns = function(type = "hatch", angle = NA, random = NA, length = NA, amplitude = NA) {
	args = list(angle = angle, random = random, length = length, amplitude = amplitude)
	n = max(base::length(type), vapply(args, base::length, integer(1)))
	nms = if (base::length(type) == n) names(type) else NULL
	type = rep_len(type, n)
	args = lapply(args, rep_len, length.out = n)

	specs = vapply(seq_len(n), function(i) {
		pars = lapply(args, "[[", i)
		pars = pars[!vapply(pars, is.na, logical(1))]
		pattern_spec(type[i], pars, warn = TRUE)
	}, FUN.VALUE = character(1))
	structure(specs, names = nms, class = c("tmap_patterns", "character"))
}

#' @export
print.tmap_patterns = function(x, ...) {
	print(unclass(x), ...)
	invisible(x)
}

#' @export
format.tmap_patterns = function(x, ...) {
	format(unclass(x), ...)
}

# Pattern specification string, e.g. "hatch(angle=45)", from a type (or shortcut) and parameters
pattern_spec = function(type, pars = list(), warn = FALSE) {
	if (is.na(type)) return(NA_character_)
	if (type %in% names(.pattern_shortcuts)) {
		sc = .pattern_shortcuts[[type]]
		type = sc$type
		pars = c(pars, sc[setdiff(names(sc), c("type", names(pars)))])
	}
	if (!(type %in% names(.pattern_types))) {
		cli::cli_abort(c("Unknown pattern type {.val {type}}.",
						 i = "Available types: {.val {names(.pattern_types)}}; shortcuts: {.val {names(.pattern_shortcuts)}}."),
					   call = NULL)
	}
	def = .pattern_types[[type]]
	unused = setdiff(names(pars), names(def))
	if (warn && length(unused)) {
		cli::cli_warn("Pattern type {.val {type}} does not use {.arg {unused}}", .frequency = "regularly", .frequency_id = paste0("pattern_unused_", type))
	}
	pars = c(pars[intersect(names(pars), names(def))], def[setdiff(names(def), names(pars))])
	pars = pars[names(def)]
	if (!length(pars)) return(type)
	paste0(type, "(", paste(names(pars), vapply(pars, function(p) as.character(signif(as.numeric(p), 6)), character(1)), sep = "=", collapse = ","), ")")
}

# Parse a pattern specification string (or type or shortcut) into list(type, pars)
pattern_parse = function(x) {
	if (is.na(x)) return(NULL)
	m = regmatches(x, regexec("^([a-z_]+)(\\((.*)\\))?$", x))[[1]]
	if (!length(m)) cli::cli_abort("Incorrect pattern specification {.val {x}}", call = NULL)
	pars = list()
	if (nzchar(m[4])) {
		kv = strsplit(strsplit(m[4], ",", fixed = TRUE)[[1]], "=", fixed = TRUE)
		pars = structure(lapply(kv, function(p) as.numeric(p[2])), names = vapply(kv, "[", character(1), 1))
	}
	list(type = m[2], pars = pars)
}

# Normalize pattern values (character vector of types/shortcuts/specifications, a tmap_patterns
# object, or a list of these) to a character vector of specification strings
pattern_norm = function(x) {
	if (is.list(x)) x = unlist(lapply(x, unclass))
	if (is.factor(x)) x = as.character(x)
	if (!is.character(x) && !all(is.na(x))) cli::cli_abort("Pattern values should be pattern types or {.fun tm_patterns} specifications", call = NULL)
	nms = names(x)
	res = vapply(as.character(x), function(xi) {
		p = pattern_parse(xi)
		if (is.null(p)) NA_character_ else pattern_spec(p$type, p$pars)
	}, FUN.VALUE = character(1), USE.NAMES = FALSE)
	names(res) = nms
	res
}

# Are x valid pattern values?
pattern_valid = function(x) {
	tryCatch({
		y = pattern_norm(x)
		length(y) > 0 && !all(is.na(y))
	}, error = function(e) FALSE)
}
