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
	noise = list(),
	terrain = list(grain = 1))

# Shortcuts: pattern types with specific parameter values
.pattern_shortcuts = list(
	hatch_up = list(type = "hatch", angle = 45),
	hatch_down = list(type = "hatch", angle = 135),
	hatch_horizontal = list(type = "hatch", angle = 0),
	hatch_vertical = list(type = "hatch", angle = 90),
	crosshatch_diagonal = list(type = "crosshatch", angle = 45),
	stipple = list(type = "dots", random = 0.7))

# Pattern sets, used as pattern "palettes" in the values of scales; the first n patterns are used.
# A "-" prefix reverses the order. Guidelines used for the order (see sandbox/patterns_plan.md):
# - neighbors in the order differ in motif, not only in orientation (orientation alone is a weak way
#   to tell areas apart), e.g. hatch and dots, rather than hatch and hatch_down
# - the first patterns have a similar amount of ink, so that no category stands out; the darker
#   patterns (crosshatch, circles, hexagons) come later
# - "stipple" is not used: random dots read as a density (dot density maps), not as a category
.pattern_sets = list(
	mixed = c("hatch", "dots", "hatch_down", "bricks", "waves", "hatch_horizontal", "zigzag",
			  "hatch_vertical", "hexagons", "crosses", "herringbone", "crosshatch", "circles",
			  "dashes"),
	classic = c("hatch", "dots", "hatch_down", "hatch_horizontal", "crosshatch", "hatch_vertical",
				"crosshatch_diagonal", "dashes"),
	lines = c("hatch", "waves", "hatch_down", "dashes", "hatch_horizontal", "zigzag",
			  "hatch_vertical", "herringbone", "crosshatch", "crosshatch_diagonal"),
	shapes = c("dots", "bricks", "circles", "hexagons", "crosses", "noise"))

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
#' | `"dots"` | `random` (default 0), `seed` (default 1) |
#' | `"dashes"` | `angle` (default 0), `length` (default 0.5) |
#' | `"bricks"` | `angle` (default 0), `length` (default 2) |
#' | `"herringbone"` | `angle` (default 0), `length` (default 2) |
#' | `"hexagons"` | `angle` (default 0) |
#' | `"waves"` | `angle` (default 0), `amplitude` (default 0.25) |
#' | `"zigzag"` | `angle` (default 0), `amplitude` (default 0.25) |
#' | `"circles"` | `random` (default 0), `seed` (default 1) |
#' | `"crosses"` | `random` (default 0), `seed` (default 1) |
#' | `"noise"` | -- |
#' | `"terrain"` | `grain` (default 1), `seed` (default 1) |
#'
#' Shortcuts: `"hatch_up"` (same as `"hatch"`), `"hatch_down"` (angle 135),
#' `"hatch_horizontal"` (angle 0), `"hatch_vertical"` (angle 90),
#' `"crosshatch_diagonal"` (crosshatch with angle 45), and `"stipple"` (dots with `random = 0.7`).
#'
#' Pattern sets, which are used like color palettes:
#'
#' | Set | Patterns | Use |
#' |---|---|---|
#' | `"mixed"` | 14: hatch, dots, hatch_down, bricks, waves, ... | default; patterns that differ most |
#' | `"classic"` | 8: hatches in four directions, crosshatches, dots, dashes | the classic black-and-white look |
#' | `"lines"` | 10: hatches, waves, zigzag, dashes, herringbone, crosshatches | line patterns only |
#' | `"shapes"` | 6: dots, bricks, circles, hexagons, crosses, noise | no line patterns, e.g. to combine with a hatched layer |
#'
#' Used as `values` of a scale, the first `n` patterns are taken. A `"-"` prefix reverses the order.
#' With `tm_patterns()`, a set is expanded to all its patterns, which can be subsetted, e.g.
#' `tm_patterns("classic")[c(1, 3, 5)]`.
#'
#' The patterns are shown with [tmap_show_values()]: all types and shortcuts with
#' `tmap_show_values("pattern")`, and a set with e.g. `tmap_show_values("pattern", values = "classic")`.
#'
#' The spacing between the pattern elements is determined by the map variable
#' `pattern_density`, the line width or dot size by `pattern_size`, and the color by `pattern_col`.
#' The parameters `length` and `amplitude` are relative to that spacing, so the whole
#' motif is scaled with the density.
#'
#' The pattern type `"terrain"` is a texture rather than a motif: random relief, shaded with light
#' from the northwest, which looks like rough terrain. It is drawn as highlights (white) and shadows
#' (`pattern_col`) on top of the fill color, which cancel out roughly, so the average color does not
#' change much. Its strength is determined by `pattern_density` (between 0, no texture, and 1), which
#' makes it suitable for showing uncertainty: the rougher, the less certain. `pattern_size` is not used.
#' In plot mode, it requires a graphics device that supports clipping paths.
#'
#' @param type Pattern type, shortcut, or pattern set. See details.
#' @param angle Angle of the motif in degrees, counter-clockwise from horizontal. For `"hatch"`, 0 means horizontal lines and 45 lines from lower left to upper right.
#' @param random Randomness of the placement of dots, circles, or crosses, between 0 (regular grid) and 1 (displacement of up to half a grid cell). Each grid cell contains one element, so elements are spread evenly, although they may touch at high densities.
#' @param length Length of dashes, bricks, or herringbone blocks, relative to the spacing.
#' @param amplitude Amplitude of waves and zigzag lines, relative to the spacing.
#' @param grain Size of the relief of `"terrain"`: the largest wavelength, relative to the spacing at density 1
#'   (which is 1/20 inch by default, see the `scale` argument of [tm_layout()]). By default 1. The relief cannot
#'   be finer than the resolution allows (wavelengths of at least 3 pixels), so smaller values only have an
#'   effect at high resolutions, e.g. [tmap_save()] with 300 dpi.
#' @param seed Seed (a positive integer) of the random placement of dots, circles, or crosses (with `random > 0`),
#'   and of the relief of `"terrain"`.
#'   Patterns with different seeds have elements at different positions, which is useful when several dot
#'   patterns are drawn on top of each other, e.g. a dot density map with one layer per category. By default 1.
#' @return A character vector of class `tmap_patterns` with one pattern specification per element.
#' @example ./examples/tm_patterns.R
#' @seealso [tm_polygons()], [tmap_show_values()]
#' @export
tm_patterns = function(type = "hatch", angle = NA, random = NA, length = NA, amplitude = NA, grain = NA, seed = NA) {
	type = pattern_expand_sets(type)
	args = list(angle = angle, random = random, length = length, amplitude = amplitude, grain = grain, seed = seed)
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
						 i = "Available types: {.val {names(.pattern_types)}}.",
						 i = "Shortcuts: {.val {names(.pattern_shortcuts)}}."),
					   call = NULL)
	}
	def = .pattern_types[[type]]
	# seed: optional parameter of the randomly placed elements; only part of the specification when not 1
	extra = if (type %in% c("dots", "circles", "crosses", "terrain")) "seed" else character()
	seed = pars$seed
	unused = setdiff(names(pars), c(names(def), extra))
	if (warn && length(unused)) {
		cli::cli_warn("Pattern type {.val {type}} does not use {.arg {unused}}",
					  .frequency = "regularly", .frequency_id = paste0("pattern_unused_", type))
	}
	pars = c(pars[intersect(names(pars), names(def))], def[setdiff(names(def), names(pars))])
	pars = pars[names(def)]
	if (length(extra) && !is.null(seed) && !is.na(seed) && seed != 1) pars$seed = round(seed)
	if (!length(pars)) return(type)
	vals = vapply(pars, function(p) as.character(signif(as.numeric(p), 6)), character(1))
	paste0(type, "(", paste(names(pars), vals, sep = "=", collapse = ","), ")")
}

# Parse a pattern specification string (or type or shortcut) into list(type, pars)
pattern_parse = function(x) {
	if (is.na(x)) return(NULL)
	m = regmatches(x, regexec("^([a-z_]+)(\\((.*)\\))?$", x))[[1]]
	if (!length(m)) cli::cli_abort("Incorrect pattern specification {.val {x}}", call = NULL)
	pars = list()
	if (nzchar(m[4])) {
		kv = strsplit(strsplit(m[4], ",", fixed = TRUE)[[1]], "=", fixed = TRUE)
		pars = structure(lapply(kv, function(p) as.numeric(p[2])),
						 names = vapply(kv, "[", character(1), 1))
	}
	list(type = m[2], pars = pars)
}

# Is x the name of a pattern set (optionally with "-" prefix)?
pattern_is_set = function(x) {
	is.character(x) & sub("^-", "", x) %in% names(.pattern_sets)
}

# Patterns of a set (all of them, or n of them for a scale)
pattern_set = function(x, n = NULL) {
	nm = sub("^-", "", x)
	ps = .pattern_sets[[nm]]
	if (startsWith(x, "-")) ps = rev(ps)
	if (is.null(n)) return(ps)
	k = length(ps)
	if (n > k) {
		cli::cli_warn(paste("{.field [pattern]} the pattern set {.val {nm}} contains {k} patterns,",
							"which is fewer than the number of classes ({n}), so patterns are recycled"))
		return(rep(ps, length.out = n))
	}
	ps[seq_len(n)]
}

# Replace pattern set names by their patterns
pattern_expand_sets = function(x) {
	if (!is.character(x) || !any(pattern_is_set(x))) return(x)
	res = lapply(x, function(xi) if (pattern_is_set(xi)) pattern_set(xi) else xi)
	unlist(res, use.names = FALSE)
}

# Normalize pattern values (character vector of types/shortcuts/specifications, a tmap_patterns
# object, or a list of these) to a character vector of specification strings
pattern_norm = function(x) {
	if (is.list(x)) x = unlist(lapply(x, unclass))
	if (is.factor(x)) x = as.character(x)
	x = pattern_expand_sets(x)
	if (!is.character(x) && !all(is.na(x))) {
		cli::cli_abort("Pattern values should be pattern types or {.fun tm_patterns} specifications",
					   call = NULL)
	}
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
