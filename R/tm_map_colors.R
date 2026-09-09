#' Color adjacent polygons differently
#'
#' Fill polygons such that adjacent ones get different colors. This is a
#' geometry-derived map variable: it is not taken from the data, but computed
#' from the shape itself, with [tmaptools::map_coloring()]. The plain variable
#' name `"MAP_COLORS"` does the same with the default settings.
#'
#' A map can be colored in many equally valid ways. `permutation` picks one of
#' them: raise it until the map looks good. It changes both which polygons share
#' a color and which color each group is given, and the result does not depend
#' on the random seed, so a map keeps its colors between plots.
#'
#' All colors are used about equally often, so no color dominates the map. When
#' there are too few for adjacent polygons to differ everywhere, more are used,
#' and a message says so.
#'
#' @param ncols Number of colors. By default the palette decides: as many colors
#'   as it provides, so that a palette of three colors gives three groups rather
#'   than seven groups that have to share three colors. A number given here is
#'   capped by what the palette can supply.
#' @param permutation Which of the many colorings of the map to use. Each whole
#'   number gives a different, equally valid map.
#' @param ... Passed on to [tmaptools::map_coloring()], e.g. `algorithm`.
#' @return The name of the geometry-derived variable, with the arguments
#'   attached.
#' @seealso [tmaptools::map_coloring()]
#' @example ./examples/tm_map_colors.R
#' @export
tm_map_colors = function(ncols = NA, permutation = 0, ...) {
	if (!identical(ncols, NA) && (!is.numeric(ncols) || length(ncols) != 1L || is.na(ncols) || ncols < 1)) {
		cli::cli_abort("{.arg ncols} should be a single positive number, or {.val NA} to let the palette decide.")
	}
	if (!is.numeric(permutation) || length(permutation) != 1L || is.na(permutation) || permutation < 0) {
		cli::cli_abort("{.arg permutation} should be a whole number, 0 or higher.")
	}
	args = list(permutation = as.integer(permutation), ...)
	if (!identical(ncols, NA)) args$ncols = as.integer(ncols)
	structure("MAP_COLORS", map_colors = args)
}
