# Cross-mode basemap equivalence registry
# ----------------------------------------
# Different modes expose different basemap providers (see tmap_providers()).
# When a map made in one mode is reproduced in another, a provider that is valid
# in the first mode may not exist in the second. This registry maps providers to
# a small set of visual "concepts" (gray canvas, dark, satellite, ...) so the
# closest available provider in the target mode can be substituted, rather than
# falling back to the generic default. The mappings are necessarily approximate.
#
# Mode extensions (e.g. tmap.mapgl) declare their equivalences with
# tmapSubmitBasemapEquivalents(). Each concept is a named list with mode names
# (e.g. "plot", "view", "maplibre", "mapbox") as names and provider name(s) as
# values. When several providers are listed for a mode, the first is the
# representative used when substituting into that mode; all of them are matched
# when identifying the concept of an incoming provider.

.tmap_basemap_equiv = new.env(parent = emptyenv())
.tmap_basemap_equiv$concepts = list()

#' Register cross-mode basemap equivalences
#'
#' Used by tmap mode extensions to declare which basemap providers are visually
#' equivalent across modes. This lets a basemap be approximated when a map is
#' reproduced in a different mode, instead of falling back to the mode default.
#'
#' @param equivalents a named list of concept entries. Each entry is itself a
#'   named list with mode names (e.g. `plot`, `view`, `maplibre`, `mapbox`) as
#'   names and provider name(s) as values.
#' @param replace if `TRUE`, replace the whole registry; otherwise the entries
#'   are merged by concept name (existing names are overwritten), so repeated
#'   submissions are idempotent and multiple extensions compose.
#' @export
#' @keywords internal
tmapSubmitBasemapEquivalents = function(equivalents, replace = FALSE) {
	if (replace) {
		.tmap_basemap_equiv$concepts = equivalents
	} else {
		.tmap_basemap_equiv$concepts = utils::modifyList(.tmap_basemap_equiv$concepts, equivalents)
	}
	invisible(NULL)
}

# Closest equivalent of `serv` in `to_mode`, or NA_character_ if `serv` is not
# known to the registry. `serv` may be a provider from any mode.
basemap_equivalent = function(serv, to_mode) {
	for (concept in .tmap_basemap_equiv$concepts) {
		if (serv %in% unlist(concept, use.names = FALSE)) {
			q = concept[[to_mode]]
			if (!is.null(q) && length(q)) return(q[[1]])
		}
	}
	NA_character_
}

# Gentle message used when a basemap is substituted by its cross-mode
# equivalent (as opposed to a genuinely unknown provider, which warns).
message_basemaps_equivalent = function(serv, mode, eq) {
	cli::cli_inform(c(
		"i" = "{.field [basemaps]} Provider {.str {serv}} is not available in mode {.str {mode}}; using the closest equivalent {.str {eq}}."
	))
}
