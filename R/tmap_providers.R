#' @rdname tmap_providers
#' @param provider provider name
#' @export
tmap_provider_credits = function(provider) {
	x = tmap_providers(credits = TRUE)
	if (provider %in% names(x)) {
		x[[provider]]
	} else
		""
}

#' Get basemap tiles providers
#'
#' Get basemap tiles providers and the credits (attribution text). [tmap_providers()] returns a list (or vector) with provider nams (or credits). [tmap_provider_credits()]
#'
#' @param mode mode. If not specified the default mode is used
#' @param credits If `TRUE` the credit (attribution) text is returned. If `FALSE` (default) the provider name.
#' @param as.list Should the output be returned as list where names are provider names? By default `TRUE` when `credits` is also `TRUE`.
#' @return list or vector (see `as.list`) with providers (or credits). [tmap_provider_credits()] returns the credits text for the provided provider.
#' @export
#' @name tmap_providers
tmap_providers = function(mode, credits = FALSE, as.list = credits) {
	if (missing(mode)) mode = getOption("tmap.mode")
	gs = tmap_graphics_name(mode)

	fun = paste0("tmap", gs, "Providers")
	x = do.call(fun, list(credits = credits))
	if (as.list) {
		x
	} else {
		unlist(x, use.names = FALSE)
	}
}
