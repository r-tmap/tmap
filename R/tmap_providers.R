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

#' Get basemap tile providers
#'
#' Get basemap tile providers and their credits (attribution text).
#' \code{tmap_providers()} returns a list or vector of provider names or
#' credits. \code{tmap_provider_credits()} returns the attribution text for a
#' specific provider.
#'
#' \code{.tmap_providers} is an environment populated with all available
#' provider names as named entries. Its primary purpose is to enable
#' autocomplete in IDEs such as RStudio: typing \code{.tmap_providers$} in the
#' console or a script triggers a dropdown list of all available providers,
#' making it easy to discover and select provider names without consulting the
#' documentation. It is not intended to be called as a function.
#'
#' @param mode mode. If not specified the current active mode is used.
#' @param credits If \code{TRUE} the credit (attribution) text is returned.
#'   If \code{FALSE} (default) the provider name.
#' @param as.list Should the output be returned as a list where names are
#'   provider names? By default \code{TRUE} when \code{credits} is also
#'   \code{TRUE}.
#' @return \code{tmap_providers()} returns a list or vector (see
#'   \code{as.list}) of provider names or credits.
#'   \code{tmap_provider_credits()} returns the attribution text for the
#'   specified provider. \code{.tmap_providers} is an environment; see
#'   Details.
#' @export
#' @name tmap_providers
#' @examples
#' # List all providers for the current mode
#' tmap_providers()
#'
#' # Use IDE autocomplete to discover providers interactively:
#' # type .tmap_providers$ in the RStudio console
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
