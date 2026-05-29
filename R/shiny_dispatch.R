# --- dispatch carrier -------------------------------------------------------

tmap_graphics_class <- function(mode = NA) {
	if (is.na(mode)) mode <- getOption("tmap.mode")
	gs <- tmap_graphics_name(mode)
	structure(list(mode = mode, gs = gs), class = c(gs, "tmapGS"))
}

# --- generics (exported so extension pkgs can add methods) ------------------

#' @rdname renderTmap
#' @export
renderTmapGS <- function(x, ...) UseMethod("renderTmapGS")

#' @rdname renderTmap
#' @export
tmapOutputGS <- function(x, ...) UseMethod("tmapOutputGS")

#' @rdname renderTmap
#' @export
tmapProxyGS <- function(x, ...) UseMethod("tmapProxyGS")

#' @exportS3Method
renderTmapGS.default <- function(x, ...)
	stop('renderTmap does not (yet) support mode "', x$mode, '"', call. = FALSE)

#' @exportS3Method
tmapOutputGS.default <- function(x, ...)
	stop('tmapOutput does not (yet) support mode "', x$mode, '"', call. = FALSE)

#' @exportS3Method
tmapProxyGS.default <- function(x, ...)
	stop('tmapProxy does not (yet) support mode "', x$mode, '"', call. = FALSE)

