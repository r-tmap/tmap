#' Deprecated tmap functions
#' 
#' For consistency, tmap function names will be prefixed with a tm_ or tmap_. Therefore, function names such as \code{animation_tmap} have been renamed to \code{tmap_animation}.
#' 
#' \itemize{
#'  \item \code{animation_tmap}: replaced by \code{\link{tmap_animation}}
#'  \item \code{save_tmap}: replaced by \code{\link{tmap_save}}
#'  \item \code{style_catalogue}: replaced by \code{\link{tmap_style_catalogue}}
#'  \item \code{style_catalog}: replaced by \code{\link{tmap_style_catalog}}
#'  \item \code{last_map}: replaced by \code{\link{tmap_last}}
#'  \item \code{tm_style_white}: replaced by \code{\link{tm_style}("white")}
#'  \item \code{tm_style_gray}: replaced by \code{\link{tm_style}("gray")}
#'  \item \code{tm_style_grey}: replaced by \code{\link{tm_style}("grey")}
#'  \item \code{tm_style_natural}: replaced by \code{\link{tm_style}("natural")}
#'  \item \code{tm_style_cobalt}: replaced by \code{\link{tm_style}("cobalt")}
#'  \item \code{tm_style_col_blind}: replaced by \code{\link{tm_style}("col_blind")}
#'  \item \code{tm_style_albatross}: replaced by \code{\link{tm_style}("albatross")}
#'  \item \code{tm_style_beaver}: replaced by \code{\link{tm_style}("beaver")}
#'  \item \code{tm_style_bw}: replaced by \code{\link{tm_style}("bw")}
#'  \item \code{tm_style_classic}: replaced by \code{\link{tm_style}("classic")}
#'  \item \code{tm_format_World}: replaced by \code{\link{tm_format}("World")}
#'  \item \code{tm_format_World_wide}: replaced by \code{\link{tm_format}("World_wide")}
#'  \item \code{tm_format_NLD}: replaced by \code{\link{tm_format}("NLD")}
#'  \item \code{tm_format_NLD_wide}: replaced by \code{\link{tm_format}("NLD_wide")}
#'  \item \code{tm_format_Europe}: not used anymore, since the dataset Europe is no longer maintained
#'  \item \code{tm_format_Europe2}: not used anymore, since the dataset Europe is no longer maintained
#'  \item \code{tm_format_Europe_wide}: not used anymore, since the dataset Europe is no longer maintained
#' }
#' 
#' @rdname deprecated_functions
#' @name deprecated_functions
NULL

#' @rdname deprecated_functions
#' @param ... passed on to replacing functions
#' @name animation_tmap
#' @keywords internal
#' @export
animation_tmap <- function(...) {
	warning("animation_tmap is deprecated as of tmap version 2.0. Please use tmap_animation instead")
	tmap_animation(...)
}

#' @rdname deprecated_functions
#' @name save_tmap
#' @keywords internal
#' @export
save_tmap <- function(...) {
	warning("save_tmap is deprecated as of tmap version 2.0. Please use tmap_save instead")
	tmap_save(...)
}


#' @rdname deprecated_functions
#' @name style_catalogue
#' @keywords internal
#' @export
style_catalogue <- function(...) {
	warning("style_catalogue is deprecated as of tmap version 2.0. Please use tmap_style_catalogue")
	tmap_style_catalogue(...)
}


#' @rdname deprecated_functions
#' @name style_catalog
#' @export
style_catalog <- function(...) {
	warning("style_catalog is deprecated as of tmap version 2.0. Please use tmap_style_catalog")
	tmap_style_catalog(...)
}


#' @rdname deprecated_functions
#' @name last_map
#' @keywords internal
#' @export
last_map <- function() {
	warning("last_map is deprecated as of tmap version 2.0. Please use tmap_last")
	tmap_last()
}

#' @rdname deprecated_functions
#' @name tm_style_white
#' @keywords internal
#' @export
tm_style_white <- function(...) {
	warning("tm_style_white is deprecated as of tmap version 2.0. Please use tm_style(\"white\", ...) instead")
	tm_style("white", ...)
}

#' @rdname deprecated_functions
#' @name tm_style_gray
#' @keywords internal
#' @export
tm_style_gray <- function(...) {
	warning("tm_style_gray is deprecated as of tmap version 2.0. Please use tm_style(\"gray\", ...) instead")
	tm_style("gray", ...)
}

#' @rdname deprecated_functions
#' @name tm_style_natural
#' @keywords internal
#' @export
tm_style_natural <- function(...) {
	warning("tm_style_natural is deprecated as of tmap version 2.0. Please use tm_style(\"natural\", ...) instead")
	tm_style("natural", ...)
}


#' @rdname deprecated_functions
#' @name tm_style_grey
#' @keywords internal
#' @export
tm_style_grey <- function(...) {
	warning("tm_style_grey is deprecated as of tmap version 2.0. Please use tm_style(\"grey\", ...) instead")
	tm_style("grey", ...)
}

#' @rdname deprecated_functions
#' @name tm_style_cobalt
#' @keywords internal
#' @export
tm_style_cobalt <- function(...) {
	warning("tm_style_white is deprecated as of tmap version 2.0. Please use tm_style(\"cobalt\", ...) instead")
	tm_style("cobalt", ...)
}

#' @rdname deprecated_functions
#' @name tm_style_col_blind
#' @keywords internal
#' @export
tm_style_col_blind <- function(...) {
	warning("tm_style_col_blind is deprecated as of tmap version 2.0. Please use tm_style(\"col_blind\", ...) instead")
	tm_style("col_blind", ...)
}


#' @rdname deprecated_functions
#' @name tm_style_albatross
#' @keywords internal
#' @export
tm_style_albatross <- function(...) {
	warning("tm_style_albatross is deprecated as of tmap version 2.0. Please use tm_style(\"albatross\", ...) instead")
	tm_style("albatross", ...)
}

#' @rdname deprecated_functions
#' @name tm_style_beaver
#' @keywords internal
#' @export
tm_style_beaver <- function(...) {
	warning("tm_style_beaver is deprecated as of tmap version 2.0. Please use tm_style(\"beaver\", ...) instead")
	tm_style("beaver", ...)
}

#' @rdname deprecated_functions
#' @name tm_style_bw
#' @keywords internal
#' @export
tm_style_bw <- function(...) {
	warning("tm_style_bw is deprecated as of tmap version 2.0. Please use tm_style(\"bw\", ...) instead")
	tm_style("bw", ...)
}

#' @rdname deprecated_functions
#' @name tm_style_classic
#' @keywords internal
#' @export
tm_style_classic <- function(...) {
	warning("tm_style_classic is deprecated as of tmap version 2.0. Please use tm_style(\"classic\", ...) instead")
	tm_style("classic", ...)
}

#' @rdname deprecated_functions
#' @name tm_format_World
#' @keywords internal
#' @export
tm_format_World <- function(...) {
	warning("tm_format_World is deprecated as of tmap version 2.0. Please use tm_format(\"World\", ...) instead")
	tm_format("World", ...)
}


#' @rdname deprecated_functions
#' @name tm_format_World_wide
#' @keywords internal
#' @export
tm_format_World_wide <- function(...) {
	warning("tm_format_World is deprecated as of tmap version 2.0. Please use tm_format(\"World\", ...) instead")
	tm_format("World", ...)
}


#' @rdname deprecated_functions
#' @name tm_format_Europe
#' @keywords internal
#' @export
tm_format_Europe <- function(...) {
	warning("tm_format_Europe not used anymore as of tmap version 2.0, since the data object Europe is no longer contained")
}


#' @rdname deprecated_functions
#' @name style_catalog
#' @keywords internal
#' @export
tm_format_Europe2 <- function(...) {
	warning("tm_format_Europe2 not used anymore as of tmap version 2.0, since the data object Europe is no longer contained")
}


#' @rdname deprecated_functions
#' @name tm_format_Europe_wide
#' @keywords internal
#' @export
tm_format_Europe_wide <- function(...) {
	warning("tm_format_Europe_wide not used anymore as of tmap version 2.0, since the data object Europe is no longer contained")
}




#' @rdname deprecated_functions
#' @name tm_format_NLD
#' @keywords internal
#' @export
tm_format_NLD <- function(...) {
	warning("tm_format_NLD is deprecated as of tmap version 2.0. Please use tm_format(\"NLD\", ...) instead")
	tm_format("NLD", ...)
}

#' @rdname deprecated_functions
#' @name tm_format_NLD_wide
#' @keywords internal
#' @export
tm_format_NLD_wide <- function(...) {
	warning("tm_format_NLD_wide is deprecated as of tmap version 2.0. Please use tm_format(\"NLD_wide\", ...) instead")
	tm_format("NLD_wide", ...)
}

