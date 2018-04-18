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
#'  \item \code{tm_style_white}: replaced by \code{\link{tm_style}(("white")}
#'  \item \code{tm_style_gray}: replaced by \code{\link{tm_style}(("gray")}
#'  \item \code{tm_style_grey}: replaced by \code{\link{tm_style}(("grey")}
#'  \item \code{tm_style_natural}: replaced by \code{\link{tm_style}(("natural")}
#'  \item \code{tm_style_cobalt}: replaced by \code{\link{tm_style}(("cobalt")}
#'  \item \code{tm_style_col_blind}: replaced by \code{\link{tm_style}(("col_blind")}
#'  \item \code{tm_style_albatross}: replaced by \code{\link{tm_style}(("albatross")}
#'  \item \code{tm_style_beaver}: replaced by \code{\link{tm_style}(("beaver")}
#'  \item \code{tm_style_bw}: replaced by \code{\link{tm_style}(("bw")}
#'  \item \code{tm_style_classic}: replaced by \code{\link{tm_style}(("classic")}
#'  \item \code{tm_format_World}: replaced by \code{\link{tm_format}(("World")}
#'  \item \code{tm_format_World_wide}: replaced by \code{\link{tm_format}(("World_wide")}
#'  \item \code{tm_format_NLD}: replaced by \code{\link{tm_format}(("NLD")}
#'  \item \code{tm_format_NLD_wide}: replaced by \code{\link{tm_format}(("NLD_wide")}
#'  \item \code{tm_format_Europe}: not used anymore, since the dataset Europe is no longer maintained
#'  \item \code{tm_format_Europe2}: not used anymore, since the dataset Europe is no longer maintained
#'  \item \code{tm_format_Europe_wide}: not used anymore, since the dataset Europe is no longer maintained
#' }
#' 
#' @name deprecated functions
NULL

#' @keywords internal
#' @export
animation_tmap <- function(...) {
	warning("animation_tmap is deprecated as of tmap version 2.0. Please use tmap_animation instead")
	tmap_animation(...)
}

#' @keywords internal
#' @export
save_tmap <- function(...) {
	warning("save_tmap is deprecated as of tmap version 2.0. Please use tmap_save instead")
	tmap_save(...)
}


#' @keywords internal
#' @export
style_catalogue <- function(...) {
	warning("style_catalogue is deprecated as of tmap version 2.0. Please use tmap_style_catalogue")
	tmap_style_catalogue(...)
}


#' @export
style_catalog <- function(...) {
	warning("style_catalog is deprecated as of tmap version 2.0. Please use tmap_style_catalog")
	tmap_style_catalog(...)
}


#' @keywords internal
#' @export
last_map <- function() {
	warning("last_map is deprecated as of tmap version 2.0. Please use tmap_last")
	tmap_last()
}

#' @keywords internal
#' @export
tm_style_white <- function(...) {
	warning("tm_style_white is deprecated as of tmap version 2.0. Please use tm_style(\"white\", ...) instead")
	tm_style("white", ...)
}

#' @keywords internal
#' @export
tm_style_gray <- function(...) {
	warning("tm_style_gray is deprecated as of tmap version 2.0. Please use tm_style(\"gray\", ...) instead")
	tm_style("gray", ...)
}

#' @keywords internal
#' @export
tm_style_natural <- function(...) {
	warning("tm_style_natural is deprecated as of tmap version 2.0. Please use tm_style(\"natural\", ...) instead")
	tm_style("natural", ...)
}


#' @rdname tm_layout
#' @export
tm_style_grey <- function(...) {
	warning("tm_style_grey is deprecated as of tmap version 2.0. Please use tm_style(\"grey\", ...) instead")
	tm_style("grey", ...)
}

#' @rdname tm_layout
#' @export
tm_style_cobalt <- function(...) {
	warning("tm_style_white is deprecated as of tmap version 2.0. Please use tm_style(\"cobalt\", ...) instead")
	tm_style("cobalt", ...)
}

#' @keywords internal
#' @export
tm_style_col_blind <- function(...) {
	warning("tm_style_col_blind is deprecated as of tmap version 2.0. Please use tm_style(\"col_blind\", ...) instead")
	tm_style("col_blind", ...)
}


#' @keywords internal
#' @export
tm_style_albatross <- function(...) {
	warning("tm_style_albatross is deprecated as of tmap version 2.0. Please use tm_style(\"albatross\", ...) instead")
	tm_style("albatross", ...)
}

#' @keywords internal
#' @export
tm_style_beaver <- function(...) {
	warning("tm_style_beaver is deprecated as of tmap version 2.0. Please use tm_style(\"beaver\", ...) instead")
	tm_style("beaver", ...)
}

#' @keywords internal
#' @export
tm_style_bw <- function(...) {
	warning("tm_style_bw is deprecated as of tmap version 2.0. Please use tm_style(\"bw\", ...) instead")
	tm_style("bw", ...)
}

#' @keywords internal
#' @export
tm_style_classic <- function(...) {
	warning("tm_style_classic is deprecated as of tmap version 2.0. Please use tm_style(\"classic\", ...) instead")
	tm_style("classic", ...)
}

#' @keywords internal
#' @export
tm_format_World <- function(...) {
	warning("tm_format_World is deprecated as of tmap version 2.0. Please use tm_format(\"World\", ...) instead")
	tm_format("World", ...)
}


#' @keywords internal
#' @export
tm_format_World_wide <- function(...) {
	warning("tm_format_World is deprecated as of tmap version 2.0. Please use tm_format(\"World\", ...) instead")
	tm_format("World", ...)
}


#' @keywords internal
#' @export
tm_format_Europe <- function(...) {
	warning("tm_format_Europe not used anymore as of tmap version 2.0, since the data object Europe is no longer contained")
}


#' @keywords internal
#' @export
tm_format_Europe2 <- function(...) {
	warning("tm_format_Europe2 not used anymore as of tmap version 2.0, since the data object Europe is no longer contained")
}


#' @keywords internal
#' @export
tm_format_Europe_wide <- function(...) {
	warning("tm_format_Europe_wide not used anymore as of tmap version 2.0, since the data object Europe is no longer contained")
}




#' @keywords internal
#' @export
tm_format_NLD <- function(...) {
	warning("tm_format_NLD is deprecated as of tmap version 2.0. Please use tm_format(\"NLD\", ...) instead")
	tm_format("NLD", ...)
}

#' @keywords internal
#' @export
tm_format_NLD_wide <- function(...) {
	warning("tm_format_NLD_wide is deprecated as of tmap version 2.0. Please use tm_format(\"NLD_wide\", ...) instead")
	tm_format("NLD_wide", ...)
}

