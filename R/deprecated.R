#' Deprecated tmap functions
#' 
#' For consistency, tmap function names will be prefixed with a tm_ or tmap_. Therefore, function names such as \code{animation_tmap} have been renamed to \code{tmap_animation}.
#' 
#' @param ... arguments passed on to new functions
#' 
#' @name deprecated_functions
#' @rdname Deprecated
#' @export
animation_tmap <- function(...) {
	warning("animation_tmap is deprecated as of tmap version 2.0. Please use tmap_animation instead")
	tmap_animation(...)
}


#' @rdname Deprecated
#' @export
save_tmap <- function(...) {
	warning("save_tmap is deprecated as of tmap version 2.0. Please use tmap_save instead")
	tmap_save(...)
}


#' @rdname Deprecated
#' @export
style_catalogue <- function(...) {
	warning("style_catalogue is deprecated as of tmap version 2.0. Please use tmap_style_catalogue")
	tmap_style_catalogue(...)
}

#' @rdname Deprecated
#' @export
style_catalog <- function(...) {
	warning("style_catalog is deprecated as of tmap version 2.0. Please use tmap_style_catalog")
	tmap_style_catalog(...)
}


#' @rdname Deprecated
#' @export
last_map <- function() {
	warning("last_map is deprecated as of tmap version 2.0. Please use tmap_last")
	tmap_last()
}

#' @rdname Deprecated
#' @export
tm_style_white <- function(...) {
	warning("tm_style_white is deprecated as of tmap version 2.0. Please use tm_style(\"white\", ...) instead")
	tm_style("white", ...)
}

#' @rdname Deprecated
#' @export
tm_style_gray <- function(...) {
	warning("tm_style_gray is deprecated as of tmap version 2.0. Please use tm_style(\"gray\", ...) instead")
	tm_style("gray", ...)
}

#' @rdname Deprecated
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

#' @rdname Deprecated
#' @export
tm_style_col_blind <- function(...) {
	warning("tm_style_col_blind is deprecated as of tmap version 2.0. Please use tm_style(\"col_blind\", ...) instead")
	tm_style("col_blind", ...)
}


#' @rdname Deprecated
#' @export
tm_style_albatross <- function(...) {
	warning("tm_style_albatross is deprecated as of tmap version 2.0. Please use tm_style(\"albatross\", ...) instead")
	tm_style("albatross", ...)
}

#' @rdname Deprecated
#' @export
tm_style_beaver <- function(...) {
	warning("tm_style_beaver is deprecated as of tmap version 2.0. Please use tm_style(\"beaver\", ...) instead")
	tm_style("beaver", ...)
}

#' @rdname Deprecated
#' @export
tm_style_bw <- function(...) {
	warning("tm_style_bw is deprecated as of tmap version 2.0. Please use tm_style(\"bw\", ...) instead")
	tm_style("bw", ...)
}

#' @rdname Deprecated
#' @export
tm_style_classic <- function(...) {
	warning("tm_style_classic is deprecated as of tmap version 2.0. Please use tm_style(\"classic\", ...) instead")
	tm_style("classic", ...)
}

#' @rdname Deprecated
#' @export
tm_format_World <- function(...) {
	warning("tm_format_World is deprecated as of tmap version 2.0. Please use tm_format(\"World\", ...) instead")
	tm_format("World", ...)
}


#' @rdname Deprecated
#' @export
tm_format_World_wide <- function(...) {
	warning("tm_format_World is deprecated as of tmap version 2.0. Please use tm_format(\"World\", ...) instead")
	tm_format("World", ...)
}


#' @rdname Deprecated
#' @export
tm_format_Europe <- function(...) {
	warning("tm_format_Europe not used anymore as of tmap version 2.0, since the data object Europe is no longer contained")
}


#' @rdname Deprecated
#' @export
tm_format_Europe2 <- function(...) {
	warning("tm_format_Europe2 not used anymore as of tmap version 2.0, since the data object Europe is no longer contained")
}


#' @rdname Deprecated
#' @export
tm_format_Europe_wide <- function(...) {
	warning("tm_format_Europe_wide not used anymore as of tmap version 2.0, since the data object Europe is no longer contained")
}




#' @rdname Deprecated
#' @export
tm_format_NLD <- function(...) {
	warning("tm_format_NLD is deprecated as of tmap version 2.0. Please use tm_format(\"NLD\", ...) instead")
	tm_format("NLD", ...)
}

#' @rdname Deprecated
#' @export
tm_format_NLD_wide <- function(...) {
	warning("tm_format_NLD_wide is deprecated as of tmap version 2.0. Please use tm_format(\"NLD_wide\", ...) instead")
	tm_format("NLD_wide", ...)
}

