#' Deprecated tmap functions
#' 
#' For consistency, tmap function names will be prefixed with a tm_ or tmap_. Therefore, function names such as \code{animation_tmap} have been renamed to \code{tmap_animation}.
#' 
#' @param ... arguments passed on to new functions
#' 
#' @name animation_tmap
#' @rdname Deprecated
animation_tmap <- function(...) {
	warning("animation_tmap is deprecated as of tmap version 2.0. Please use tmap_animation instead")
	tmap_animation(...)
}


#' @name save_tmap
#' @rdname Deprecated
save_tmap <- function(...) {
	warning("save_tmap is deprecated as of tmap version 2.0. Please use tmap_save instead")
	tmap_save(...)
}


#' @name style_catalogue
#' @rdname Deprecated
style_catalogue <- function() {
	warning("style_catalogue is deprecated as of tmap version 2.0. Please use tmap_style_catalogue")
	tmap_style_catalogue(...)
}

#' @name style_catalog
#' @rdname Deprecated
style_catalog <- function() {
	warning("style_catalog is deprecated as of tmap version 2.0. Please use tmap_style_catalog")
	tmap_style_catalog(...)
}


#' @name last_map
#' @rdname Deprecated
last_map <- function() {
	warning("last_map is deprecated as of tmap version 2.0. Please use tmap_last")
	tmap_last(...)
}
