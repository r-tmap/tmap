#' Set the default tmap style
#' 
#' Set the default tmap style, which is contained in the global option \code{tmap.style} (see also \code{\link{tmap_options}}. The default style (i.e. when loading the package) is \code{"white"}.
#' 
#' @param style name of the style. The function \code{tm_style_<style>} should exist and be a wrapper of \code{\link{tm_layout}}. The default style when loading the package is \code{"white"}, which corresponds to the function \code{\link{tm_style_white}}. See \code{\link{tm_layout}} for predefined styles, and \code{\link{style_catalogue}} for creating a catelogue.
#' @return the style before changing
#' @seealso \code{\link{tm_layout}} for predefined styles, \code{\link{tmap_style_catalogue}} to create a style catelogue of all available styles, and \code{\link{tmap_options}} for tmap options.
#' @example ./examples/tmap_style.R
#' @seealso \code{\link{tmap_options}} for tmap options
#' @export
#' @rdname tmap_style
tmap_style <- function(style) {
	current.style <- getOption("tmap.style")
	
	if (missing(style)) {
		message("current tmap style is \"", current.style, "\"")
		message("other available styles are: \"", paste(other_styles(current.style), collapse = "\", \""), "\" ")
	} else {
		.tmapOptions <- get(".tmapOptions", envir = .TMAP_CACHE)	
		check_style(style)
		options(tmap.style=style)
		if (style == "white") {
			.tmapOptions <- .defaultTmapOptions
		} else {
			styleOptions <- get(".tmapStyles", envir = .TMAP_CACHE)[[style]]
			.tmapOptions[names(styleOptions)] <- styleOptions
			attr(.tmapOptions, "style") <- style
		}
		
		assign(".tmapOptions", .tmapOptions, envir = .TMAP_CACHE)	
		message("tmap style set to \"", style, "\"")
		message("other available styles are: \"", paste(other_styles(style), collapse = "\", \""), "\" ")
	}
	invisible(current.style)
}


other_styles <- function(style) {
	otherstyles <- setdiff(names(get(".tmapStyles", envir = .TMAP_CACHE)), style)
	if (style %in% c("gray", "grey")) {
		otherstyles <- setdiff(otherstyles, c("gray", "grey"))
	} else {
		otherstyles <- setdiff(otherstyles, "grey")
	}
	otherstyles
}

#' @export
#' @rdname tmap_style
#' @param x tmap options list (should be the same format as \code{tmap_options()})
tmap_style_load <- function(x) {
	style <- attr(x, "style")
	attr(x, "style") <- NULL
	styles <- get(".tmapStyles", envir = .TMAP_CACHE)
	styles[[style]] <- x
	assign(".tmapStyles", styles, envir = .TMAP_CACHE)
	message("style \"", style, "\" loaded successfully")
	invisible(NULL)
}

#' @export
#' @rdname tmap_style
tmap_style_save <- function(style) {
	stylediff <- suppressMessages(tmap_options_diff())
	
	.tmapOptions <- get(".tmapOptions", envir = .TMAP_CACHE)	
	
	if (is.null(stylediff)) {
		message("current style is the same as the default style, so nothing to save")
		return(invisible(.tmapOptions))
	}
	
	options(tmap.style=style)
	attr(.tmapOptions, "style") <- style
	assign(".tmapOptions", .tmapOptions, envir = .TMAP_CACHE)
	
	styles <- get(".tmapStyles", envir = .TMAP_CACHE)
	styles[[style]] <- suppressMessages(tmap_options_diff())
	assign(".tmapStyles", styles, envir = .TMAP_CACHE)
	
	message("current tmap options saved as style \"", style, "\"")
	invisible(.tmapOptions)
}

check_style <- function(style) {
	obs <- c(ls(envir = .GlobalEnv), ls("package:tmap"))
	fname <- paste("tm_style", style, sep="_")
	if (!fname %in% obs) stop("current style \"" , style, "\" unknown, i.e. the function \"" , fname, "\" does not exist.", call. = FALSE)
}
