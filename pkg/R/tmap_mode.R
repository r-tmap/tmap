tmap_mode <- function(mode=c("plot", "view")) {
	if (is.null(match.call(expand.dots = TRUE)[-1])) {
		current.mode <- getOption("tmap.mode")
		message("current tmap mode is \"", current.mode, "\"")
	} else {
		mode <- match.arg(mode)
		options(tmap.mode=mode)
		if (mode=="plot") {
			message("tmap mode set to plotting")
		} else {
			message("tmap mode set to interactive viewing")
		}
	}
}	

tmap_toggle <- function() {
	current.mode <- getOption("tmap.mode")
	tmap_mode(ifelse(current.mode=="plot", "view", "plot"))
}

tmap_style <- function(style) {
	if (missing(style)) {
		message("current tmap style is \"", getOption("tmap.style"), "\"")
	} else {
		obs <- c(ls(), ls("package:tmap"))
		fname <- paste("tm_style", style, sep="_")
		if (!fname %in% obs) warning("current style \"" , style, "\" unknown, i.e. the function \"" , fname, "\" does not exist.")
		options(tmap.style=style)
	}
}
