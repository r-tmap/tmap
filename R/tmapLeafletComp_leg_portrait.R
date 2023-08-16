tmapLeafletCompPrepare = function(comp, o) {
	UseMethod("tmapLeafletCompPrepare")
}

tmapLeafletCompHeight = function(comp, o) {
	UseMethod("tmapLeafletCompHeight")
}

tmapLeafletCompWidth = function(comp, o) {
	UseMethod("tmapLeafletCompWidth")
}

tmapLeafletLegPlot = function(comp, lf, o) {
	UseMethod("tmapLeafletLegPlot")
}


#' @method tmapLeafletCompPrepare tm_legend_standard_portrait
#' @export
tmapLeafletCompPrepare.tm_legend_standard_portrait = function(comp, o) {
	tmapLeaflet_legend_comp(comp, o)
}

tmapLeaflet_legend_comp = function(comp, o) {
	within(comp, {
		if ("biv" %in% names(attributes(gp$fill))) {
			warning("Bivariate legend not implemented for view mode", call. = FALSE)
			show = FALSE
		}
		
		nuq = vapply(comp$gp, length, FUN.VALUE = integer(1))
		varying = names(nuq)[which(nuq>1)]

		if (all(c("col", "fill") %in% varying)) {
			message("Legend in view mode doesn't support both fill and col varying. Setting col to the first value")
			nuq["col"] = 1
			varying = setdiff(varying, "col")
			gp$col = gp$col[1]
		}
		type = if ((!is.na(gp$fill[1]) && any(nchar(gp$fill) > 50)) || (!is.na(gp$fill_alpha[1]) && any(nchar(gp$fill_alpha) > 50)) ||
				   (!is.na(gp$col[1]) && any(nchar(gp$col) > 50)) || (!is.na(gp$col_alpha[1]) && any(nchar(gp$col_alpha) > 50))) {
			"gradient"
		} else {
			"symbols"
		}
		
		bg.color = do.call("process_color", c(list(col=bg.color), o$pc))
		title.color = do.call("process_color", c(list(col=title.color), o$pc))
		text.color = do.call("process_color", c(list(col=text.color), o$pc))
		
		gp2 = gp_to_lpar(gp, mfun = comp$mfun, shape = comp$item.shape)
	})
}


#' @method tmapLeafletCompHeight tm_legend_standard_portrait
#' @export
tmapLeafletCompHeight.tm_legend_standard_portrait = function(comp, o) {
	comp
}



#' @method tmapLeafletCompWidth tm_legend_standard_portrait
#' @export
tmapLeafletCompWidth.tm_legend_standard_portrait = function(comp, o) {
	comp
}





#' @method tmapLeafletCompPrepare tm_legend_standard_landscape
#' @export
tmapLeafletCompPrepare.tm_legend_standard_landscape = function(comp, o) {
	tmapLeaflet_legend_comp(comp, o)
}

#' @method tmapLeafletCompHeight tm_legend_standard_landscape
#' @export
tmapLeafletCompHeight.tm_legend_standard_landscape = function(comp, o) {
	comp
}



#' @method tmapLeafletCompWidth tm_legend_standard_landscape
#' @export
tmapLeafletCompWidth.tm_legend_standard_landscape = function(comp, o) {
	comp
}

