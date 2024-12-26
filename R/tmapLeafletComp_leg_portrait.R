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


#' @export
tmapLeafletCompPrepare.tm_legend_standard_portrait = function(comp, o) {
	tmapLeaflet_legend_comp(comp, o)
}

tmapLeaflet_legend_comp = function(comp, o) {
	icons = all(comp$gp$shape >= 1000)

	# replace gp visual values with user-specified used (e.g. tm_shape(World) + tm_polygons("HPI", fill.legend = tm_legend(col = "red")))
	comp$gp = add_user_specified_values(comp$gp, comp[intersect(names(comp), names(comp$gp))])

	# icons replaced by 'normal' shapes: apply icon.scale
	if (icons && all(comp$gp$shape < 1000) && !is.null(comp$layer_args$icon.scale)) {
		comp$gp$size = comp$gp$size * comp$layer_args$icon.scale
	}

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

		} else if (any(c("bgcol", "bgcol_alpha") %in% varying)) {
			"none"
		} else {
			"symbols"
		}

		bg.color = do.call("process_color", c(list(col=bg.color), o$pc))
		title.color = do.call("process_color", c(list(col=title.color), o$pc))
		text.color = do.call("process_color", c(list(col=text.color), o$pc))


		gp2 = gp_to_lpar(gp, mfun = comp$mfun, shape = comp$item.shape, size_factor = 14)
	})
}


#' @export
tmapLeafletCompHeight.tm_legend_standard_portrait = function(comp, o) {
	within(comp, {
		if (type == "gradient") {
			height = if (is.na(height)) {
				get_legend_option(item.height, "gradient") * nitems * absolute_fontsize
			} else {
				if (height < 0) {
					-height
				} else {
					height * absolute_fontsize
				}
			}
		}
	})
}



#' @export
tmapLeafletCompWidth.tm_legend_standard_portrait = function(comp, o) {
	within(comp, {
		if (type == "gradient") {
			width = if (is.na(width)) {
				get_legend_option(item.width, "gradient") * absolute_fontsize
			} else {
				if (width < 0) {
					-width
				} else {
					width * absolute_fontsize
				}
			}
		}
	})
}





#' @export
tmapLeafletCompPrepare.tm_legend_standard_landscape = function(comp, o) {
	tmapLeaflet_legend_comp(comp, o)
}

#' @export
tmapLeafletCompHeight.tm_legend_standard_landscape = function(comp, o) {
	within(comp, {
		if (type == "gradient") {
			height = if (is.na(height)) {
				get_legend_option(item.height, "gradient") * absolute_fontsize
			} else {
				if (height < 0) {
					-height
				} else {
					height * absolute_fontsize
				}
			}
		}
	})
}



#' @export
tmapLeafletCompWidth.tm_legend_standard_landscape = function(comp, o) {
	within(comp, {
		if (type == "gradient") {
			width = if (is.na(width)) {
				get_legend_option(item.width, "gradient") * nitems * absolute_fontsize
			} else {
				if (width < 0) {
					-width
				} else {
					width * absolute_fontsize
				}
			}
		}
	})
}

