submit_labels = function(labels, cls, pane, group) {
	layerIds = get("layerIds", envir = .TMAP_LEAFLET)

	if (length(layerIds)) {
		labels = local({
			labels_all = unlist(lapply(layerIds, function(l) l$Lid), use.names = FALSE)
			pos <- length(labels_all)
			labels_all = gsub("_", ".", labels_all,fixed = TRUE)
			labels_all = make.names(c(labels_all, labels), unique = TRUE)
			labels_all = gsub(".", "_", labels_all,fixed = TRUE)
			labels_all[(pos + 1): length(labels_all)]
		})
	} else {
		labels = make.names(labels, unique = TRUE)
		labels = gsub(".", "_", labels,fixed = TRUE)
	}

	layerIds = c(layerIds, list(list(name = pane, type = cls, group = group, Lid = labels)))

	assign("layerIds", layerIds, envir = .TMAP_LEAFLET)
	labels
}

impute_webgl = function(use_WebGL, dt, supported, checkif = NULL, type, hover, popup, crs_class) {
	if (!identical(use_WebGL, FALSE)) {
		if (crs_class == "L.CRS.Simple") {
			message_webgl_crs_simple()
			use_WebGL = FALSE
		} else {

			vary = vapply(dt, function(x) {
				any(is.na(x)) || any(x!=x[1])
			}, FUN.VALUE = logical(1))

			vary = vary[setdiff(names(vary), c(supported, "tmapID__", "ord__"))]

			if (!is.null(checkif)) {
				checks = vapply(seq_along(checkif), function(i) {
					if (!(checkif[i] %in% names(dt))) {
						TRUE
					} else {
						dt[[names(checkif)[i]]][1] %in% checkif[[i]]
					}
				}, FUN.VALUE = logical(1))
			} else {
				checks = TRUE
			}

			if (any(vary)) {
				if (is.na(use_WebGL)) {
					use_WebGL = FALSE
				} else {
					message_webgl_vars(supported, vary)
				}
			} else if (!all(checks)) {
				if (is.na(use_WebGL)) {
					use_WebGL = FALSE
				} else {
					checkif = lapply(checkif, function(x) {
						if (is.character(x)) paste0("\"", x, "\"") else x
					})

					checkif = lapply(checkif, function(x) {
						if (length(x) == 1) {
							x
						} else {
							k = length(x)
							paste(paste(head(x, -1), collapse = ", "), tail(x, 1), sep = " or ")
						}
					})

					message_webgl_checks(checks, checkif)

				}
			} else if (type != "symbols" && hover) {
				if (!is.na(use_WebGL)) {
					message_webgl_hover(type)
				} else {
					use_WebGL = FALSE
				}
			} else if ((is.na(use_WebGL))) {
				n = nrow(dt)
				use_WebGL = (n >= 1000)
			}
		}
	}
	use_WebGL
}


expand_coords_gp = function(coords, gp, ndt) {
	expanded = (ncol(coords) == 3L)
	if  (expanded) {
		gp = lapply(gp, function(gpi) {
			if (is.list(gpi)) {
				unlist(gpi)
			} else if (length(gpi) == ndt) {
				gpi[coords[,3L]]
			} else {
				gpi
			}
		})
		coords = coords[, 1:2, drop=FALSE]
	}
	list(coords = coords, gp = gp, expanded = expanded)
}

#' @export
#' @keywords internal
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	UseMethod("tmapLeafletDataPlot")
}

#' @export
#' @keywords internal
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.default = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	NULL
}


lty2dash = function(lty) {
	tab = c(solid = "", dashed = "4 4", dotted = "1 3", dotdash = "1 3 4 3", longdash = "7 3", twodash = "2 2 6 2")
	are_words = (lty %in% names(tab))
	if (all(are_words)) {
		unname(tab[lty])
	} else {
		are_letters = (suppressWarnings(!is.na(as.numeric(lty))))

		if (!all(are_letters | are_words)) {
			stop("Incorrect lty specification: ", lty[which(!are_letters & !are_words)[1]])
		} else {
			lty[are_words] = unname(tab[lty[are_words]])
			lty[are_letters] = vapply(strsplit(lty[are_letters], ""), FUN = function(x) paste(x, collapse = " "), FUN.VALUE = character(1))
		}
		lty
	}

}

makeSymbolIcons2  = function (shape, color, fillColor = color, opacity, fillOpacity = opacity,
		  strokeWidth = 1, width, height = width, ...)
{
	symbols <- Map(leaflegend::makeSymbol, shape = shape, width = width,
				   height = height, color = color, fillColor = fillColor,
				   opacity = opacity, fillOpacity = fillOpacity, `stroke-width` = strokeWidth,
				   ...)
	leaflet::icons(iconUrl = unname(symbols), iconAnchorX = width/2 + strokeWidth,
				   iconAnchorY = height/2 + strokeWidth)
}


split_alpha_channel <- function(x, alpha) {
	if (is.null(x)) {
		list(col=NULL, opacity=0)
	} else {
		RGBA <- col2rgb(x, alpha = TRUE)
		col <- rgb(RGBA[1,], RGBA[2,], RGBA[3,], maxColorValue = 255)
		opacity <- unname(RGBA[4,]/255 * alpha)
		list(col=col, opacity=opacity)
	}
}

