#' @export
#' @rdname tmap_internal
tmapChartBinned = function(chart, bin_colors, breaks_def, na.show, x1) {
	if (is.numeric(x1)) {
		tmapChartBinned_numeric(chart, bin_colors, breaks_def, na.show, x1)
	} else {
		tmapChartBinned_categorical(chart, bin_colors, breaks_def, na.show, x1)
	}
}

#' @export
#' @rdname tmap_internal
tmapChartBinned2d = function(chart, chart1, chart2) {
	if (is.numeric(chart1$x1) && is.numeric(chart2$x1)) {
		tmapChartBinned2d_numnum(chart, chart1, chart2)
	} else if (is.numeric(chart1$x1) && !is.numeric(chart2$x1)) {
		tmapChartBinned2d_numcat(chart, chart1, chart2)
	} else if (!is.numeric(chart1$x1) && is.numeric(chart2$x1)) {
		res = tmapChartBinned2d_numcat(chart, chart2, chart1)
		names(res$tab) = names(res$tab)[c(2,1,3)]
		res
	} else {
		tmapChartBinned2d_catcat(chart, chart1, chart2)
	}
}

#' @export
#' @rdname tmap_internal
tmapChartBinned2d_numcat = function(chart, chart1, chart2) {
	res = bin_num(chart1$x1, chart1$breaks_def, chart1)
	chart$tab = as.data.frame(table(bin1 = res$xcat, bin2 = chart2$x1, useNA = "no"), responseName = "freq")
	chart$pal = NA
	chart$datatype = "categorized"
	chart$predefined = FALSE
	chart
}

#' @export
#' @rdname tmap_internal
tmapChartBinned2d_numnum = function(chart, chart1, chart2) {
	res1 = bin_num(chart1$x1, chart1$breaks_def, chart1)
	res2 = bin_num(chart2$x1, chart2$breaks_def, chart2)
	chart$tab = as.data.frame(table(bin1 = res1$xcat, bin2 = res2$xcat, useNA = "no"), responseName = "freq")
	chart$pal = NA
	chart$datatype = "categorized"
	chart$predefined = FALSE
	chart
}


#' @export
#' @rdname tmap_internal
tmapChartBinned2d_catcat = function(chart, chart1, chart2) {
	chart$tab = as.data.frame(table(bin1 = chart1$x1, bin2 = chart2$x1, useNA = "no"), responseName = "freq")
	chart$pal = NA
	chart$datatype = "categorized"
	chart$predefined = FALSE
	chart
}


#' @export
#' @rdname tmap_internal
tmapChartRaw = function(chart, na.show, x1, ...) {
	if (!na.show) x1 = na.omit(x1)

	chart$df = data.frame(x = 1L, y = x1)
	chart$datatype = "raw"
	chart$predefined = FALSE
	chart
}

#' @export
#' @rdname tmap_internal
tmapChartRaw_nna = function(chart, na.show, x1, ...) {
	x1 = na.omit(x1)

	chart$df = data.frame(x = 1L, y = x1)
	chart$datatype = "raw"
	chart$predefined = FALSE
	chart
}

#' @export
#' @rdname tmap_internal
tmapChartNone = function(chart, na.show, x1, ...) {
	chart
}

#' @export
#' @rdname tmap_internal
tmapChartPass = function(chart, na.show, x1, ...) {
	args = c(list(na.show = na.show, x1 = x1), list(...))
	chart[names(args)] = args
	chart
}

#' @export
#' @rdname tmap_internal
tmapChartBinned_categorical = function(chart, bin_colors, breaks_def, na.show, x1) {
	if (chart$type == "histogram") {
		message("histograms are supposed to be used for numerical data, a bar chart will be shown instead (tm_chart_bar)")
	}

	if (na.show) {
		tab = as.data.frame(table(bin = x1, useNA = "always"), responseName = "freq")
		tab$color = factor(1L:nrow(tab))
		pal = structure(bin_colors, names = levels(tab$color))
	} else {
		tab = as.data.frame(table(bin = x1, useNA = "no"), responseName = "freq")
		tab$color = factor(1L:nrow(tab))
		pal = structure(bin_colors, names = levels(tab$color))
	}
	chart$tab = tab
	chart$pal = pal
	chart$datatype = "categorized"
	chart$predefined = TRUE
	chart
}


bin_num = function(x1, breaks_def, chart) {
	# are breaks (and bin_colors)
	predefined = !is.null(breaks_def)

	if (is.null(chart$breaks_def)) {
		if (!predefined) {
			breaks = pretty(x1)
			ids = rep(1L, length(breaks) - 1)

		} else {
			breaks = breaks_def
			ids = 1L:(length(breaks) - 1L)
		}
	} else {
		breaks = chart$breaks_def
		subbreaks = (all(breaks_def %in% breaks))

		break_mids = (breaks[-1] + head(breaks, -1)) / 2

		if (predefined) {
			ids = as.integer(cut(break_mids, breaks_def, include.lowest = TRUE, right = FALSE))
		} else {
			ids = rep(1L, length(breaks) - 1)
		}

	}
	xcat = cut(x1, breaks = breaks, include.lowest = TRUE, right = FALSE)
	list(xcat = xcat, ids = ids)
}


#' @export
#' @rdname tmap_internal
tmapChartBinned_numeric = function(chart, bin_colors, breaks_def, na.show, x1) {

	# are breaks (and bin_colors)
	predefined = !is.null(breaks_def)

	res = bin_num(x1, breaks_def, chart)
	xcat = res$xcat
	ids = res$ids

	if (!predefined) {
		bin_colors = chart$object.color
	}

	if (na.show) {
		tab = as.data.frame(table(bin=xcat, useNA = "always"), responseName = "freq")
		tab$color = factor(c(ids, length(bin_colors)), levels = seq_along(bin_colors))
		pal = structure(bin_colors, names = levels(tab$color))
	} else {
		tab = as.data.frame(table(bin=xcat, useNA = "no"), responseName = "freq")
		tab$color = factor(ids, levels = seq_along(bin_colors))
		pal = structure(bin_colors, names = levels(tab$color))
	}
	chart$tab = tab
	chart$pal = pal
	chart$datatype = "binned"
	chart$predefined = predefined
	chart
}
