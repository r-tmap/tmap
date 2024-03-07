tmapChartBinned = function(chart, bin_colors, breaks_def, na.show, x1) {
	if (is.numeric(x1)) {
		tmapChartBinned_numeric(chart, bin_colors, breaks_def, na.show, x1)	
	} else {
		tmapChartBinned_categorical(chart, bin_colors, breaks_def, na.show, x1)	
	}
}


tmapChartRaw = function(chart, na.show, x1, ...) {
	if (!na.show) x1 = na.omit(x1)
	
	chart$df = data.frame(x = 1L, y = x1)
	chart$datatype = "raw"
	chart$predefined = FALSE
	chart
}

tmapChartRaw_nna = function(chart, na.show, x1, ...) {
	x1 = na.omit(x1)
	
	chart$df = data.frame(x = 1L, y = x1)
	chart$datatype = "raw"
	chart$predefined = FALSE
	chart
}

tmapChartNone = function(chart, na.show, x1, ...) {
	chart
}

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

tmapChartBinned_numeric = function(chart, bin_colors, breaks_def, na.show, x1) {
	
	
	
	
	# are breaks (and bin_colors)
	predefined = !is.null(breaks_def)
	
	if (!predefined) {
		bin_colors = chart$object.color
	}
	df = data.frame(x = x1)
	
	
	
	if (is.null(chart$breaks)) {
		if (!predefined) {
			breaks = pretty(x1)
			ids = 1L
			
		} else {
			breaks = breaks_def
			ids = 1L:(length(breaks) - 1L)
		}
	} else {
		breaks = chart$breaks
		subbreaks = (all(breaks_def %in% breaks))
		
		break_mids = (breaks[-1] + head(breaks, -1)) / 2
		
		if (!is.null(breaks_def)) {
			ids = as.integer(cut(break_mids, breaks_def, include.lowest = TRUE, right = FALSE))
		} else {
			ids = 1
		}
		
	}
	
	df$xcat = cut(df$x, breaks = breaks, include.lowest = TRUE, right = FALSE)
	
	if (na.show) {
		tab = as.data.frame(table(bin=df$xcat, useNA = "always"), responseName = "freq")
		tab$color = factor(c(ids, length(bin_colors)), levels = seq_along(bin_colors))
		pal = structure(bin_colors, names = levels(tab$color))
	} else {
		tab = as.data.frame(table(bin=df$xcat, useNA = "no"), responseName = "freq")
		tab$color = factor(ids, levels = seq_along(bin_colors))
		pal = structure(bin_colors, names = levels(tab$color))
	}
	chart$tab = tab
	chart$pal = pal
	chart$datatype = "binned"
	chart$predefined = predefined
	chart
}
