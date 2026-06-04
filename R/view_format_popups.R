view_format_popups = function(id = NULL, titles, format, values, layout = NULL) {

	layout = complete_popup_layout(layout)

	pop_vars = intersect(names(format), names(values))
	format = format[pop_vars]

	h = lapply(format, function(f) {
		if (f$html.escape) {
			htmltools::htmlEscape
		} else {
			function(x) x
		}
	})

	if (!is.null(id)) {
		labels = paste0("<b>", id, "</b>")
	} else {
		labels = ""
	}

	titles_format = mapply(function(ti, hi) {
		hi(ti)
	}, titles, h, SIMPLIFY = FALSE)

	values_format = mapply(function(v, f, hi) {
		if (inherits(v, "units")) {
			popup_append = paste0(" ", as.character(attr(v, "units")))
		} else {
			popup_append = ""
		}
		numbers = hi(if (is.numeric(v)) do.call("fancy_breaks", c(list(vec = as.numeric(v), intervals = FALSE), f)) else v)
		paste0(numbers, popup_append)
	}, values, format, h, SIMPLIFY = FALSE)

	# Per-region styling (alignment + color), shared with the maplibre backend.
	td_label = paste0("<td class=\"tmap-popup-label\"", .popup_style_attr(layout$label.align, layout$label.color), "><nobr>")
	td_value = paste0("<td class=\"tmap-popup-value\"", .popup_style_attr(layout$value.align, layout$value.color), "><nobr>")
	th_attr  = .popup_style_attr(layout$title.align, layout$title.color)

	labels2 = mapply(function(l, v) {
		paste0("<tr>", td_label, l, "</nobr></td>", td_value, v, "</nobr></td>")
	}, titles_format, values_format, SIMPLIFY = FALSE)

	labels3 = paste0(do.call("paste", c(labels2, list(sep = "</tr>"))), "</tr>")

	padding_right = ifelse(length(titles_format) > 13, 15, 0) # add padding for horizontal scroll bar. These will appear on most browsers when there are over 13 normal lines (tested: RStudio, FF, Chrome)

	# Scrolling is controlled solely by max.height: a length caps the height and
	# the table scrolls past it; "none" means no cap, so the popup grows to fit
	# and (with overflow-y:auto) never shows a scrollbar. max-height and overflow
	# are kept on the SAME (inner) element so they act together.
	inner_style = paste0("max-height:", layout$max.height, ";overflow-y:auto;overflow-x:hidden;padding-right:", padding_right, "px;")

	# When a fixed width is set, let the table fill it so the extra space falls
	# between the label and value columns (value column right-aligned to the
	# popup's right edge) rather than as blank space to the right of the table.
	table_style = if (identical(layout$width, "auto")) "" else " style=\"width:100%;\""

	# Free-form user CSS (tm_popup(css=)) injected after the width rule so it can
	# override anything above. Target classes: .tmap-popup, .tmap-popup-table,
	# .tmap-popup-title, .tmap-popup-label, .tmap-popup-value.
	css_block = if (!is.null(layout$css) && length(layout$css) == 1L && !is.na(layout$css) && nzchar(layout$css)) {
		paste0("<style>", layout$css, "</style>")
	} else {
		""
	}

	paste0(
		"<style> div.leaflet-popup-content {width:", layout$width, " !important;}</style>",
		css_block,
		"<div class=\"tmap-popup\" style=\"", inner_style, "\">",
		"<table class=\"tmap-popup-table\"", table_style, ">",
		"<thead><tr><th class=\"tmap-popup-title\" colspan=\"2\"", th_attr, ">", labels, "</th></tr></thead>",
		labels3,
		"</table></div>"
	)
}
