# View mode legend for tm_scale_intervals(label.style = "continuous"): one solid block per class
# (equal size, as in plot mode) with the breaks labeled at the block boundaries. Drawn as one SVG image
# (leaflegend::addLegendImage) so that it behaves like the other legends (group toggling, title).
leaflet_intervals_legend = function(cmp, lf, title, group, position, orientation, layerId, className) {
	x = legend_intervals_svg(cmp, orientation)
	img = structure(paste0("data:image/svg+xml;base64,", base64enc::base64encode(charToRaw(enc2utf8(x$svg)))),
					class = "svgURI")

	leaflegend::addLegendImage(lf, images = img, labels = "", width = x$width, height = x$height,
							   title = title, position = position, group = group,
							   orientation = "vertical", labelStyle = "",
							   layerId = layerId, className = className)
}

# SVG of a legend for tm_scale_intervals(label.style = "continuous"), used by the view mode (above) and by
# extension packages (tmap.mapgl). orientation: "vertical" (portrait) or "horizontal" (landscape).
# Returns a list with the svg code (character), width and height (px).
legend_intervals_svg = function(cmp, orientation = "vertical", fs = 13,
								font = "'Helvetica Neue',Arial,Helvetica,sans-serif") {
	vary = if ("fill" %in% cmp$varying) "fill" else "col"
	vals = unname(cmp$gp[[vary]])
	na.show = isTRUE(cmp$na.show)
	labs = as.character(cmp$labels)
	sel = cmp$labels_select

	if (na.show) {
		colNA = vals[length(vals)]
		vals = head(vals, -1)
		labNA = trimws(labs[length(labs)])
		labs = head(labs, -1)
		sel = head(sel, -1)
	}
	# each value is a "_"-separated list of the colours on both sides of a break; the class colours are
	# the last colours of the first k values (k = number of classes)
	k = length(labs) - 1L
	cols = vapply(strsplit(vals[seq_len(k)], "_", fixed = TRUE), function(x) x[length(x)], character(1))
	labs[!rep_len(sel, length(labs))] = ""

	alpha = cmp$gp[[paste0(vary, "_alpha")]]
	alpha = if (is.null(alpha) || is.na(alpha[1])) 1 else alpha[1]

	tl = 4               # tick length
	gap = 3              # space between ticks and labels
	tw = function(x) nchar(x) * fs * 0.6  # estimated text width
	esc = function(x) htmltools::htmlEscape(x)
	font = paste0("font-family:", font)

	vertical = (orientation == "vertical")
	len = unname(if (vertical) cmp$height else cmp$width)
	thick = unname(if (vertical) cmp$width else cmp$height)
	if (!vertical) {
		# landscape: bar length from width; height is usually small
		thick = min(thick, 20)
		len = max(len, length(labs) * max(tw(labs), 20) * 1.2)
	}
	step = len / k
	pos = (0:k) * step

	if (vertical) {
		pad = ceiling(fs / 2) + 1     # room for the top and bottom labels
		x0 = 1
		W = x0 + thick + tl + gap + max(tw(c(labs, if (na.show) labNA)), 0) + 2
		blocks = sprintf('<rect x="%.1f" y="%.2f" width="%.1f" height="%.2f" fill="%s" fill-opacity="%s"/>',
						 x0, pad + pos[-(k + 1)], thick, step + 0.5 * (seq_len(k) < k), cols, alpha)
		frame = sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" fill="none" stroke="#404040" stroke-width="0.5"/>',
						x0, pad, thick, len)
		ticks = sprintf('<line x1="%.1f" x2="%.1f" y1="%.2f" y2="%.2f" stroke="black" stroke-width="1"/>',
						x0 + thick, x0 + thick + tl, pad + pos, pad + pos)
		texts = sprintf('<text x="%.1f" y="%.2f" dominant-baseline="central" font-size="%s" style="%s">%s</text>',
						x0 + thick + tl + gap, pad + pos, fs, font, esc(labs))
		H = len + 2 * pad
		if (na.show) {
			yNA = H + 4
			nas = paste0(sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" fill="%s" fill-opacity="%s" stroke="#404040" stroke-width="0.5"/>',
								 x0, yNA, thick, thick, colNA, alpha),
						 sprintf('<text x="%.1f" y="%.1f" dominant-baseline="central" font-size="%s" style="%s">%s</text>',
						 		x0 + thick + tl + gap, yNA + thick / 2, fs, font, esc(labNA)))
			H = yNA + thick + 2
		} else nas = ""
	} else {
		padL = ceiling(max(tw(labs[1]) / 2, 1)) + 1
		padR = ceiling(max(tw(labs[k + 1]) / 2, 1)) + 1
		y0 = 1
		blocks = sprintf('<rect x="%.2f" y="%.1f" width="%.2f" height="%.1f" fill="%s" fill-opacity="%s"/>',
						 padL + pos[-(k + 1)], y0, step + 0.5 * (seq_len(k) < k), thick, cols, alpha)
		frame = sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" fill="none" stroke="#404040" stroke-width="0.5"/>',
						padL, y0, len, thick)
		ticks = sprintf('<line x1="%.2f" x2="%.2f" y1="%.1f" y2="%.1f" stroke="black" stroke-width="1"/>',
						padL + pos, padL + pos, y0 + thick, y0 + thick + tl)
		texts = sprintf('<text x="%.2f" y="%.1f" text-anchor="middle" dominant-baseline="hanging" font-size="%s" style="%s">%s</text>',
						padL + pos, y0 + thick + tl + gap, fs, font, esc(labs))
		W = padL + len + padR
		if (na.show) {
			xNA = W + 10
			nas = paste0(sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" fill="%s" fill-opacity="%s" stroke="#404040" stroke-width="0.5"/>',
								 xNA, y0, thick, thick, colNA, alpha),
						 sprintf('<text x="%.2f" y="%.1f" dominant-baseline="hanging" font-size="%s" style="%s">%s</text>',
						 		xNA, y0 + thick + tl + gap, fs, font, esc(labNA)))
			W = xNA + max(thick, tw(labNA)) + 2
		} else nas = ""
		H = y0 + thick + tl + gap + fs + 3
	}
	W = ceiling(W)
	H = ceiling(H)

	svg = paste0('<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="', W, '" height="', H,
				 '" viewBox="0 0 ', W, ' ', H, '">',
				 paste(blocks, collapse = ""), frame, paste(ticks, collapse = ""), paste(texts, collapse = ""), nas,
				 '</svg>')
	list(svg = svg, width = W, height = H)
}
