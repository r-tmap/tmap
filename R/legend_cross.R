# Cross legends (tm_legend_cross): a grid of the legend items of two map variables of the same layer, e.g.
# pattern (rows) and fill (columns). The grid is drawn as a bivariate legend.

message_legend_cross = function(var, cross, reason) {
	cli::cli_inform(c("{.field [tm_legend_cross]} the legend of {.arg {var}} is not crossed with {.arg {cross}}: {reason}.",
					  i = "Separate legends are shown instead."),
					.frequency = "regularly", .frequency_id = paste0("legend_cross_", reason))
}

# Attach the legends of the crossed map variables to the cross legends (legs: list of legend data.tables per map
# variable of one layer). The legends of the crossed map variables are not shown.
legend_cross_attach = function(legs) {
	nms = names(legs)
	for (i in seq_along(legs)) {
		cross = legs[[i]]$legend[[1]][["cross"]]
		if (is.null(cross)) next
		var = nms[i]
		ok = cross %in% nms && cross != var
		if (ok) {
			lt = legs[[cross]]$legend
			ok = length(lt) == length(legs[[i]]$legend) &&
				all(vapply(lt, function(l) isTRUE(l$active), logical(1)))
		}
		if (!ok) {
			message_legend_cross(var, cross, "it is not a data-driven map variable of the same layer")
			next
		}
		is_cont = function(ls) any(vapply(ls, function(l) identical(l$scale, "continuous"), logical(1)))
		if (is_cont(lt) || is_cont(legs[[i]]$legend)) {
			message_legend_cross(var, cross, "continuous scales are not supported (use e.g. tm_scale_intervals())")
			next
		}
		legs[[i]]$legend = mapply(function(l, x) {
			l$cross_legend = x
			l
		}, legs[[i]]$legend, lt, SIMPLIFY = FALSE)
		legs[[cross]]$legend = lapply(lt, function(x) {
			x$show = FALSE
			x
		})
	}
	legs
}

# Create the cross legend from the legend leg of the map variable legnm (rows) with its attached legend of the
# crossed map variable (columns). gp contains the visual values of the legend of legnm, and gp_layer the layer
# specification of the visual values (e.g. "__fill"). The visual values of the grid cells are stored in cross_gp.
legend_cross = function(leg, gp, gp_layer, legnm) {
	cl = leg$cross_legend
	leg$cross_legend = NULL
	cross = leg[["cross"]]

	m = leg$nitems - isTRUE(leg$na.show)
	n = cl$nitems - isTRUE(cl$na.show)

	gpl = unlist(gp_layer, use.names = FALSE)
	ids_leg = which(paste0("__", legnm) == gpl)
	ids_cross = which(paste0("__", cross) == gpl)

	# cells: per column (crossed map variable), the rows from bottom to top (as in bivariate legends), where the
	# first item of the map variable is shown in the top row
	gp2 = gp
	for (j in ids_leg) gp2[[j]] = rep(rev(leg$vvalues[seq_len(m)]), times = n)
	for (j in ids_cross) gp2[[j]] = rep(cl$vvalues[seq_len(n)], each = m)
	gp2$fill = rep_len(gp2$fill, n * m)
	attr(gp2$fill, "biv") = TRUE
	attr(gp2$fill, "m") = m
	attr(gp2$fill, "n") = n

	empty = function(x) is.null(x) || all(is.na(x)) || identical(x, "")
	leg$labels = c(rev(leg$labels[seq_len(m)]), cl$labels[seq_len(n)])
	leg$nitems = m
	leg$na.show = FALSE
	leg$xlab = if (empty(leg$cross_xlab)) cl$title else leg$cross_xlab
	leg$ylab = if (empty(leg$cross_ylab)) leg$title else leg$cross_ylab
	leg$title = if (empty(leg$cross_title)) "" else leg$cross_title
	leg$cross_gp = gp2
	leg$is_cross = TRUE
	leg
}

# Should the column labels of a cross legend be rotated (when they do not fit in the columns)?
legend_cross_rotate = function(comp, textS, o) {
	m = attr(comp$gp$fill, "m")
	n = attr(comp$gp$fill, "n")
	colLabels = comp$labels[(m + 1L):(m + n)]
	w = graphics::strwidth(colLabels, units = "inch", cex = textS, family = comp$text.fontfamily,
						   font = fontface2nr(comp$text.fontface))
	any(w > get_legend_option(comp$item.width, "bivariate") * textS * o$lin)
}

# Cross legend in view mode: an html table with the swatches (PNG images) of the cells, the row labels on the left,
# and the (rotated) column labels below
leaflet_cross_legend_html = function(cmp, o) {
	gp = cmp$gp
	m = attr(gp$fill, "m")
	n = attr(gp$fill, "n")
	k = n * m
	gpvars = c("fill", "fill_alpha", "col", "col_alpha", "lwd", "pattern", "pattern_col", "pattern_size",
			   "pattern_density")
	g = lapply(gp[gpvars], function(x) rep_len(if (is.null(x) || all(is.na(x))) NA else x, k))
	px = 24
	img = vapply(seq_len(k), function(i) {
		file = pattern_swatch_png(g$pattern[i], density = g$pattern_density[i], size = g$pattern_size[i],
						   col = g$pattern_col[i], fill = g$fill[i], fill_alpha = g$fill_alpha[i],
						   border = g$col[i], border_alpha = g$col_alpha[i], lwd = g$lwd[i], scale = o$scale, px = px)
		# the image is embedded as data uri
		paste0("data:image/png;base64,", base64enc::base64encode(file))
	}, character(1))
	esc = function(x) htmltools::htmlEscape(as.character(x))
	labs = cmp$labels
	rowLabs = labs[seq_len(m)]
	colLabs = labs[m + seq_len(n)]
	# rows from top (r = m) to bottom (r = 1); cell index: (j - 1) * m + r
	rows = vapply(m:1, function(r) {
		cells = vapply(seq_len(n), function(j) {
			paste0('<td style="padding:0;"><img src="', img[(j - 1) * m + r], '" width="', px, '" height="', px,
				   '" style="display:block;"></td>')
		}, character(1))
		paste0('<tr><td style="padding:0 6px 0 0; text-align:left; white-space:nowrap;">', esc(rowLabs[r]),
			   '</td>', paste(cells, collapse = ""), '</tr>')
	}, character(1))
	colRow = paste0('<tr><td></td>', paste0('<td style="padding:2px 0 0 0; vertical-align:top;">',
		'<div style="writing-mode:vertical-rl; transform:rotate(180deg); white-space:nowrap; margin:auto;">',
		esc(colLabs), '</div></td>', collapse = ""), '</tr>')
	title = if (nonempty_text(cmp$title)) paste0('<div style="font-size:14px; margin-bottom:5px;">',
												 esc(cmp$title), '</div>') else ""
	ylab = if (nonempty_text(cmp$ylab)) paste0('<tr><td style="padding:0 6px 2px 0; text-align:left;"><em>',
											   esc(cmp$ylab), '</em></td><td colspan="', n, '"></td></tr>') else ""
	xlab = if (nonempty_text(cmp$xlab)) paste0('<tr><td></td><td colspan="', n,
											   '" style="text-align:center; padding-top:2px;"><em>', esc(cmp$xlab),
											   '</em></td></tr>') else ""
	paste0(title, '<table style="border-collapse:collapse; font-size:12px;">', ylab, paste(rows, collapse = ""),
		   colRow, xlab, '</table>')
}
