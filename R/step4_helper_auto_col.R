# Resolve color "auto" values in step 4
resolve_auto_col = function(tmx, o) {
	tmx = resolve_auto_text_col(tmx, o)
	resolve_auto_pattern_col(tmx, o)
}

# Resolve text color "auto" (tm_text(col = "auto")): black or white, depending on
# the color underneath each label. That is the fill of the topmost polygon layer
# below the text layer in the same group (matched by feature id), blended with the
# map background. For labels with a background (bgcol), that background is used.
resolve_auto_text_col = function(tmx, o) {
	bg = if (isTRUE(o$bg) && valid_colors(o$bg.color)) o$bg.color else "white"
	bynames = paste0("by", 1:3, "__")

	lapply(tmx, function(tmxi) {
		lys = tmxi$layers
		for (i in seq_along(lys)) {
			tml = lys[[i]]
			dt = tml$mapping_dt
			if (!("tm_data_text" %in% tml$mapping_fun) || !("col" %in% names(dt)) || !any(dt$col == "auto", na.rm = TRUE)) next

			# topmost polygon layer below the text layer
			below = vapply(lys, function(l) {
				("tm_data_polygons" %in% l$mapping_fun) && "fill" %in% names(l$mapping_dt) && as.numeric(l$lid) < as.numeric(tml$lid)
			}, FUN.VALUE = logical(1))
			pid = if (any(below)) which(below)[which.max(vapply(lys[below], function(l) as.numeric(l$lid), numeric(1)))] else NA

			if (!is.na(pid)) {
				pdt = lys[[pid]]$mapping_dt
				pby = intersect(names(pdt), bynames)
				pdt = pdt[, c("tmapID__", pby, "fill", intersect(names(pdt), "fill_alpha")), with = FALSE]
				data.table::setnames(pdt, "fill", "fill__auto")
				if (!("fill_alpha" %in% names(pdt))) pdt[, "fill_alpha" := 1]
				data.table::setnames(pdt, "fill_alpha", "fill_alpha__auto")

				# join on feature id and shared facet columns; facets of the polygon layer
				# that the text layer does not have are added to the text layer
				jby = c("tmapID__", intersect(pby, names(dt)))
				pdt = unique(pdt, by = c(jby, setdiff(pby, names(dt))))
				dt = merge(dt, pdt, by = jby, all.x = TRUE, sort = FALSE, allow.cartesian = TRUE)
			} else {
				dt[, c("fill__auto", "fill_alpha__auto") := list(NA_character_, 1)]
			}

			ida = which(dt$col == "auto")
			bg_rgb = matrix(col2rgb(bg), nrow = 3, ncol = length(ida))
			f = dt$fill__auto[ida]
			f[is.na(f)] = "#00000000"
			under = blend_rgb(f, dt$fill_alpha__auto[ida], bg_rgb)

			if ("bgcol" %in% names(dt)) {
				balpha = if ("bgcol_alpha" %in% names(dt)) dt$bgcol_alpha[ida] else 1
				bgc = dt$bgcol[ida]
				bgc[is.na(bgc)] = "#00000000"
				under = blend_rgb(bgc, balpha, under)
			}

			dt[ida, "col" := contrast_text_col(under)]
			dt[, c("fill__auto", "fill_alpha__auto") := NULL]

			lys[[i]]$mapping_dt = dt
		}
		tmxi$layers = lys
		tmxi
	})
}

# alpha compositing of colors x (with alpha) over y (opaque rgb matrix, 3 x n); returns rgb matrix
blend_rgb = function(x, alpha, y) {
	xr = col2rgb(x, alpha = TRUE)
	a = rep_len(alpha, ncol(xr)) * xr[4, ] / 255
	a[is.na(a)] = 0
	t(t(xr[1:3, , drop = FALSE]) * a + t(y) * (1 - a))
}

# black text on light backgrounds, white text on dark ones (rgb matrix input)
contrast_text_col = function(m) {
	ifelse(colSums(m * c(.299, .587, .114)) >= 128, "#000000", "#FFFFFF")
}

# Resolve pattern color "auto" (tm_polygons(pattern_col = "auto")): the pattern color
# (dark) or white, whichever contrasts most with the fill of the same feature
# (blended with the map background)
resolve_auto_pattern_col = function(tmx, o) {
	bg = if (isTRUE(o$bg) && valid_colors(o$bg.color)) o$bg.color else "white"
	inks = auto_pattern_inks(o)

	lapply(tmx, function(tmxi) {
		tmxi$layers = lapply(tmxi$layers, function(tml) {
			dt = tml$mapping_dt
			if (!all(c("pattern_col", "fill") %in% names(dt)) || !any(dt$pattern_col == "auto", na.rm = TRUE)) return(tml)

			ida = which(dt$pattern_col == "auto")
			f = dt$fill[ida]
			f[is.na(f)] = "#00000000"
			falpha = if ("fill_alpha" %in% names(dt)) dt$fill_alpha[ida] else 1
			under = blend_rgb(f, falpha, matrix(col2rgb(bg), nrow = 3, ncol = length(ida)))

			dt[ida, "pattern_col" := contrast_col(under, dark = inks[1], light = inks[2])]
			tml$mapping_dt = dt
			tml
		})
		tmxi
	})
}

# dark and light ink for pattern_col = "auto": the default pattern color (if dark) and white
auto_pattern_inks = function(o) {
	dark = getAesOption("value.const", o, aes = "pattern_col", layer = "polygons")
	if (!is.character(dark) || length(dark) != 1L || !valid_colors(dark) || is_light(dark)) dark = "grey25"
	inks = c(dark, "#FFFFFF")
	if (is.list(o$pc)) do.call(process_color, c(list(col = inks), o$pc)) else inks
}

# alpha compositing of colors x (with alpha) over y (opaque rgb matrix, 3 x n); returns rgb matrix
blend_rgb = function(x, alpha, y) {
	xr = col2rgb(x, alpha = TRUE)
	a = rep_len(alpha, ncol(xr)) * xr[4, ] / 255
	a[is.na(a)] = 0
	t(t(xr[1:3, , drop = FALSE]) * a + t(y) * (1 - a))
}

# black text on light backgrounds, white text on dark ones (rgb matrix input)
contrast_text_col = function(m) {
	ifelse(colSums(m * c(.299, .587, .114)) >= 128, "#000000", "#FFFFFF")
}

# dark or light color, whichever differs most in luminance from the background (rgb matrix input)
contrast_col = function(m, dark, light) {
	lum = function(x) colSums(x * c(.299, .587, .114))
	l = lum(m)
	ifelse(abs(l - lum(col2rgb(dark))) >= abs(l - lum(col2rgb(light))), dark, light)
}

# legend items: text color "auto" contrasts with the item background (bgcol) or the legend
# background; pattern color "auto" contrasts with the item fill
legend_auto_col = function(gp, o) {
	is_auto = function(x) is.character(x) && any(x == "auto", na.rm = TRUE)
	if (!is_auto(gp$col) && !is_auto(gp$pattern_col)) return(gp)

	bg = if (is.null(o)) "white" else if (isTRUE(o$legend.bg) && valid_colors(o$legend.bg.color)) o$legend.bg.color else if (valid_colors(o$bg.color)) o$bg.color else "white"
	n = max(length(gp$col), length(gp$bgcol), length(gp$pattern_col), length(gp$fill))
	bg_rgb = matrix(col2rgb(bg), nrow = 3, ncol = n)

	if (is_auto(gp$col)) {
		under = bg_rgb
		if (is.character(gp$bgcol) && all(valid_colors(gp$bgcol))) {
			balpha = if (is.numeric(gp$bgcol_alpha)) gp$bgcol_alpha else 1
			under = blend_rgb(rep_len(gp$bgcol, n), balpha, under)
		}
		col = rep_len(gp$col, n)
		ida = which(col == "auto")
		col[ida] = contrast_text_col(under[, ida, drop = FALSE])
		gp$col = col
	}

	if (is_auto(gp$pattern_col)) {
		under = bg_rgb
		if (is.character(gp$fill) && all(valid_colors(gp$fill))) {
			falpha = if (is.numeric(gp$fill_alpha)) gp$fill_alpha else 1
			under = blend_rgb(rep_len(gp$fill, n), falpha, under)
		}
		inks = if (is.null(o)) c("grey25", "#FFFFFF") else auto_pattern_inks(o)
		pcol = rep_len(gp$pattern_col, n)
		ida = which(pcol == "auto")
		pcol[ida] = contrast_col(under[, ida, drop = FALSE], dark = inks[1], light = inks[2])
		gp$pattern_col = pcol
	}
	gp
}
