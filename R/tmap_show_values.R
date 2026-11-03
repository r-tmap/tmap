#' Show the available values of categorical map variables
#'
#' Draws the values of a categorical map variable (other than color), each labeled with the code
#' that specifies it. These codes can be used as constant values (e.g. `tm_symbols(shape = 22)`)
#' and as `values` of a scale (e.g. `tm_scale_categorical(values = c("hatch", "dots"))`). For
#' color palettes, see [cols4all::c4a_gui()].
#'
#' By default, all available values are shown. With `values`, specific values are shown instead,
#' in the same form as the `values` argument of a scale, such as [tm_scale_categorical()]:
#'
#' | `variable` | Available values | Other values |
#' |---|---|---|
#' | `"pattern"` | pattern types and shortcuts (see [tm_patterns()]) | pattern sets: `"mixed"` (the default of scales), `"classic"`, `"lines"`, and `"shapes"`; [tm_patterns()] specifications |
#' | `"shape"` | `0` to `25` (see [graphics::points()]) | |
#' | `"lty"` | `"solid"`, `"dashed"`, `"dotted"`, `"dotdash"`, `"longdash"`, and `"twodash"` | strings of 2, 4, 6, or 8 hexadecimal digits, the lengths of the dashes and gaps (e.g. `"44"`; see [graphics::par()]) |
#' | `"fontface"` | `"plain"`, `"italic"`, `"bold"`, and `"bold.italic"` | |
#'
#' A pattern set is shown as a group of patterns, in the order in which they are used: a scale
#' with `n` categories takes the first `n` patterns of the set. Several sets can be shown at
#' once, e.g. all sets with `values = c("mixed", "classic", "lines", "shapes")`.
#'
#' @param variable the map variable: one of `"pattern"` (see [tm_polygons()]),
#'   `"shape"` (see [tm_symbols()]), `"lty"` (line type, see e.g. [tm_lines()]), and
#'   `"fontface"` (see [tm_text()]).
#' @param values the values to show. By default (`NULL`), all available values. See details.
#' @param col the color of the lines, the borders of symbols, the patterns, and the text.
#' @param fill the fill color of polygons (`"pattern"`) and of the symbols with a fill (shapes 21 to 25).
#' @param ncol number of columns. By default, determined by the number of values.
#' @return The values shown (invisibly), as codes that can be used as `values` of a scale. When several pattern sets are shown, a list with one element per set.
#' @example ./examples/tmap_show_values.R
#' @seealso [tm_patterns()], [tm_symbols()], [tm_lines()], [tm_text()]
#' @export
tmap_show_values = function(variable = c("pattern", "shape", "lty", "fontface"), values = NULL,
							col = "grey20", fill = "grey90", ncol = NA) {
	variable = match.arg(variable)
	all_values = is.null(values)

	# groups of values: list(title, values, labels); only pattern sets have a title
	if (variable == "pattern") {
		if (all_values) values = c(names(.pattern_types), names(.pattern_shortcuts))
		groups = if (is.character(values) && !inherits(values, "tmap_patterns")) {
			is_set = pattern_is_set(values)
			# consecutive values that are not sets form one group
			grp = cumsum(is_set | c(TRUE, is_set[-length(is_set)]))
			lapply(split(values, grp), function(v) {
				if (pattern_is_set(v[1])) {
					labs = pattern_set(v[1])
					list(title = paste0("pattern set \"", v[1], "\""), values = pattern_norm(labs),
						 labels = labs, quote = TRUE)
				} else {
					list(title = NULL, values = pattern_norm(v), labels = v, quote = TRUE)
				}
			})
		} else {
			v = pattern_norm(values)
			list(list(title = NULL, values = v, labels = v, quote = FALSE))
		}
		if (any(is.na(unlist(lapply(groups, "[[", "values"))))) {
			cli::cli_abort("Unknown pattern(s); see {.help tmap::tm_patterns}", call = NULL)
		}
	} else {
		if (variable == "shape") {
			if (all_values) values = 0:25
			if (!is.numeric(values) || !isTRUE(tmapValuesCheck_shape(values, is_var = FALSE))) {
				cli::cli_abort("{.arg values} should be numeric symbol shapes (0 to 25)", call = NULL)
			}
		} else if (variable == "lty") {
			if (all_values) values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
			if (!isTRUE(tmapValuesCheck_lty(values, is_var = FALSE))) {
				cli::cli_abort("{.arg values} should be line types (see {.help graphics::par})", call = NULL)
			}
		} else {
			if (all_values) values = c("plain", "italic", "bold", "bold.italic")
			if (!isTRUE(tmapValuesCheck_fontface(values, is_var = FALSE))) {
				cli::cli_abort("{.arg values} should be font faces (see {.help grid::gpar})", call = NULL)
			}
		}
		groups = list(list(title = NULL, values = values, labels = as.character(values),
						   quote = is.character(values)))
	}

	nmax = max(vapply(groups, function(g) length(g$values), integer(1)))
	if (is.na(ncol)) {
		ncol = switch(variable,
					  pattern = min(nmax, if (length(groups) > 1) 7L else 6L),
					  shape = min(nmax, 13L),
					  lty = 1L,
					  fontface = min(nmax, 4L))
	}

	# rows of the layout: per group a title row (if any) and rows of cells
	rows = do.call(rbind, lapply(seq_along(groups), function(k) {
		g = groups[[k]]
		nr = ceiling(length(g$values) / ncol)
		rbind(if (!is.null(g$title)) data.frame(group = k, type = "title", first = NA_integer_),
			  data.frame(group = k, type = "cells", first = (seq_len(nr) - 1L) * ncol + 1L))
	}))
	# a footer with the pattern sets, so that they can be found
	footer = if (variable == "pattern" && all_values) {
		paste0("Pattern sets, to be used as values: ",
			   paste0("\"", names(.pattern_sets), "\"", collapse = ", "),
			   " (show them with tmap_show_values(\"pattern\", values = <set>))")
	}
	heights = do.call(grid::unit.c, lapply(rows$type, function(tp) {
		if (tp == "title") grid::unit(2, "lines") else grid::unit(1, "null")
	}))
	if (!is.null(footer)) heights = grid::unit.c(heights, grid::unit(2, "lines"))

	grid::grid.newpage()
	if (variable == "pattern") pattern_device_support()
	grid::pushViewport(grid::viewport(width = grid::unit(1, "npc") - grid::unit(1, "lines"),
									  height = grid::unit(1, "npc") - grid::unit(1, "lines"),
									  layout = grid::grid.layout(length(heights), 1, heights = heights)))

	# label size: shrink when the longest label does not fit in a cell
	labs_all = unlist(lapply(groups, show_value_labels))
	cell_w = grid::convertWidth(grid::unit(1, "npc"), "inch", valueOnly = TRUE) / ncol
	avail = if (variable == "lty") {
		7 * grid::convertWidth(grid::unit(1, "lines"), "inch", valueOnly = TRUE)
	} else 0.95 * cell_w
	lab_w = max(grid::convertWidth(grid::stringWidth(labs_all), "inch", valueOnly = TRUE)) * 0.8
	cex = 0.8 * min(1, avail / lab_w)

	for (r in seq_len(nrow(rows))) {
		g = groups[[rows$group[r]]]
		grid::pushViewport(grid::viewport(layout.pos.row = r))
		if (rows$type[r] == "title") {
			grid::grid.text(g$title, x = grid::unit(0.25, "lines"), y = grid::unit(0.6, "lines"),
							just = c("left", "bottom"),
							gp = grid::gpar(cex = 0.9, fontface = "bold", col = "grey20"))
		} else {
			labs = show_value_labels(g)
			ids = rows$first[r] - 1L + seq_len(ncol)
			ids = ids[ids <= length(g$values)]
			grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, ncol)))
			for (j in seq_along(ids)) {
				i = ids[j]
				grid::pushViewport(grid::viewport(layout.pos.col = j))
				show_value_cell(variable, g$values[i], labs[i], col = col, fill = fill, cex = cex)
				grid::upViewport()
			}
			grid::upViewport()
		}
		grid::upViewport()
	}
	if (!is.null(footer)) {
		grid::pushViewport(grid::viewport(layout.pos.row = length(heights)))
		fw = grid::convertWidth(grid::unit(1, "npc") - grid::unit(0.5, "lines"), "inch", valueOnly = TRUE)
		sw = grid::convertWidth(grid::stringWidth(footer), "inch", valueOnly = TRUE)
		fcex = 0.8 * min(1, fw / (0.8 * sw))
		grid::grid.text(footer, x = grid::unit(0.25, "lines"), just = "left",
						gp = grid::gpar(cex = fcex, col = "grey30", fontface = "italic"))
		grid::upViewport()
	}
	grid::upViewport()

	if (variable != "pattern") return(invisible(values))
	res = lapply(groups, function(g) if (g$quote) g$labels else unclass(g$values))
	if (length(res) == 1L) return(invisible(res[[1]]))
	# several groups: a list, named by pattern set
	names(res) = vapply(groups, function(g) {
		if (is.null(g$title)) "" else sub("^pattern set \"(.*)\"$", "\\1", g$title)
	}, character(1))
	invisible(res)
}

# labels: the codes to specify the values, quoted for character values (except pattern
# specifications)
show_value_labels = function(g) {
	ifelse(g$quote & !grepl("(", g$labels, fixed = TRUE), paste0("\"", g$labels, "\""), g$labels)
}

show_value_cell = function(variable, value, lab, col, fill, cex) {
	if (variable == "lty") {
		# label left of the line
		grid::pushViewport(grid::viewport(x = grid::unit(8, "lines"), just = "left",
										  width = grid::unit(1, "npc") - grid::unit(8.5, "lines")))
		show_value(variable, value, col = col, fill = fill)
		grid::upViewport()
		grid::grid.text(lab, x = grid::unit(7.5, "lines"), just = "right",
						gp = grid::gpar(cex = cex, col = "grey30"))
	} else {
		# label below the value
		grid::pushViewport(grid::viewport(y = grid::unit(1.5, "lines"), just = "bottom",
										  height = grid::unit(1, "npc") - grid::unit(1.75, "lines"),
										  width = grid::unit(1, "npc") - grid::unit(0.5, "lines")))
		show_value(variable, value, col = col, fill = fill)
		grid::upViewport()
		grid::grid.text(lab, y = grid::unit(0.75, "lines"), gp = grid::gpar(cex = cex, col = "grey30"))
	}
}

show_value = function(variable, value, col, fill) {
	switch(variable,
		   pattern = {
		   	grid::grid.rect(gp = grid::gpar(fill = fill, col = NA))
		   	if (value != "none") {
		   		grid::grid.draw(pattern_area_grob(grid::rectGrob(), value, density = 0.8, col = col))
		   	}
		   	grid::grid.rect(gp = grid::gpar(fill = NA, col = col, lwd = 0.5))
		   },
		   shape = grid::grid.points(x = 0.5, y = 0.5, pch = value, size = grid::unit(2, "lines"),
		   						  gp = grid::gpar(col = col, fill = fill, lwd = 1.5)),
		   lty = grid::grid.lines(x = c(0.02, 0.98), y = 0.5,
		   						gp = grid::gpar(col = col, lty = value, lwd = 2)),
		   fontface = grid::grid.text("Abc", gp = grid::gpar(col = col, fontface = value, cex = 1.6)))
}
