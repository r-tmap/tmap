add_to_gt = function(gt, grb, row, col) {
	vp = grid::viewport(layout.pos.col = col, layout.pos.row = row)
	gtr = grid::grobTree(grb, vp = vp)

	grid::addGrob(gt, gtr, gPath = grid::gPath("gt_main"))
}





get_legend_option = function(x, type) {
	if (length(x) == 1 || (!type %in% names(x))) x else if (is.list(x)) x[[type]] else x[type]
}


gp_to_gpar = function(gp, id = NULL, sel = "all", split_to_n = NULL, pick_middle = TRUE, o, type) {
	type = tolower(type)
	if (sel == "all") {
		if (is.na(gp$fill_alpha[1]) && !is.na(gp$col_alpha[1])) sel = "col"
		if (!is.na(gp$fill_alpha[1]) && is.na(gp$col_alpha[1])) sel = "fill"
	}


	# get alpha value (sel: "all" means fill and col, "fill" and "col" mean fill and col only respectively)
	alpha = if (sel == "fill") {
		if (!is.na(gp$fill_alpha[1])) gp$fill_alpha else 1
	} else {
		if (!is.na(gp$col_alpha[1])) gp$col_alpha else 1
	}

	# create a list of gp elements
	lst = c(list(fill = {if (sel == "col") NA else gp$fill},
			   col = {if (sel == "fill") NA else gp$col},
			   alpha = alpha,
			   lty = if (sel == "fill") getAesOption("value.blank", o, aes = "lty", layer = type) else if (!is.na(gp$lty[1])) gp$lty else getAesOption("value.const", o, aes = "lty", layer = type) ,
			   lwd = {if (!all(is.na(gp$lwd))) gp$lwd else getAesOption("value.blank", o, aes = "lwd", layer = type)},
			   lineend = {if (!all(is.na(gp$lineend))) gp$lineend else "round"},
			   linejoin = {if (!all(is.na(gp$linejoin))) gp$linejoin else "round"},
			   size = {if (!all(is.na(gp$size))) gp$size else 1},
			   cex = {if (!all(is.na(gp$cex))) gp$cex else 1},
			   fontface = {if (!all(is.na(gp$cex))) gp$fontface else getAesOption("value.blank", o, aes = "fontface", layer = type)},
			   fontfamily = {if (!all(is.na(gp$fontfamily))) gp$fontfamily else o$text.fontfamily},
			   shape = {if (!all(is.na(gp$shape))) gp$shape else getAesOption("value.const", o, aes = "shape", layer = type)}))

	#
	if (!is.null(id)) {
		lst = lapply(lst, "[", id)
	}

	lst_isnum = c(fill = FALSE,
				  col = FALSE,
				  alpha = TRUE,
				  lty = FALSE,
				  lwd = TRUE,
				  lineend = FALSE,
				  linejoin = FALSE,
				  size = TRUE,
				  cex = TRUE,
				  fontface = FALSE,
				  fontfamily = FALSE,
				  shape = TRUE)

	lst = mapply(function(lsti, isnum) {
		if (!is.character(lsti)) return(lsti)

		if (nchar(lsti[1]) > 50) {
			x = cont_split(lsti)
			x = lapply(x, function(i) {
				i[i == "NA"] <- NA
				i
			})
			if (isnum) x = lapply(x, as.numeric)
			if (pick_middle) {
				x = sapply(x, function(i) {
					if (all(is.na(i))) NA else {
						sq = (o$continuous.nclass_per_legend_break/2) + (rep(0:5,each=2) * c(1,-1))[-1] # priority for middle values
						i[sq[which(!is.na(i)[sq])[1]]]
					}
				})
			}
			return(x)

		} else {
			return(lsti)
		}
	}, lst, lst_isnum[names(lst)], SIMPLIFY = FALSE)

	if (!is.null(split_to_n)) {
		split_gp(lst, split_to_n)
	} else {
		do.call(grid::gpar, lst)
	}
}

split_gp = function(gp, n) {
	lst = lapply(1L:n, function(i) {
		lapply(gp, function(gpi) {
			if (length(gpi) == n) gpi[i] else gpi[1]
		})
	})
	lapply(lst, function(lsti) {
		do.call(grid::gpar, lsti)
	})
}


determine_scale = function(label, rot, row, col, g, scale = 1) {

	w = sum(g$colsIn[col])
	h = sum(g$rowsIn[row])

	labwidth = graphics::strwidth(label, units = "inches")
	labheight = graphics::strheight(label, units = "inches")


	scale = min(scale, {if (rot %in% c(0, 180)) {
		min(w / labwidth, h / labheight)
	} else {
		min(h / labwidth, w / labheight)
	}})
}

frc = function(row, col) paste0(sprintf("%02d", row), "_", sprintf("%02d", col))

impute_gp = function(gp, dt) {
	dtn = setdiff(names(dt), c("tmapID__", paste0("by", 1L:3L, "__")))

	cols = paste0("__", dtn)
	gp1 = sapply(gp, "[[", 1)
	gpids = which(gp1 %in% cols)
	#gp[gpids] = as.list(dt[, dtn, with = FALSE])

	for (i in gpids) gp[i] = as.list(dt[, dtn[match(gp1[i], cols)], with = FALSE])
	gp
}

rndrectGrob = function(...) {
	args = list(...)
	if ("r" %in% names(args) && args$r > 0) {
		args$r = grid::unit(args$r, "pt")
		do.call(grid::roundrectGrob, args)
	} else {
		args$r = NULL
		do.call(grid::rectGrob, args)
	}
}

# numeric vector (interpreted as inches) will be cast to a unit vector of inches with nulls on the head and tail. The values of the nulls depend on the justification
set_unit_with_stretch = function(x, ids = NULL, sides = c("both", "first", "second")) {
	u = rep("inch", length(x))
	if (!is.null(ids)) {
		x[ids] = 1/length(ids)
		u[ids] = "null"
		x = c(0, x, 0)
	} else {
		sides = match.arg(sides)
		x = if (sides == "both") c(0.5, x, 0.5) else if (sides == "first") c(1, x, 0) else c(0, x, 1)
	}
	u = c("null", u, "null")
	grid::unit(x, units = u)
}

# numeric vector for which spacers will be added between
unit_add_between = function(x, y) {
	n = length(x)
	if (n == 1L) return(x)
	x = rep(x, each = 2)[-(2L*n)]
	x[seq(2, (2*n)-1, by = 2)] = y
	x
}

# numeric vector for which head and tail will be added
unit_add_sides = function(x, y) {
	if (length(y) == 1L) {
		c(y, x, y)
	} else {
		c(y[1], x, y[2])
	}
}



# u is unit vector, tot = is the total size (width or height). The null units are distributed such that the total equals tot
distr_space_over_nulls = function(u, tot, stretchID = NA) {
	u0 = grid::unitType(u) == "null"

	un = grid::convertUnit(u, unitTo = "inch", valueOnly = TRUE)
	tn = as.numeric(tot)

	un_not0 = sum(un[!u0])
	un_dist_over0 = tn - un_not0

	normalize = function(x, s = 1) (x / sum(x)) * s

	if (un_dist_over0 < 0) {
		un[u0] = 0
		if (!is.na(stretchID)) un[stretchID] = max(0, un[stretchID] + un_dist_over0)
		un = normalize(un, s = tn)
	} else {
		un[u0] = normalize(as.numeric(u[u0]), un_dist_over0)
	}
	#print(un)
	grid::unit(un, units = "inch")
}

rescale_gp = function(gp, scale, skip = character()) {
	if ("lwd" %in% names(gp) && (!"lwd" %in% skip)) gp$lwd = gp$lwd * scale
	if ("size" %in% names(gp) && (!"size" %in% skip)) gp$size = gp$size * sqrt(scale)
	if ("cex" %in% names(gp) && (!"cex" %in% skip)) gp$cex = gp$cex * sqrt(scale)
	gp
}




appendGlist = function(glist, x) {
	glist = grid::gList(glist, x)
	names(glist)[length(glist)] = x$name
	glist
}

swap_pch_15_20 = function(gp) {
	# swap fill and col for pch 15-20
	if (any(gp$shape %in% 15:20)) {
		gp = make_equal_list(gp)
		pch15_20 = which(gp$shape %in% 15:20)
		fill = gp$col[pch15_20]
		fill_alpha = gp$col_alpha[pch15_20]
		gp$col[pch15_20] = gp$fill[pch15_20]
		gp$col_alpha[pch15_20] = gp$fill_alpha[pch15_20]
		gp$fill[pch15_20] = fill
		gp$fill_alpha[pch15_20] = fill_alpha
	}
	gp
}


# zero_one_to_hex = function(x) {
# 	# using indexing
# 	u = unique(x)
#
# 	x255 = round(u * 255)
#
# 	nc = c(0:9, LETTERS[1:6])
#
# 	y1 = (x255 %/% 16) + 1
# 	y2 = (x255 - (y1 - 1) * 16) + 1
#
# 	r = paste0(nc[y1], nc[y2])
# 	r[match(x, u)]
# }

# 255 to 2digit hex number
num_to_hex = function(x) {

	nc = c(0:9, LETTERS[1:6])

	y1 = (x %/% 16) + 1
	y2 = (x - (y1 - 1) * 16) + 1

	paste0(nc[y1], nc[y2])
}

hex_to_num = function(h) {
	nc = c(0:9, LETTERS[1:6])
	y1 = match(substr(h, 1, 1), nc)
	y2 = match(substr(h, 2, 2), nc)
	(y1 - 1) * 16 + (y2 - 1)
}

merge_alpha = function(dt, name) {
	name_a = paste0(name, "_alpha")
	f = function(d) {
		d1 = d[1,]
		col = d1[[name]]
		alpha = d1[[name_a]]

		if (nchar(col) == 9) {
			a = hex_to_num(substr(col, 8, 9)) * alpha
			cl = substr(col, 1, 7)
		} else {
			a = alpha * 255
			cl = col
		}
		ac = paste0(cl, num_to_hex(round(a)))
		rep(ac, nrow(d))
	}

	dt[, ca:=f(.SD), by = c(name, name_a), .SDcols = c(name, name_a)]
}





npc_to_native = function(x, scale) {
	x * (scale[2] - scale[1])# + scale[1]
}

