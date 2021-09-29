add_to_gt = function(gt, grb, row, col) {
	vp = grid::viewport(layout.pos.col = col, layout.pos.row = row)
	gtr = grid::grobTree(grb, vp = vp)
	
	grid::addGrob(gt, gtr, gPath = grid::gPath("gt_main"))
}





get_legend_option = function(x, type) {
	if (length(x) == 1 || (!type %in% names(x))) x else x[type]
}

leg_standard_p_lines = function(leg) {
	space = get_legend_option(leg$setup$space, leg$type)
	space.na = get_legend_option(leg$setup$space.na, leg$type)
	hs = if (leg$type == "symbols") {
		gp = gp_to_gpar(leg$gp)
		if (!is.na(leg$setup$height)) {
			# specified height in lines: recalculate space (and set space.na the same)
			space = (leg$setup$height - sum(rep(pmax(1, gp$size), length.out = leg$nitems))) / leg$nitems
			space.na = space
		} else {
			# add little margin needed for large symbols
			space = space + gp$size / 10
			space.na = space.na + gp$size / 10
		}
		size = rep(gp$size, length.out = leg$nitems)
		rep(pmax(1, c(size[1:(leg$nitems - leg$na.show)] + space, {if (leg$na.show) (size[leg$nitems] + space.na) else NULL})), length.out = leg$nitems)
	} else {
		if (!is.na(leg$setup$height)) {
			space = (leg$setup$height - leg$nitems) / leg$nitems
			space.na = space
		}
		c(rep(1 + space, length.out = leg$nitems - leg$na.show), {if (leg$na.show) (1 + space.na) else NULL})
	}
	# final rescale to meet specified height
	if (!is.na(leg$setup$height)) {
		if (sum(hs) != leg$setup$height) {
			hs = hs / sum(hs) * leg$setup$height
		}	
	}
	hs
}

gp_to_gpar = function(gp, id = NULL, sel = "all", split_to_n = NULL, pick_middle = TRUE) {
	if (sel == "all") {
		if (is.na(gp$fill_alpha) && !is.na(gp$col_alpha)) sel = "col"
		if (!is.na(gp$fill_alpha) && is.na(gp$col_alpha)) sel = "fill"
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
			   lty = if (sel == "fill") "blank" else if (!is.na(gp$lty[1])) gp$lty else "solid",
			   lwd = {if (!is.na(gp$lwd[1])) gp$lwd else 0},
			   lineend = {if (!is.na(gp$lineend[1])) gp$lineend else "round"},
			   linejoin = {if (!is.na(gp$linejoin[1])) gp$linejoin else "round"},
			   size = {if (!is.na(gp$size[1])) gp$size else 1},
			   shape = {if (!is.na(gp$shape[1])) gp$shape else 21}))
	
	# 
	if (!is.null(id)) {
		lst = lapply(lst, "[", id)
	}
	
	lst = mapply(function(lsti, isnum) {
		if (!is.character(lsti)) return(lsti)
		
		if (nchar(lsti[1]) > 50) {
			x = strsplit(lsti, split = "-", fixed=TRUE)
			x = lapply(x, function(i) {
				i[i=="NA"] <- NA
				i
			})
			if (isnum) x = lapply(x, as.numeric)
			if (pick_middle) {
				x = sapply(x, function(i) {
					if (all(is.na(i))) NA else {
						sq = c(5,6,4,7,3,8,2,9,1,10) # priority for middle values
						i[sq[which(!is.na(i)[sq])[1]]]
					}
				})
			}
			return(x)
			
		} else {
			return(lsti)
		}
	}, lst, c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE), SIMPLIFY = FALSE)
	
	if (!is.null(split_to_n)) {
		lst = lapply(1L:split_to_n, function(i) {
			lapply(lst, function(lsti) {
				if (length(lsti) == split_to_n) lsti[i] else lsti[1]
			})
		})
		lapply(lst, function(lsti) {
			do.call(grid::gpar, lsti)
		})
	} else {
		do.call(grid::gpar, lst)
	}
}


determine_scale = function(label, rot, row, col, g, scale = 1) {
	
	w = sum(g$colsIn[col])
	h = sum(g$rowsIn[row])
	
	labwidth = grid::convertWidth(grid::stringWidth(label), unitTo = "inches", valueOnly = TRUE)
	labheight = grid::convertWidth(grid::stringHeight(label), unitTo = "inches", valueOnly = TRUE)
	
	
	scale = min(scale, {if (rot %in% c(0, 180)) {
		min(w / labwidth, h / labheight)
	} else {
		min(h / labwidth, w / labheight)
	}})
}



tmapGridWrap = function(label, facet_row, facet_col, facet_page, o) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)
	
	gt = gts[[facet_page]]

	rot = g$panel_rot
	row = g$rows_panel_ids[facet_row]
	col = g$cols_panel_ids[facet_col]
	
	frame.col = if (identical(o$frame, FALSE)) o$attr.color else if (identical(o$frame, TRUE)) o$attr.color else o$frame
	
	
	gpar_rect = grid::gpar(fill = o$panel.label.bg.color, lwd=o$frame.lwd, col = o$frame)
	gpar_text = rescale_gp(grid::gpar(cex = o$panel.label.size, col = o$panel.label.color, fontfamily = o$panel.label.fontfamily, fontface = o$panel.label.fontface), o$scale_down)
	
	# resize due to not fitting
	gpar_text$cex = determine_scale(label = label, rot = rot, row = row, col = col, g = g, scale = gpar_text$cex)
	
	grb = grid::grobTree(
		grid::rectGrob(gp = gpar_rect),
		grid::textGrob(label = label, rot = rot, gp = gpar_text)
	)
	
	
	gt = add_to_gt(gt, grb, row = row, col = col)

	gts[[facet_page]] = gt
	
	assign("gts", gts, envir = .TMAP_GRID)
	
	
}

tmapGridXtab = function(label, facet_row = NULL, facet_col = NULL, facet_page, o) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)
	
	gt = gts[[facet_page]]
	
	if (is.null(facet_row)) {
		rot = g$panel_col_rot
		row = g$cols_panel_row_id
		col = g$cols_panel_col_ids[facet_col]
	} else {
		rot = g$panel_row_rot
		row = g$rows_panel_row_ids[facet_row]
		col = g$rows_panel_col_id
	}
	
	gpar_rect = grid::gpar(fill = o$panel.label.bg.color, lwd=o$frame.lwd)
	gpar_text = rescale_gp(grid::gpar(cex = o$panel.label.size, col = o$panel.label.color, fontfamily = o$panel.label.fontfamily, fontface = o$panel.label.fontface), o$scale_down)
	
	# resize due to not fitting
	gpar_text$cex = determine_scale(label = label, rot = rot, row = row, col = col, g = g, scale = gpar_text$cex)
	
	grb = grid::grobTree(
		grid::rectGrob(gp = gpar_rect),
		grid::textGrob(label = label, rot = rot, gp = gpar_text)
	)

	gt = add_to_gt(gt, grb, row = row, col = col)
	
	gts[[facet_page]] = gt
	
	assign("gts", gts, envir = .TMAP_GRID)
	
	
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


tmapGridRun = function(o) {
	gts = get("gts", .TMAP_GRID)
	lapply(gts, function(gt) {
		grid::grid.newpage()
		grid::grid.draw(gt)
	})
}
