add_to_gt = function(gt, grb, row, col) {
	vp = grid::viewport(layout.pos.col = col, layout.pos.row = row)
	gtr = grid::grobTree(grb, vp = vp)
	
	grid::addGrob(gt, gtr, gPath = grid::gPath("gt_main"))
}





get_legend_option = function(x, type) {
	if (length(x) == 1 || (!type %in% names(x))) x else if (is.list(x)) x[[type]] else x[type]
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
			   lwd = {if (!all(is.na(gp$lwd))) gp$lwd else 0},
			   lineend = {if (!all(is.na(gp$lineend))) gp$lineend else "round"},
			   linejoin = {if (!all(is.na(gp$linejoin))) gp$linejoin else "round"},
			   size = {if (!all(is.na(gp$size))) gp$size else 1},
			   shape = {if (!all(is.na(gp$shape))) gp$shape else 21}))
	
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



