tmapGridInit = function(o, show = TRUE, newpage = TRUE, return.asp = FALSE, vp, prx, ...) {
	rlang::check_installed("grid")

	rows = with(o, {
		x = c(outer.margins.top = outer.margins[3],
			  meta.buffers.top.out = meta.buffers[3],
			  meta.margins.top = meta.margins[3],
			  meta.buffers.top.in = meta.buffers[3],
			  xylab.margins.top = xylab.margins[3],

			  panel.xtab.top = panel.xtab.size[3],
			  panel.xtab.margin.top = panel.xtab.margin[3],
			  grid.buffers.top = grid.buffers[3],
			  grid.margins.top = grid.margins[3],

			  {if (o$nrows > 1) rep(c(panel.wrap.size[3], panel.wrap.margin[3], 0, panel.wrap.margin[1], panel.wrap.size[1], between_marginH), o$nrows -1) else NULL},
			                          panel.wrap.size[3], panel.wrap.margin[3], 0, panel.wrap.margin[1], panel.wrap.size[1],

			  grid.margins.bottom = grid.margins[1],
			  grid.buffers.bottom = grid.buffers[1],
			  panel.xtab.margin.bottom = panel.xtab.margin[1],
			  panel.xtab.bottom = panel.xtab.size[1],

			  xylab.margins.bottom = xylab.margins[1],
			  meta.buffers.bottom.in = meta.buffers[1],
			  meta.margins.bottom = meta.margins[1],
			  meta.buffers.bottom.out = meta.buffers[1],
			  outer.margins.bottom = outer.margins[1])

		u = grid::unit(x, "npc")
		names(u) = names(x)
		u
	})

	cols = with(o, {
		x = c(outer.margins.left = outer.margins[2],
			  meta.buffers.left.out = meta.buffers[2],
			  meta.margins.left = meta.margins[2],
			  meta.buffers.left.in = meta.buffers[2],
			  xylab.margins.left = xylab.margins[2],

			  panel.xtab.left = panel.xtab.size[2],
			  panel.xtab.margin.left = panel.xtab.margin[2],
			  grid.buffers.left = grid.buffers[2],
			  grid.margins.left = grid.margins[2],

			  {if (o$ncols > 1) rep(c(panel.wrap.size[2], panel.wrap.margin[2], 0, panel.wrap.margin[4], panel.wrap.size[4], between_marginW), o$ncols -1) else NULL},
			                          panel.wrap.size[2], panel.wrap.margin[2], 0, panel.wrap.margin[4], panel.wrap.size[4],

			  grid.margins.left = grid.margins[4],
			  grid.buffers.left = grid.buffers[4],
			  panel.xtab.margin.right = panel.xtab.margin[4],
			  panel.xtab.right = panel.xtab.size[4],

			  xylab.margins.right = xylab.margins[4],
			  meta.buffers.right.in = meta.buffers[4],
			  meta.margins.right = meta.margins[4],
			  meta.buffers.right.out = meta.buffers[4],
			  outer.margins.right = outer.margins[4])

		u = grid::unit(x, "npc")
		names(u) = names(x)
		u
	})

	nr = length(rows)
	nc = length(cols)

	cols_facet_ids = 1:o$ncols * 6 + 6
	rows_facet_ids = 1:o$nrows * 6 + 6

	#if (o$panel.type == "xtab") {
	cols_panel_col_ids = cols_facet_ids
	cols_panel_row_id = ifelse(o$panel.xtab.pos[2] == "top", 6, nc - 5)

	rows_panel_row_ids = rows_facet_ids
	rows_panel_col_id = ifelse(o$panel.xtab.pos[1] == "left", 6, nc - 5)
	#} else if (o$panel.type == "wrap") {
	cols_panel_ids = cols_facet_ids + ifelse(o$panel.wrap.pos  == "left", -2, ifelse(o$panel.wrap.pos  == "right", 2, 0))
	rows_panel_ids = rows_facet_ids + ifelse(o$panel.wrap.pos  == "top", -2, ifelse(o$panel.wrap.pos  == "bottom", 2, 0))

	panel_col_rot = ifelse(o$panel.xtab.pos[2] == "top", o$panel.label.rot[2], o$panel.label.rot[4])
	panel_row_rot = ifelse(o$panel.xtab.pos[1] == "left", o$panel.label.rot[1], o$panel.label.rot[3])
	panel_rot = ifelse(o$panel.wrap.pos  == "left", o$panel.label.rot[1], ifelse(o$panel.wrap.pos  == "right", o$panel.label.rot[3], ifelse(o$panel.wrap.pos  == "top", o$panel.label.rot[2], o$panel.label.rot[4])))

	#}
	#####
	### x and y lab
	xlab_row_id = if (o$xlab.side == "top") 5 else nr - 4
	xlab_col_ids = 6:(nc-5)

	ylab_col_id = if (o$ylab.side == "left") 5 else nc - 4
	ylab_row_ids = 6:(nr-5)


	prows = as.numeric(rows)
	pcols = as.numeric(cols)

	if (sum(prows) >= 1 || sum(pcols) >= 1) stop("Margins are too large, or too many facets.", call. = FALSE)

	fasp = ((1-sum(pcols)) / (1-sum(prows))) * o$dasp / o$ncols * o$nrows # asp per facet (with original outer margins)
	gasp = ((1-sum(pcols)) / (1-sum(prows))) * o$dasp # asp total facets (with original outer margins)

	if (!o$legend.only) {
		if (!is.na(o$asp) && o$asp != 0) {
			# follow device
			fasp = o$asp
		} else if (is.na(o$asp) && !is.na(o$sasp)) {
			fasp = o$sasp
		}
	}

	gasp2 = fasp * o$ncols / o$nrows # target gasp

	# needed for tmap save and arrange
	if (return.asp) return(gasp2)


	if (gasp2 > gasp) {
		extra.height =   (1 - ((1 - sum(pcols))/(gasp2/o$dasp))) - sum(prows)
		rows[c(1, length(rows))] = rows[c(1, length(rows))] + grid::unit(extra.height / 2, "npc")
	} else if (gasp2 < gasp) {
		extra.width =   (1 - ((1 - sum(prows)) * (gasp2/o$dasp))) - sum(pcols)
		cols[c(1, length(cols))] = cols[c(1, length(cols))] + grid::unit(extra.width / 2, "npc")
	}

	cols[cols_facet_ids] = (grid::unit(1, "npc") - sum(cols)) / o$ncols
	rows[rows_facet_ids] = (grid::unit(1, "npc") - sum(rows)) / o$nrows

	colsIn = as.numeric(cols) * o$devsize[1]
	rowsIn = as.numeric(rows) * o$devsize[2]

	#sum(grid::convertWidth(cols, "inches", valueOnly = TRUE))


	vp_tree = grid::vpStack(grid::viewport(width = grid::unit(o$cw, "snpc"), height = grid::unit(o$ch, "snpc"), name = "vp_asp"),
							grid::viewport(layout = grid::grid.layout(nrow = length(rows), ncol = length(cols), widths = cols, heights = rows), name = "vp_main")
	)

	bgcol = o$outer.bg.color

	outerRect = if (o$outer.bg) rndrectGrob(gp=grid::gpar(lwd = NA, fill = bgcol), name = "outer_rect") else NULL

	gts = lapply(1L:o$npages, function(ip) {
		grid::grobTree(
			outerRect,
			grid::grobTree(name = "gt_main"),
			vp = vp_tree, name = "tmap_grob_tree")
	})

	g = list(
		rows_facet_ids = rows_facet_ids,
		cols_facet_ids = cols_facet_ids,

		rows_panel_ids = rows_panel_ids,
		cols_panel_ids = cols_panel_ids,

		rows_panel_row_ids = rows_panel_row_ids,
		rows_panel_col_id = rows_panel_col_id,
		cols_panel_row_id = cols_panel_row_id,
		cols_panel_col_ids = cols_panel_col_ids,

		panel_col_rot = panel_col_rot,
		panel_row_rot = panel_row_rot,
		panel_rot = panel_rot,

		xlab_row_id = xlab_row_id,
		xlab_col_ids = xlab_col_ids,

		ylab_col_id = ylab_col_id,
		ylab_row_ids = ylab_row_ids,

		meta_rows = c(3, (nr-2)),
		meta_cols = c(3, (nc-2)),

		fasp = fasp,

		colsIn = colsIn,
		rowsIn = rowsIn,

		nc = nc,
		nr = nr
	)


	# design mode: widths and heights (and asp) for subset of cols/rows
	res = do.call(rbind, lapply(1:7, function(i) {
		tot_col = sum(g$colsIn[i:(nc-(i-1))])
		tot_row = sum(g$rowsIn[i:(nr-(i-1))])
		c(tot_col, tot_row, tot_col/ tot_row)
	}))


	# take make rows and columns
	g$mapColsIn = g$colsIn[g$cols_facet_ids]
	g$mapRowsIn = g$rowsIn[g$rows_facet_ids]

	# add w/h/asp for first map
	res = rbind(res, c(g$mapColsIn[1], g$mapRowsIn[1], g$mapColsIn[1]/ g$mapRowsIn[1]))

	## show aspect ratios in design mode
	if (getOption("tmap.design.mode")) {
		posttext <- apply(format(res[c(1,2,4, 8),], digits = 3), FUN = paste, MARGIN = 1, collapse = " ")
		pretext <- c("device", "plot area", "facets area", "map area")

		lns <- nchar(pretext) + nchar(posttext)
		l <- max(max(nchar(pretext)) + max(nchar(posttext)) + 1, 25)
		medtext <- vapply(l-lns, function(i)paste(rep(" ", i), collapse=""), character(1))

		texts <- c("---------------W (in)-H (in)-asp---",
				   paste("|", pretext, medtext, posttext, "|"),
				   paste(rep("-", l+6), collapse=""))

		texts_colors = c("Color codings:",
						 "- light blue  outer margins",
						 "- dark blue   buffers around outside cells",
						 "- light green outside cells",
						 "- dark green  x and ylab cells",
						 "- pink        panels",
						 "- red         margins for outside grid labels",
						 "- orange      margins around maps for grid labels",
						 "- yellow      map area",
						 "- lavender    component areas",
						 "Guide lines:",
						 "- thick       component position (legend, scalebar, etc.)",
						 "- thin        component-element position (e.g. legend items)"
						 )

		for (tx in c(texts, texts_colors)) message(tx)

		gts = lapply(gts, function(gt) {

			p = rep(cols4all::c4a("brewer.paired"), 3)

			gt = gt %>%
				add_to_gt(grid::rectGrob(gp=grid::gpar(fill = p[1])), row = 1:(nr), col = 1:(nc)) %>%  # outer
				add_to_gt(grid::rectGrob(gp=grid::gpar(fill = p[2])), row = 2:(nr-1), col = 2:(nc-1)) %>%   # meta buffer out
				add_to_gt(grid::rectGrob(gp=grid::gpar(fill = p[3])), row = 3:(nr-2), col = 3:(nc-2)) %>%   # meta margins
				add_to_gt(grid::rectGrob(gp=grid::gpar(fill = p[2])), row = 4:(nr-3), col = 4:(nc-3)) %>%   # meta buffer in
				add_to_gt(grid::rectGrob(gp=grid::gpar(fill = p[4])), row = 5:(nr-4), col = 5:(nc-4))  # xylab
			if (o$panel.type == "xtab") {
				#add_to_gt(grid::rectGrob(gp=grid::gpar(fill = p[5])), row = 6:(nr-5), col = 6:(nc-5)) # panel buffer
				gt = add_to_gt(gt, grid::rectGrob(gp=grid::gpar(fill = p[5])), row = 6:(nr-5), col = 6:(nc-5)) # panel
			}

			gt = gt %>%
				add_to_gt(grid::rectGrob(gp=grid::gpar(fill = p[6])), row = 7:(nr-6), col = 7:(nc-6)) %>%  # grid buffer
				add_to_gt(grid::rectGrob(gp=grid::gpar(fill = p[7])), row = 8:(nr-7), col = 8:(nc-7))  # grid


			for (i in 1:o$nrows) {
				for (j in 1:o$ncols) {
					gt = add_to_gt(gt, grid::rectGrob(gp=grid::gpar(fill = p[11])), row = g$rows_facet_ids[i], col = g$cols_facet_ids[j])
					if (o$panel.type == "wrap") {
						gt = add_to_gt(gt, grid::rectGrob(gp=grid::gpar(fill = p[5])), row = g$rows_panel_ids[i], col = g$cols_panel_ids[j])
					}

				}
			}

			gt
		})
	}

	# margins around first map (needed for georeferencing)
	rid = g$rows_facet_ids[1]
	cid = g$cols_facet_ids[1]
	map_all_margins = c(
		sum(g$rowsIn[(rid+1L):nr]),
		sum(g$colsIn[1:(cid-1L)]),
		sum(g$rowsIn[1:(rid-1L)]),
		sum(g$colsIn[(cid+1L):nc]))




	if (is.null(vp) && show && newpage) {
		grid.newpage()
	}# else {
	#	if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
	#}
	assign("gts", gts, envir = .TMAP_GRID)
	assign("g", g, envir = .TMAP_GRID)
	assign("gasp2", gasp2, envir = .TMAP_GRID)
	.TMAP$start_pane_id = 401
	list(dev = res[1,],map = res[8,], margins = map_all_margins)
}

tmapGridAux = function(o, q) {
	NULL
}
