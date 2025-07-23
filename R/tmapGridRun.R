tmapGridRun = function(o, q, show, knit, knit_opts, args) {
	gts = get("gts", .TMAP_GRID)


	if (getOption("tmap.design.mode") && getOption("tmap.design.mode_overall")) {
		g = get("g", .TMAP_GRID)
		nr = g$nr
		nc = g$nc
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




	if (show) {
		if (o$show_gif_ani) {
			d <- paste(tempdir(), "/tmap_plots", sep="/")
			dir.create(d, showWarnings = FALSE)
			devsize = c(grid::convertWidth(grid::unit(1, "npc"), unitTo = "inch", valueOnly = TRUE) * 72 * o$dpr,
						grid::convertHeight(grid::unit(1, "npc"), unitTo = "inch", valueOnly = TRUE) * 72 * o$dpr)
		}

		mapply(function(gt,i) {
			if (o$show_gif_ani) {
				png(paste0(d, "/plot", sprintf("%03d", i), ".png"), width = devsize[1], height = devsize[2], res = 72*o$dpr, type = "cairo-png")
				grid::grid.newpage()
			} else {
				if (is.null(o$vp) && i != 1L) grid::grid.newpage()
			}

			tryCatch({
				grid::grid.draw(gt)
			}, error = function(e) {
				stop("Plot error. Try adding + tm_check_fix()", call. = FALSE)
			})
			if (o$show_gif_ani) {
				dev.off()
			}

		}, gts, seq_along(gts), SIMPLIFY = FALSE)
		if (!is.null(o$vp)) upViewport(1)
	}
	if (length(gts) == 1) gts = gts[[1]]

	if (show && o$show_gif_ani) {
		files = list.files(path = d, pattern = "^plot[0-9]{3}\\.png$", full.names = TRUE)
		if (o$play == "pingpong") files = c(files, rev(files))
		filename = tempfile(fileext = ".gif")
		create_animation(filename = filename, files = files, width = devsize[1], height = devsize[2], delay = NA, fps = o$fps, loop = o$play != "once", progress = FALSE, gif = TRUE, showAni = TRUE, dpr = o$dpr, knit = knit, knit_opts = knit_opts)
		if (knit && !knitr::is_latex_output()) {
			knitr_path <- knitr::fig_path('.gif', knit_opts)
			dir.create(dirname(knitr_path), showWarnings = FALSE, recursive = TRUE)
			file.copy(filename, knitr_path, overwrite = TRUE)
			if (is.null(knit_opts$out.width)) {
				knit_opts$out.width <- knit_opts$fig.width * knit_opts$dpi / (knit_opts$fig.retina %||% 1)
			}
			return(do.call(knitr::knit_print, c(list(x = knitr::include_graphics(knitr_path), options = knit_opts), args)))
		}
	}

	gts
}
