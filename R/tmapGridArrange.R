tmapGridArrange = function(tms, nx, ncol, nrow, opts, knit, show, args, options) {
	widths <- opts$widths
	heights = opts$heights
	if (is.na(widths[1])) widths <- rep(1/ncol, ncol)
	if (is.na(heights[1])) heights <- rep(1/nrow, nrow)

	grid.newpage()
	vp <- viewport(layout=grid.layout(nrow=nrow, ncol=ncol, widths = widths, heights=heights), name = "tmap_arrangement")
	pushViewport(vp)

	if (!is.null(opts$asp) || !is.null(opts$outer.margins)) {
		layout_args <- list(asp=opts$asp, outer.margins=opts$outer.margins)
		layout_args <- layout_args[!vapply(layout_args, is.null, logical(1))]
		tml <- do.call(tm_options, layout_args)
	} else {
		tml <- NULL
	}

	nc <- 1
	nr <- 1
	for (i in 1:nx) {
		tm <- tms[[i]]
		if (!is.null(tml)) tm <- tm + tml
		print(tm, vp = viewport(layout.pos.col = nc, layout.pos.row = nr))
		nc <- nc + 1
		if (nc > ncol) {
			nc <- 1
			nr <- nr + 1
		}
	}
}
