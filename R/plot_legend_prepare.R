plot_legend_prepare <- function(gp, gal, gt, scaleFactor) {

	varnames <- c("fill", "symbol.size", "symbol.col", "symbol.shape", "line.col", "line.lwd", "raster", "text.size", "text.col")
	varnames_hist <- c("fill", "symbol.col", "line.col", "raster")
	
	# todo hist: "fill_hist"
	# is.portrait
	
	if (gt$legend.show) {
		xadd <- lapply(gal, function(g) {
			type <- if(g$type=="symbol") {
				if (is.null(g$size)) {
					"symbol.col"
				} else "symbol.size"
			} else if (g$type=="line") {
				"line.col"
			} else if (g$type=="text") {
				if (is.null(g$size) || length(g$size)==1) {
					"text.col"
				} else {
					"text.size"
				}
			} else "fill"
			
			nitems <- max(length(g$col), length(g$size), length(g$shape), length(g$labels), length(g$text), length(g$lwd), length(g$lty))
			
			if (nitems == 0) {
				type = "title"
				legend.labels = NULL
				legend.palette = NULL
				legend.text = NULL
				legend.sizes = NULL
				legend.shapes = NULL
				line.legend.lwd = NULL
				line.legend.lty = NULL
			} else {
				revfun <- if (g$reverse) rev else function(x)x
				
				legend.format <- process_legend_format(g$legend.format, gt$legend.format, 1)
				
				legend.labels <- revfun(if (is.null(g$labels)) rep("", nitems) else rep(g$labels, length.out=nitems))
				attr(legend.labels, "align") <- legend.format$text.align
				
				size_ext <- ifelse(type == "text.size", 1, scaleFactor)
				
				palette_colors <- revfun(if (is.null(g$col)) rep("grey50", nitems) else rep(g$col, length.out=nitems))
				legend.palette <- do.call("process_color", c(list(col=palette_colors, alpha = g$alpha), gt$pc))
				
				legend.text = revfun(if (is.null(g$text)) NULL else rep(g$text, length.out=nitems))
				legend.sizes = revfun(if (is.null(g$size)) size_ext else rep(g$size, length.out=nitems) * size_ext)
				legend.shapes = revfun(if (is.null(g$shape)) rep(21, nitems) else rep(g$shape, length.out=nitems))

				line.legend.lwd=revfun(if (is.null(g$lwd) && type != "line.col") NULL else rep({if (is.null(g$lwd)) 1 else g$lwd}, length.out=nitems))
				line.legend.lty=revfun(if (is.null(g$lty)) NULL else rep(g$lty, length.out=nitems))
				
				if (type == "text" && is.null(legend.text)) stop("For tm_add_legend(type = \"text\"), please speficy the argument text", call. = FALSE) # other types should always work

			}
			
			
			
			list(legend.type=type,
				 legend.title=g$title,
				 legend.is.portrait=g$is.portrait,
				 legend.z=g$z,
				 legend.labels=legend.labels,
				 legend.text=legend.text,
				 legend.palette=legend.palette,
				 legend.sizes=legend.sizes,
				 legend.shapes=legend.shapes,
				 border.col=g$border.col,
				 lwd=g$border.lwd,
				 line.legend.lwd=line.legend.lwd,
				 line.legend.lty=line.legend.lty,
				 symbol.border.lwd=g$border.lwd,
				 symbol.border.col=g$border.col,
				 symbol.normal.size=formals(tm_symbols)$legend.max.symbol.size,
				 symbol.max.size=if (is.null(g$size)) NULL else max(g$size)) # * scaleFactor
		})
		
		x <- lapply(gp, function(gpl) {
			y <- lapply(varnames, function(v) {
				if (!is.na(gpl$varnames[[v]][1])) {
					if (gpl[[paste(v, "legend.show", sep=".")]]) {
						legend.labels <- paste(v, "legend.labels", sep=".")
						legend.text <- paste(v, "legend.text", sep=".")
						legend.palette <- paste(v, "legend.palette", sep=".")
						legend.sizes <- paste(v, "legend.sizes", sep=".")
						legend.shapes <-paste(v, "legend.shapes", sep=".")
						legend.title <- paste(v, "legend.title", sep=".")
						legend.is.portrait <- paste(v, "legend.is.portrait", sep=".")
						legend.z <- paste(v, "legend.z", sep=".")
						legend.misc <- paste(v, "legend.misc", sep=".")
						list_misc <- gpl[[legend.misc]]
						if (v %in% c("symbol.col", "symbol.shape")) list_misc$symbol.max.size <- list_misc$symbol.max.size * scaleFactor
						
						size_ext <- ifelse(v == "text.size", 1, scaleFactor)
						c(list(legend.type=v,
							   legend.title=gpl[[legend.title]],
							   legend.is.portrait=gpl[[legend.is.portrait]],
							   legend.z=gpl[[legend.z]],
							   legend.labels=gpl[[legend.labels]],
							   legend.text=gpl[[legend.text]],
							   legend.palette=gpl[[legend.palette]],
							   legend.sizes=gpl[[legend.sizes]] * size_ext,
							   legend.shapes=gpl[[legend.shapes]]),
						  list_misc)
					}
				}
			})
			
			yhist <- lapply(varnames_hist, function(v) {
				vh <- paste(v, "hist", sep="_")
				legend.hist <- paste(v, "legend.hist", sep=".")
				
				if (!is.na(gpl$varnames[[v]][1])) {
					if (gpl[[legend.hist]]) {
						legend.labels <- paste(v, "legend.labels", sep=".")
						legend.palette <- paste(v, "legend.palette", sep=".")
						legend.sizes <- paste(v, "legend.sizes", sep=".")
						legend.shapes <-paste(v, "legend.shapes", sep=".")
						legend.title <- paste(v, "legend.hist.title", sep=".")
						legend.hist.z <- paste(v, "legend.hist.z", sep=".")
						legend.hist.misc <- paste(v, "legend.hist.misc", sep=".")
						list_hist_misc <- gpl[[legend.hist.misc]]
						c(list(legend.type="hist",
							   legend.title=gpl[[legend.title]],
							   legend.is.portrait=TRUE,
							 legend.z=gpl[[legend.hist.z]],
							 legend.labels=gpl[[legend.labels]],
							 legend.palette=gpl[[legend.palette]],
							 legend.sizes=gpl[[legend.sizes]] * scaleFactor,
							 legend.shapes=gpl[[legend.shapes]]),
						  list_hist_misc)
					} else NULL
				} else NULL
			})
			c(y[!vapply(y, is.null, logical(1))], yhist[!vapply(yhist, is.null, logical(1))])
		})
		#x <- c(x, xadd)
	
		legelem <- c(do.call("c", x), xadd)
	} else legelem <- list(NULL)
	
	if (all(vapply(legelem, is.null, logical(1)))) {
		return(NULL)
	} else {
		legelem
	}
}