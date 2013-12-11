process_bubblemap <- function(g, free.scales) {
	shp.name <- g$coor
	xsize <- g$bubble.size
	xcol <- g$bubble.col
	bubble.border <- g$bubble.border
	scale <- g$bubble.scale
	n <- g$n
	style <- g$style
	breaks <- g$breaks
	palette <- g$palette
	labels <- g$labels
	auto.palette.mapping <- g$auto.palette.mapping
	contrast <- g$contrast
	colorNA <- g$colorNA
	
	shpcols <- names(get(shp.name))
	
	varysize <- all(xsize %in% shpcols) && !is.null(xsize)
	varycol <- all(xcol %in% shpcols) && !is.null(xcol)
	
	if (is.null(xsize)) xsize <- 1
	if (is.null(xcol)) xcol <- "red"
	if (is.null(bubble.border)) bubble.border <- NA
	
	if (!varysize && !varycol) stop("Bubblemap needs to map a variable to either size or color (or both). Use geo.bubbles otherwise.")
	
	nx <- ifelse(varysize, length(xsize), length(xcol))
	
	if (varysize) {
		Xsize <- get(shp.name)@data[, xsize, drop=FALSE]
		if (any(Xsize<0)) stop("Bubble size variable contains negative values")
	} else {
		bubble.size <- xsize
		bubble.legend.sizes <- NA
		bubble.legend.size_labels <- NA
	}
	if (varycol) {
		Xcol <- get(shp.name)@data[, xcol, drop=FALSE]
	} else {
		bubble.col <- xcol
		bubble.legend.labels <- NA
		bubble.legend.palette <- NA
	}
	
	if (free.scales && nx > 1) {
		if (varysize) {
			bubble.size <- matrix(0, ncol=nx, nrow=nrow(Xsize))
			bubble.legend.sizes <- list()
			bubble.legend.size_labels <- list()
		}
		if (varycol) {
			bubble.col <- matrix("", ncol=nx, nrow=nrow(Xcol))
			bubble.legend.labels <- list()
			bubble.legend.palette <- list()
		}
		for (i in 1:nx) {
			if (varysize) {
				x_legend <- pretty(Xsize[[i]], 7)
				x_legend <- x_legend[x_legend!=0]
				x_legend <- x_legend[-c(length(x_legend)-3,length(x_legend)-1)]
				maxX <- max(Xsize[[i]], na.rm=TRUE)
				bubble.legend.sizes[[i]] <- scale*sqrt(x_legend/maxX)
				bubble.legend.size_labels[[i]] <- format(x_legend, trim=TRUE)
				bubble.size[,i] <- scale*sqrt(Xsize[[i]]/maxM)
			} else {
				xsize <- NA
			}
			if (varycol) {
				
				dat <- Xcol[[i]]
				if (is.numeric(dat)) {
					if (is.null(palette)) palette <- "Blues"
					colsLeg <- num2pal(dat, n, style=style, breaks=breaks, 
									   palette = palette,
									   auto.palette.mapping = auto.palette.mapping,
									   contrast = contrast, legend.labels=labels)
				} else {
					if (is.null(palette)) palette <- "Set3"
					colsLeg <- cat2pal(dat,
									   palette = palette,
									   colorNA = colorNA)
					
				}
				bubble.col[,i] <- colsLeg[[1]]
				bubble.legend.labels[[i]] <- colsLeg[[2]]
				bubble.legend.palette[[i]] <- colsLeg[[3]]
			} else {
				xcol <- NA
			}
		}
	} else {
		if (varysize) {
			dat <- unlist(Xsize)
			x_legend <- pretty(dat, 7)
			x_legend <- x_legend[x_legend!=0]
			x_legend <- x_legend[-c(length(x_legend)-3,length(x_legend)-1)]
			maxX <- max(dat, na.rm=TRUE)
			bubble.legend.sizes <- scale*sqrt(x_legend/maxX)
			bubble.legend.size_labels <- format(x_legend, trim=TRUE)
			bubble.size <- matrix(scale*sqrt(dat/maxX), ncol=nx)
		} else {
			xsize <- NA
		}
		if (varycol) {
			dat <- unlist(Xcol)
			
			if (is.numeric(dat)) {
				if (is.null(palette)) palette <- "Blues"
				colsLeg <- num2pal(dat, n, style=style, breaks=breaks, 
								   palette = palette,
								   auto.palette.mapping = auto.palette.mapping,
								   contrast = contrast, legend.labels=labels)
			} else {
				if (is.null(palette)) palette <- "Set3"
				colsLeg <- cat2pal(dat,
								   palette = palette,
								   colorNA = colorNA)
				
			}
			bubble.col <- matrix(unlist(split(colsLeg[[1]], 
											  rep(1:nx, each=length(colsLeg[[1]])/nx))), ncol=nx)
			bubble.legend.labels <- colsLeg[[2]]
			bubble.legend.palette <- colsLeg[[3]]
		} else {
			xcol <- NA
		}
	}
	list(bubble.size=bubble.size,
		 bubble.col=bubble.col,
		 bubble.border=bubble.border,
		 bubble.scale=scale,
		 bubble.legend.labels=bubble.legend.labels,
		 bubble.legend.palette=bubble.legend.palette,
		 bubble.legend.sizes=bubble.legend.sizes,
		 bubble.legend.size_labels=bubble.legend.size_labels,
		 xsize=xsize,
		 xcol=xcol,
		 shp.name=shp.name)
}
