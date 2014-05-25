process_bubbles <- function(data, g, free.scales.size, free.scales.col, legend.digits, legend.NA.text) {
	
	xsize <- g$bubble.size
	xcol <- g$bubble.col
	
	if (is.null(xsize)) return(list(
		bubble.size=NULL,
		bubble.legend.labels=NA,
		bubble.legend.palette=NA,
		bubble.legend.sizes=NA,
		bubble.legend.size_labels=NA,
		xsize=NA,
		xcol=NA))
	
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
	shpcols <- names(data)
	
	varysize <- all(xsize %in% shpcols) && !is.null(xsize)
	varycol <- all(xcol %in% shpcols) && !is.null(xcol)
	
	if (is.null(bubble.border)) bubble.border <- NA
	
	if (!varysize && !varycol) {
		return(c(g, list(
			bubble.size=NULL,
			bubble.legend.labels=NA,
			bubble.legend.palette=NA,
			bubble.legend.sizes=NA,
			bubble.legend.size_labels=NA,
			xsize=NA,
			xcol=NA)))
	}
		
	nxsize <- ifelse(varysize, length(xsize), 1)
	nxcol <- ifelse(varycol, length(xcol), 1)
	
	if (varysize) {
		Xsize <- data[, xsize, drop=FALSE]
		if (any(na.omit(Xsize)<0)) stop("Bubble size variable contains negative values")
	} else {
		bubble.size <- scale*xsize
		bubble.legend.sizes <- NA
		bubble.legend.size_labels <- NA
	}
	if (varycol) {
		Xcol <- data[, xcol, drop=FALSE]
	} else {
		bubble.col <- xcol
		bubble.legend.labels <- NA
		bubble.legend.palette <- NA
	}
	
	if (free.scales.size && nxsize > 1) {
		if (varysize) {
			bubble.size <- matrix(0, ncol=nxsize, nrow=nrow(Xsize))
			bubble.legend.sizes <- list()
			bubble.legend.size_labels <- list()
		}
		for (i in 1:nxsize) {
			if (varysize) {
				x_legend <- pretty(Xsize[[i]], 7)
				x_legend <- x_legend[x_legend!=0]
				x_legend <- x_legend[-c(length(x_legend)-3,length(x_legend)-1)]
				maxX <- max(Xsize[[i]], na.rm=TRUE)
				bubble.legend.sizes[[i]] <- scale*sqrt(x_legend/maxX)
				bubble.legend.size_labels[[i]] <- format(x_legend, trim=TRUE)
				bubble.size[,i] <- scale*sqrt(Xsize[[i]]/maxX)
			} else {
				xsize <- NA
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
			bubble.size <- matrix(scale*sqrt(dat/maxX), ncol=nxsize)
		} else {
			xsize <- NA
		}
	}
	
	if (free.scales.col && nxcol > 1) {
		if (varycol) {
			bubble.col <- matrix("", ncol=nxcol, nrow=nrow(Xcol))
			bubble.legend.labels <- list()
			bubble.legend.palette <- list()
		}
		for (i in 1:nxcol) {
			if (varycol) {
				
				dat <- Xcol[[i]]
				
				bubble.col.is.numeric <- is.numeric(dat)
				
				if (bubble.col.is.numeric) {
					if (is.null(palette)) palette <- "Blues"
					colsLeg <- num2pal(dat, n, style=style, breaks=breaks, 
									   palette = palette,
									   auto.palette.mapping = auto.palette.mapping,
									   contrast = contrast, legend.labels=labels,
									   legend.digits=legend.digits,
									   legend.NA.text=legend.NA.text)
				} else {
					if (is.null(palette)) palette <- "Dark2"
					#remove unused levels in legend
					if (varysize) {
						sel <- apply(matrix(as.vector(bubble.size), nrow=nrow(Xcol)), MARGIN=1, function(x)any(!is.na(x)))
					}
					colsLeg <- cat2pal(dat[sel],
									   palette = palette,
									   colorNA = colorNA,
									   legend.NA.text=legend.NA.text)
					cols <- rep(NA, length(sel))
					cols[sel] <- colsLeg[[1]]
					colsLeg[[1]] <- cols
					
				}
				bubble.col[,i] <- colsLeg[[1]]
				bubble.legend.labels[[i]] <- colsLeg[[2]]
				bubble.legend.palette[[i]] <- colsLeg[[3]]
			} else {
				xcol <- NA
				bubble.col.is.numeric <- FALSE
			}
		}
	} else {
		if (varycol) {
			dat <- unlist(Xcol)
			bubble.col.is.numeric <- is.numeric(dat)
			
			if (bubble.col.is.numeric) {
				if (is.null(palette)) palette <- "Blues"
				colsLeg <- num2pal(dat, n, style=style, breaks=breaks, 
								   palette = palette,
								   auto.palette.mapping = auto.palette.mapping,
								   contrast = contrast, legend.labels=labels,
								   legend.digits=legend.digits,
								   legend.NA.text=legend.NA.text)
			} else {
				if (is.null(palette)) palette <- "Dark2"
				#remove unused levels in legend
				if (varysize) {
					sel <- apply(matrix(as.vector(bubble.size), nrow=length(dat)), MARGIN=1, function(x)any(!is.na(x)))
				} else {
					sel <- rep(TRUE, length(dat))
				}
				colsLeg <- cat2pal(dat[sel],
								   palette = palette,
								   colorNA = colorNA,
								   legend.NA.text=legend.NA.text)
				 
				cols <- rep(NA, length(sel))
				cols[sel] <- colsLeg[[1]]
				colsLeg[[1]] <- cols
				
			}
			bubble.col <- matrix(unlist(split(colsLeg[[1]], 
											  rep(1:nxcol, each=length(colsLeg[[1]])/nxcol))), ncol=nxcol)
			bubble.legend.labels <- colsLeg[[2]]
			bubble.legend.palette <- colsLeg[[3]]
		} else {
			xcol <- NA
			bubble.col.is.numeric <- FALSE
		}
	}
	
	
	xmod <- g$bubble.xmod
	ymod <- g$bubble.ymod
	if (is.character(xmod)) xmod <- data[[xmod]]
	if (is.character(ymod)) ymod <- data[[ymod]]
	
	list(bubble.size=bubble.size,
		 bubble.col=bubble.col,
		 bubble.border=bubble.border,
		 bubble.scale=scale,
		 bubble.legend.labels=bubble.legend.labels,
		 bubble.legend.palette=bubble.legend.palette,
		 bubble.legend.sizes=bubble.legend.sizes,
		 bubble.legend.size_labels=bubble.legend.size_labels,
		 bubble.col.is.numeric=bubble.col.is.numeric,
		 xsize=xsize,
		 xcol=xcol,
		 bubble.xmod=xmod,
		 bubble.ymod=ymod)
}
