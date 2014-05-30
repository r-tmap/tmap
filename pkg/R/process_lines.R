process_lines <- function(data, g, free.scales.line.col, legend.digits, legend.NA.text) {
	npol <- nrow(data)
	x <- g$lines.col
	w <- g$lines.lwd
	
	nx <- length(x)
	nw <- length(w)
	
	shpcols <- names(data)
	
	if (nx==1 && valid_colors(x)[1]) {
		line.col=rep(x, length.out=npol)
		xline <- NA
		xlinelwd <- NA
		line.legend.labels=NA
		line.legend.palette=NA
	} else {
		palette <- g$palette
		by <- g$lines.by
	
		if (is.null(palette)) palette <- "Dark2"
	
		if (!all(x %in% shpcols)) stop("Incorrect lines col argument")
		
		if (g$lines.by) {
			dat <- data[[x[1]]]
			if (!is.factor(dat)) dat <- factor(dat)
			colsLeg <- cat2pal(dat,
							   palette = palette,
							   legend.NA.text=legend.NA.text,
							   max_levels=nlevels(dat))
			cols <- colsLeg[[3]]
			xline <- colsLeg[[2]]
			line.col <- matrix(NA, ncol=length(xline), nrow=length(xline))
			diag(line.col) <- cols
			ids <- as.integer(dat)
			ids[is.na(ids)] <- length(xline)
			line.col <- line.col[ids, ]
			line.legend.labels <- NA
			line.legend.palette <- NA
			
			
		} else {
			xline <- x
			X <- unlist(data[, x])
			colsLeg <- cat2pal(X,
							   palette = palette,
							   legend.NA.text=legend.NA.text,
							   max_levels=g$max.categories)
			line.col <- matrix(unlist(split(colsLeg[[1]], 
									   rep(1:nx, each=length(colsLeg[[1]])/nx))), ncol=nx)
			line.legend.labels <- colsLeg[[2]]
			line.legend.palette <- colsLeg[[3]]
		}
	}
	
	if (is.numeric(w)) {
		line.lwd <- rep(w, length.out=npol) * g$lines.scale
		xlinelwd <- NA
		line.legend.lwds <- NA
		line.legend.lwd_labels <- NA
	} else {
		if (!all(w %in% shpcols)) stop("Incorrect lines lwd argument")
		xlinelwd <- w
		W <- unlist(data[, w])
		w_legend <- pretty(W, 7)
		w_legend <- w_legend[w_legend!=0]
		w_legend <- w_legend[-c(length(w_legend)-3,length(w_legend)-1)]
		maxW <- max(W, na.rm=TRUE)
		line.legend.lwds <-  g$lines.scale * (w_legend/maxW)
		line.legend.lwd_labels <- format(w_legend, trim=TRUE)
		line.lwd <- matrix(g$lines.scale * (W/maxW), ncol=nw)
		
	}
	
	list(line.col=line.col,
		 line.lwd=line.lwd,
		 line.lty=g$lines.lty,
		 line.col.legend.labels=line.legend.labels,
		 line.col.legend.palette=line.legend.palette,
		 line.col.legend.misc=list(line.legend.lwd=line.lwd[1], line.legend.lty=g$lines.lty),
		 line.lwd.legend.labels=line.legend.lwd_labels,
		 line.lwd.legend.palette=ifelse(is.na(line.legend.palette[1]), line.col[1], line.legend.palette[1]),
		 line.lwd.legend.misc=list(legend.lwds=line.legend.lwds),
		 xline=xline,
		 xlinelwd=xlinelwd)

}


