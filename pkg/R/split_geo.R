split_geo <- function(gp, nx) {
	
	gpnx <- lapply(1:nx, function(i){
		g <- lapply(gp, function(x) {
			x$fill <- get_i(x$fill, i)
			x$choro.values <- get_i(x$choro.values, i)
			x$choro.legend.labels <- get_i(x$choro.legend.labels, i)
			x$choro.legend.palette <- get_i(x$choro.legend.palette, i)
			x$choro.breaks <- get_i(x$choro.breaks, i)
			x$bubble.size <- get_i(x$bubble.size, i)
			x$bubble.col <- get_i(x$bubble.col, i)
			x$bubble.legend.labels <- get_i(x$bubble.legend.labels, i)
			x$bubble.legend.palette <- get_i(x$bubble.legend.palette, i)
			x$bubble.legend.sizes <- get_i(x$bubble.legend.sizes, i)
			x$bubble.legend.size_labels <- get_i(x$bubble.legend.size_labels, i)
			x$bubble.xmod <- if(length(x$bubble.xmod) >= i) x$bubble.xmod[i] else x$bubble.xmod[1]
			x$bubble.ymod <- if(length(x$bubble.ymod) >= i) x$bubble.ymod[i] else x$bubble.ymod[1]
			x$line.col <- get_i(x$line.col, i)
			x$line.legend.labels <- get_i(x$line.legend.labels, i)
			x$line.legend.palette <- get_i(x$line.legend.palette, i)
			x$text <- if(length(x$text) >= i) x$text[i] else x$text[1]
			x$text.cex <- if(length(x$text.cex) >= i) x$text.cex[i] else x$text.cex[1]
			x$text.xmod <- if(length(x$text.xmod) >= i) x$text.xmod[i] else x$text.xmod[1]
			x$text.ymod <- if(length(x$text.ymod) >= i) x$text.ymod[i] else x$text.ymod[1]
			x
		})
	})
	names(gpnx) <- paste0("plot", 1:nx)
	
	gpnx
}

get_i <- function(x, i) {
	if (is.null(x)) {
		(NULL)
	} else if (is.matrix(x)) {
		if (ncol(x)>=i) x[,i] else x[,1]
	} else if(is.list(x)) {
		if (length(x)>=i) x[[i]] else x[[1]]
	} else x
}
