process_text_size_vector <- function(x, text, g, rescale, gt) {
	text_sel <- (x >= g$text.size.lowerbound)
	text_empty <- is.na(text) | is.na(x)
	
	if (g$text.print.tiny) {
		text.size[!text_sel & !text_empty] <- text.size.lowerbound
		text_sel <- !text_empty
	} else {
		text_sel <- text_sel & !text_empty
	}
	
	if (is.null(g$sizes.legend)) {
		x_legend <- pretty(x, 5)
		x_legend <- x_legend[x_legend!=0]
		nxl <- length(x_legend)
		if (nxl>5) x_legend <- x_legend[-c(nxl-3, nxl-1)]
	} else {
		x_legend <- g$sizes.legend
	}
	
	if (is.null(g$sizes.legend.labels)) {
		text.size.legend.labels <- do.call("fancy_breaks", c(list(vec=x_legend, intervals=FALSE), g$legend.format))
	} else {
		if (length(g$sizes.legend.labels) != length(x_legend)) stop("length of sizes.legend.labels is not equal to the number of texts in the legend")
		text.size.legend.labels <- g$sizes.legend.labels
	}
	
	maxX <- ifelse(rescale, max(x, na.rm=TRUE), 1)
	text.size <- g$text.scale*(x / maxX) ^ (1/g$root)
	
	text.max.size <- max(text.size, na.rm=TRUE)
	text.legend.sizes <- g$text.scale*(x_legend/maxX) ^ (1/g$root)
	list(text.size=text.size,
		 text_sel=text_sel,
		 text.size.legend.labels=text.size.legend.labels,
		 text.legend.sizes=text.legend.sizes,
		 text.max.size=text.max.size)
}

process_text_col_vector <- function(xc, xs, g, gt) {
	text.col.is.numeric <- is.numeric(xc)
	if (text.col.is.numeric) {
		is.diverging <- (any(na.omit(xc)<0) || any(g$breaks<0)) && (any(na.omit(xc)>0) || any(g$breaks>0))
		
		palette <- if (is.null(g$palette)) {
			gt$aes.palette[[ifelse(is.diverging, "div", "seq")]] 
		} else if (g$palette[1] %in% c("seq", "div", "cat")) {
			gt$aes.palette[[g$palette[1]]]
		} else g$palette
		colsLeg <- num2pal(xc, g$n, style=g$style, breaks=g$breaks, 
						   palette = palette,
						   auto.palette.mapping = g$auto.palette.mapping,
						   contrast = g$contrast, legend.labels=g$labels,
						   colorNA=g$colorNA, 
						   legend.NA.text=g$textNA,
						   process.colors=c(list(alpha=g$text.alpha), gt$pc),
						   legend.format=g$legend.format)
		text.col <- colsLeg[[1]]
		text.col.neutral <- colsLeg$legend.neutral.col
		text.breaks <- colsLeg[[4]]
	} else {
		palette <- if (is.null(g$palette)) {
			gt$aes.palette[[ifelse(is.ordered(x), "seq", "cat")]] 
		} else if (g$palette[1] %in% c("seq", "div", "cat")) {
			gt$aes.palette[[g$palette[1]]]
		} else g$palette
		#remove unused levels in legend
		sel <- !is.na(xs)
		colsLeg <- cat2pal(xc[sel],
						   palette = palette,
						   contrast = g$contrast,
						   colorNA = g$colorNA,
						   legend.labels=g$labels,
						   legend.NA.text=g$textNA,
						   max_levels=g$max.categories,
						   process.colors=c(list(alpha=g$text.alpha), gt$pc))
		
		text.col <- rep(NA, length(sel))
		text.col[sel] <- colsLeg[[1]]
		text.col.neutral <- text.col[sel[1]]
		text.breaks <- NA
	}
	text.col.legend.labels <- colsLeg[[2]]
	text.col.legend.palette <- colsLeg[[3]]
	
	list(text.col=text.col,
		 text.col.legend.labels=text.col.legend.labels,
		 text.col.legend.palette=text.col.legend.palette,
		 text.col.is.numeric=text.col.is.numeric,
		 text.col.neutral=text.col.neutral,
		 text.breaks=text.breaks)
}



process_text <- function(data, g, fill, gt, gby, z) {
	root <- NULL; text.size.lowerbound <- NULL; text.scale <- NULL; text.bg.alpha <- NULL; text.case <- NULL; text.alpha <- NULL
	text.shadow <- NULL
	
	npol <- nrow(data)

	if (gt$aes.colors.light["text"]) {
		collight <- gt$aes.colors["text"]
		coldark <- "black"
	} else {
		collight <- "white"
		coldark <- gt$aes.colors["text"]
	}
	
	if (is.na(fill[1])) fill <- ifelse(gt$aes.colors.light["text"], "black", "white")
	
	
	
	
	
	by <- data$GROUP_BY
	shpcols <- names(data)[1:(ncol(data)-1)]
	
	# update legend format from tm_layout
	to_be_assigned <- setdiff(names(gt$legend.format), names(g$legend.format))
	g$legend.format[to_be_assigned] <- gt$legend.format[to_be_assigned]
	
	xsize <- g$text.size
	xcol <- g$text.fontcolor
	xtext <- g$text
	
	# if by is specified, use first value only
	if (nlevels(by)>1) {
		xsize <- xsize[1]
		xcol <- xcol[1]
		xtext <- xtext[1]
	}
	nxsize <- length(xsize)
	nxcol <- length(xcol)
	nxtext <- length(xtext)
	
	varysize <- all(xsize %in% shpcols) && !is.null(xsize)
	varycol <- all(xcol %in% shpcols) && !is.null(xcol) && !(is.na(xcol[1]))
	
	nx <- max(nxcol, nxsize, nxtext)
	if (nxcol<nx) xcol <- rep(xcol, length.out=nx)
	if (nxsize<nx) xsize <- rep(xsize, length.out=nx)
	if (nxtext<nx) xtext <- rep(xtext, length.out=nx)
	
	if (!varysize) {
		if (!all(is.numeric(xsize) | xsize=="AREA")) stop("Incorrect text sizes.")
		if (is.numeric(xsize[1])) {
			g$text.size.lowerbound <- 0
		}
		for (i in 1:nx) data[[paste("SIZE", i, sep="_")]] <- if (is.numeric(xsize[i])) xsize[i] else {
			tmp <- data$SHAPE_AREAS
			(tmp / max(tmp, na.rm=TRUE)) ^ (1/g$root)
		}
		xsize <- paste("SIZE", 1:nx, sep="_")
		gby$free.scales.text.size <- FALSE
	}
	
	# check for direct color input
	is.colors <- all(valid_colors(xcol)) || is.na(xcol[1])
	if (!varycol) {
		if (!is.colors) stop("Invalid text colors")
		if (is.na(xcol)[1]) {
			if (is.matrix(fill)) {
				cols <- apply(fill, MARGIN=2, function(f) {
					light <- is_light(f)
					rep(ifelse(light, coldark, collight), length.out=npol)
				})
				cols <- apply(cols, MARGIN=2, function(cl) {
					do.call("process_color", c(list(col=col2hex(cl), alpha=g$text.alpha), gt$pc))
				})
			} else {
				light <- is_light(fill)
				cols <- rep(ifelse(light, coldark, collight), length.out=npol)
				cols <- do.call("process_color", c(list(col=col2hex(cols), alpha=g$text.alpha), gt$pc))
			}
		}
		if (!is.matrix(cols)) {
			cols <- matrix(cols, ncol=nx)
		} else {
			if (ncol(cols)!=nx) {
				cols <- cols[,rep(1:ncol(cols), length.out=nx)]
			}
		}

		for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- cols[, i]
		xcol <- paste("COLOR", 1:nx, sep="_")
	}
	
	nx <- max(nx, nlevels(by))
	
	dtcol <- process_data(data[, xcol, drop=FALSE], by=by, free.scales=gby$free.scales.text.col, is.colors=is.colors)	
	dtsize <- process_data(data[, xsize, drop=FALSE], by=by, free.scales=gby$free.scales.text.size, is.colors=FALSE)
	
	##
	if (!all(xtext %in% shpcols)) stop("Incorrect data variable used for the text")

	text <- if (nx > 1) matrix(unlist(lapply(data[, xtext]), as.character), ncol=nx) else as.character(data[[xtext]])
	if (!is.na(g$text.case)) text <- if(text.case=="upper") toupper(text) else tolower(text)
	
	
	if (is.list(dtsize)) {
		# multiple variables for size are defined
		gss <- split_g(g, n=nx)
		res <- mapply(process_text_size_vector, dtsize, as.list(as.data.frame(text)), gss, MoreArgs = list(rescale=varysize, gt), SIMPLIFY = FALSE)
		text.size <- sapply(res, function(r)r$text.size)
		text_sel <- sapply(res, function(r)r$text_sel)
		text.size.legend.labels <- lapply(res, function(r)r$text.size.legend.labels)
		text.legend.sizes <- lapply(res, function(r)r$text.legend.sizes)
		text.max.size <- sapply(res, function(r)r$text.max.size)
	} else {
		res <- process_text_size_vector(dtsize, text, g, rescale=varysize, gt)
		text.size <- matrix(res$text.size, nrow=npol)
		text_sel <- matrix(res$text_sel, nrow=npol)
		
		if (varysize) {
			text.size.legend.labels <- res$text.size.legend.labels
			text.legend.sizes <- res$text.legend.sizes
			text.max.size <- res$text.max.size
		} else {
			text.size.legend.labels <- NA
			text.legend.sizes <- NA
			text.max.size <- res$text.max.size
			xsize <- rep(NA, nx)
			text.size.legend.title <- rep(NA, nx)
		}
	}
	
	if (is.matrix(dtcol)) {
		text.col <- if (!is.colors) {
			matrix(do.call("process_color", c(list(col=dtcol, alpha=g$text.alpha), gt$pc)),
				   ncol=ncol(dtcol))
		} else dtcol
		xcol <- rep(NA, nx)
		text.col.legend.title <- rep(NA, nx)
		text.col.legend.labels <- NA
		text.col.legend.palette <- NA
		text.col.is.numeric <- NA
		text.col.neutral <- apply(text.col, 2, function(bc) na.omit(bc)[1])
		text.breaks <- NA
		text.values <- NA
	} else if (is.list(dtcol)) {
		# multiple variables for col are defined
		gsc <- split_g(g, n=nx)
		text.size_list <- as.list(as.data.frame(text.size))
		res <- mapply(process_texts_col_vector, dtcol, text.size_list, gsc, MoreArgs=list(gt), SIMPLIFY=FALSE)
		text.col <- sapply(res, function(r)r$text.col)
		text.col.legend.labels <- lapply(res, function(r)r$text.col.legend.labels)
		text.col.legend.palette <- lapply(res, function(r)r$text.col.legend.palette)
		text.col.is.numeric <- sapply(res, function(r)r$text.col.is.numeric)
		text.col.neutral <- sapply(res, function(r)r$text.col.neutral)
		text.breaks <- lapply(res, function(r)r$text.breaks)
		text.values <- dtcol
	} else {
		text.size_vector <- unlist(text.size)
		res <- process_texts_col_vector(dtcol, text.size_vector, g, gt)
		text.col <- matrix(res$text.col, nrow=npol)
		text.col.legend.labels <- res$text.col.legend.labels
		text.col.legend.palette <- res$text.col.legend.palette
		text.col.is.numeric <- res$text.col.is.numeric
		text.col.neutral <- res$text.col.neutral
		text.breaks <- res$text.breaks
		text.values <- split(dtcol, rep(1:nx, each=npol))
	}
	

	if (g$text.shadow) {
		g$text.shadowcol <- if (is.matrix(text.color)) {
			apply(text.color, MARGIN=2, function(f) {
				light <- is_light(f)
				rep(ifelse(light, coldark, collight), length.out=npol)
			})
		} else {
			light <- is_light(text.color)
			rep(ifelse(light, coldark, collight), length.out=npol)
		}
	}
	
	g$text.xmod <- if (is.character(g$text.xmod)) data[[g$text.xmod]] else rep(g$text.xmod, length.out=npol)
	g$text.ymod <-  if (is.character(g$text.ymod)) data[[g$text.ymod]] else rep(g$text.ymod, length.out=npol)
	

	if (is.na(g$text.fontface)) g$text.fontface <- gt$fontface
	if (is.na(g$text.fontfamily)) g$text.fontfamily <- gt$fontfamily
	list(text=text,
		 text.size=text.size,
		 root=g$root,
		 text.fontcolorcol=text.col,
		 text.fontface=g$text.fontface,
		 text.fontfamily=g$text.fontfamily,
		 text.shadow=g$text.shadow,
		 text.shadowcol=g$text.shadowcol,
		 text.bg.color=g$text.bg.color,
		 text.bg.alpha=g$text.bg.alpha,
		 text.scale=g$text.scale,
		 text.col.legend.labels=text.col.legend.labels,
		 text.col.legend.palette=text.col.legend.palette,
		 text.col.legend.misc=list(text.border.lwd=g$text.border.lwd, text.border.col=text.border.col, text.max.size=text.max.size),
		 text.size.legend.labels=text.size.legend.labels,
		 text.size.legend.palette= text.size.legend.palette,
		 text.size.legend.misc=list(text.border.lwd=g$text.border.lwd, text.border.col=text.border.col, legend.sizes=text.legend.sizes),
		 text.col.legend.hist.misc=list(values=text.values, breaks=text.breaks),
		 xsize=xsize,
		 xcol=xcol,
		 text.xmod=xmod,
		 text.ymod=ymod,
		 text.size.legend.show=g$legend.size.show,
		 text.col.legend.show=g$legend.col.show,
		 text.size.legend.title=text.size.legend.title,
		 text.col.legend.title=text.col.legend.title,
		 text.size.legend.is.portrait=g$legend.size.is.portrait,
		 text.col.legend.is.portrait=g$legend.col.is.portrait,
		 text.col.legend.hist=g$legend.hist,
		 text.col.legend.hist.title=text.col.legend.hist.title,
		 text.size.legend.z=text.size.legend.z,
		 text.col.legend.z=text.col.legend.z,
		 text.col.legend.hist.z=text.legend.hist.z,
		 text.id=g$text.id)
	
}