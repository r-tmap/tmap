process_text_size_vector <- function(x, text, g, rescale, gt) {
	
	if (is.null(g$sizes.legend)) {
		x_legend <- pretty(x, 5)
		x_legend <- x_legend[x_legend!=0]
		nxl <- length(x_legend)
		if (nxl>5) x_legend <- x_legend[-c(nxl-3, nxl-1)]
	} else {
		x_legend <- g$sizes.legend
	}
	
	if (is.null(g$sizes.legend.labels)) {
		size.legend.labels <- do.call("fancy_breaks", c(list(vec=x_legend, intervals=FALSE), g$legend.format))
	} else {
		if (length(g$sizes.legend.labels) != length(x_legend)) stop("length of sizes.legend.labels is not equal to the number of texts in the legend")
		size.legend.labels <- g$sizes.legend.labels
	}
	
	root <- ifelse(rescale, g$root, 1)
	
	maxX <- ifelse(rescale, max(x, na.rm=TRUE), 1)
	size <- (x / maxX) ^ (1/root)
	
	max.size <- max(size, na.rm=TRUE)
	legend.sizes <- (x_legend/maxX) ^ (1/root)
	
	
	text_sel <- (size >= g$size.lowerbound)
	text_empty <- is.na(text) | is.na(size)
	
	if (g$print.tiny) {
		size[!text_sel & !text_empty] <- size.lowerbound
		text_sel <- !text_empty
	} else {
		text_sel <- text_sel & !text_empty
	}
	
	size <- size * g$scale
	max.size <- max.size * g$scale
	legend.sizes <- legend.sizes * g$scale

	list(size=size,
		 text_sel=text_sel,
		 size.legend.labels=size.legend.labels,
		 legend.sizes=legend.sizes,
		 max.size=max.size)
}

process_text_col_vector <- function(xc, xs, g, gt) {
	col.is.numeric <- is.numeric(xc)
	if (col.is.numeric) {
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
						   process.colors=c(list(alpha=g$alpha), gt$pc),
						   legend.format=g$legend.format)
		col <- colsLeg[[1]]
		col.neutral <- colsLeg$legend.neutral.col
		breaks <- colsLeg[[4]]
	} else {
		palette <- if (is.null(g$palette)) {
			gt$aes.palette[[ifelse(is.ordered(xc), "seq", "cat")]] 
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
						   process.colors=c(list(alpha=g$alpha), gt$pc))
		
		col <- rep(NA, length(sel))
		col[sel] <- colsLeg[[1]]
		col.neutral <- col[sel[1]]
		breaks <- NA
	}
	col.legend.labels <- colsLeg[[2]]
	col.legend.palette <- colsLeg[[3]]
	
	list(col=col,
		 col.legend.labels=col.legend.labels,
		 col.legend.palette=col.legend.palette,
		 col.is.numeric=col.is.numeric,
		 col.neutral=col.neutral,
		 breaks=breaks)
}



process_text <- function(data, g, fill, gt, gby, z) {
	root <- NULL; size.lowerbound <- NULL; scale <- NULL; bg.alpha <- NULL; case <- NULL; alpha <- NULL
	shadow <- NULL
	
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
	
	xtsize <- g$size
	xtcol <- g$color
	xtext <- g$text
	
	# if by is specified, use first value only
	if (nlevels(by)>1) {
		xtsize <- xtsize[1]
		xtcol <- xtcol[1]
		xtext <- xtext[1]
	}
	nxtsize <- length(xtsize)
	nxtcol <- length(xtcol)
	nxtext <- length(xtext)
	
	varysize <- all(xtsize %in% shpcols) && !is.null(xtsize)
	varycol <- all(xtcol %in% shpcols) && !is.null(xtcol) && !(is.na(xtcol[1]))
	
	nx <- max(nxtcol, nxtsize, nxtext)
	if (nxtcol<nx) xtcol <- rep(xtcol, length.out=nx)
	if (nxtsize<nx) xtsize <- rep(xtsize, length.out=nx)
	if (nxtext<nx) xtext <- rep(xtext, length.out=nx)
	
	if (!varysize) {
		if (!all(is.numeric(xtsize) | xtsize=="AREA")) stop("Incorrect text sizes.")
		if (is.numeric(xtsize[1])) {
			g$size.lowerbound <- 0
		}
		for (i in 1:nx) data[[paste("SIZE", i, sep="_")]] <- if (is.numeric(xtsize[i])) xtsize[i] else {
			tmp <- data$SHAPE_AREAS
			(tmp / max(tmp, na.rm=TRUE)) ^ (1/g$root)
		}
		xtsize <- paste("SIZE", 1:nx, sep="_")
		gby$free.scales.size <- FALSE
	}
	
	# check for direct color input
	is.colors <- all(valid_colors(xtcol)) || is.na(xtcol[1])
	if (!varycol) {
		if (!is.colors) stop("Invalid text colors")
		if (is.na(xtcol)[1]) {
			if (is.matrix(fill)) {
				cols <- apply(fill, MARGIN=2, function(f) {
					light <- is_light(f)
					rep(ifelse(light, coldark, collight), length.out=npol)
				})
				cols <- apply(cols, MARGIN=2, function(cl) {
					do.call("process_color", c(list(col=col2hex(cl), alpha=g$alpha), gt$pc))
				})
			} else {
				light <- is_light(fill)
				cols <- rep(ifelse(light, coldark, collight), length.out=npol)
				cols <- do.call("process_color", c(list(col=col2hex(cols), alpha=g$alpha), gt$pc))
			}
		} else {
			colvec <- do.call("process_color", c(list(col=col2hex(xtcol), alpha=g$alpha), gt$pc))
			cols <- matrix(colvec, nrow=npol, ncol=nx, byrow = TRUE)
		}
		if (!is.matrix(cols)) {
			cols <- matrix(cols, ncol=nx)
		} else {
			if (ncol(cols)!=nx) {
				cols <- cols[,rep(1:ncol(cols), length.out=nx)]
			}
		}

		for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- cols[, i]
		xtcol <- paste("COLOR", 1:nx, sep="_")
	}
	
	nx <- max(nx, nlevels(by))
	
	dtcol <- process_data(data[, xtcol, drop=FALSE], by=by, free.scales=gby$free.scales.text.col, is.colors=is.colors)	
	dtsize <- process_data(data[, xtsize, drop=FALSE], by=by, free.scales=gby$free.scales.text.size, is.colors=FALSE)
	
	##
	if (!all(xtext %in% shpcols)) stop("Incorrect data variable used for the text")

	text <- if (nx > 1) matrix(unlist(lapply(data[, xtext], as.character)), ncol=nx) else as.character(data[[xtext]])
	if (!is.na(g$case)) text <- if(case=="upper") toupper(text) else tolower(text)
	
	
	if (is.list(dtsize)) {
		# multiple variables for size are defined
		gss <- split_g(g, n=nx)
		res <- mapply(process_text_size_vector, dtsize, as.list(as.data.frame(text)), gss, MoreArgs = list(rescale=varysize, gt), SIMPLIFY = FALSE)
		size <- sapply(res, function(r)r$size)
		text_sel <- sapply(res, function(r)r$text_sel)
		size.legend.labels <- lapply(res, function(r)r$size.legend.labels)
		legend.sizes <- lapply(res, function(r)r$legend.sizes)
		max.size <- sapply(res, function(r)r$max.size)
	} else {
		res <- process_text_size_vector(dtsize, text, g, rescale=varysize, gt)
		size <- matrix(res$size, nrow=npol)
		text_sel <- matrix(res$text_sel, nrow=npol)
		
		if (varysize) {
			size.legend.labels <- res$size.legend.labels
			legend.sizes <- res$legend.sizes
			max.size <- res$max.size
		} else {
			size.legend.labels <- NA
			legend.sizes <- NA
			max.size <- res$max.size
			xtsize <- rep(NA, nx)
			size.legend.title <- rep(NA, nx)
		}
	}
	
	if (is.matrix(dtcol)) {
		col <- dtcol
		xtcol <- rep(NA, nx)
		col.legend.title <- rep(NA, nx)
		col.legend.labels <- NA
		col.legend.palette <- NA
		col.is.numeric <- NA
		col.neutral <- apply(col, 2, function(bc) na.omit(bc)[1])
		breaks <- NA
		values <- NA
	} else if (is.list(dtcol)) {
		# multiple variables for col are defined
		gsc <- split_g(g, n=nx)
		size_list <- as.list(as.data.frame(size))
		res <- mapply(process_text_col_vector, dtcol, size_list, gsc, MoreArgs=list(gt), SIMPLIFY=FALSE)
		col <- sapply(res, function(r)r$col)
		col.legend.labels <- lapply(res, function(r)r$col.legend.labels)
		col.legend.palette <- lapply(res, function(r)r$col.legend.palette)
		col.is.numeric <- sapply(res, function(r)r$col.is.numeric)
		col.neutral <- sapply(res, function(r)r$col.neutral)
		breaks <- lapply(res, function(r)r$breaks)
		values <- dtcol
	} else {
		size_vector <- unlist(size)
		res <- process_text_col_vector(dtcol, size_vector, g, gt)
		col <- matrix(res$col, nrow=npol)
		col.legend.labels <- res$col.legend.labels
		col.legend.palette <- res$col.legend.palette
		col.is.numeric <- res$col.is.numeric
		col.neutral <- res$col.neutral
		breaks <- res$breaks
		values <- split(dtcol, rep(1:nx, each=npol))
	}
	

	if (g$shadow) {
		g$shadowcol <- if (is.matrix(col)) {
			apply(col, MARGIN=2, function(f) {
				light <- is_light(f)
				rep(ifelse(light, coldark, collight), length.out=npol)
			})
		} else {
			light <- is_light(col)
			rep(ifelse(light, coldark, collight), length.out=npol)
		}
	}
	
	xmod <- if (is.character(g$xmod)) data[[g$xmod]] else rep(g$xmod, length.out=npol)
	ymod <-  if (is.character(g$ymod)) data[[g$ymod]] else rep(g$ymod, length.out=npol)
	

	if (is.na(g$fontface)) g$fontface <- gt$fontface
	if (is.na(g$fontfamily)) g$fontfamily <- gt$fontfamily
	
	size.legend.palette <- col.neutral
	
	text.size.legend.title <- if (is.na(g$title.size)[1]) xtsize else g$title.size
	text.col.legend.title <- if (is.na(g$title.col)[1]) xtcol else g$title.col
	text.size.legend.z <- if (is.na(g$legend.size.z)) z else g$legend.size.z
	text.col.legend.z <- if (is.na(g$legend.col.z)) z+.33 else g$legend.col.z
	text.legend.hist.z <- if (is.na(g$legend.hist.z)) z+.66 else g$legend.hist.z
	
	if (g$legend.hist && is.na(g$legend.hist.title) && text.col.legend.z>text.legend.hist.z) {
		# histogram is drawn between title and legend enumeration
		text.col.legend.hist.title <- text.col.legend.title
		text.col.legend.title <- ""
	} else if (g$legend.hist && !is.na(g$legend.hist.title)) {
		text.col.legend.hist.title <- g$legend.hist.title
	} else text.col.legend.hist.title <- ""
	
	
	list(text=text,
		 text.size=size,
		 #root=g$root,
		 text.color=col,
		 text.fontface=g$fontface,
		 text.fontfamily=g$fontfamily,
		 text.shadow=g$shadow,
		 text.shadowcol=g$shadowcol,
		 text.bg.color=g$bg.color,
		 text.bg.alpha=g$bg.alpha,
		 text.scale=g$scale,
		 text.auto.placement=g$auto.placement,
		 text.remove.overlap=g$remove.overlap,
		 text.along.lines=g$along.lines,
		 text.overwrite.lines=g$overwrite.lines,
		 text.col.legend.labels=col.legend.labels,
		 text.col.legend.palette=col.legend.palette,
		 text.col.legend.misc=list(text.max.size=max.size),
		 text.size.legend.labels=size.legend.labels,
		 text.size.legend.palette= size.legend.palette,
		 text.size.legend.misc=list(legend.sizes=legend.sizes),
		 text.col.legend.hist.misc=list(values=values, breaks=breaks),
		 xtsize=xtsize,
		 xtcol=xtcol,
		 text.xmod=xmod,
		 text.ymod=ymod,
		 text_sel=text_sel,
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
		 text.id=g$id)
	
}