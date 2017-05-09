process_text <- function(data, g, fill, gt, gby, z, interactive) {
	root <- NULL; size.lowerbound <- NULL; scale <- NULL; bg.alpha <- NULL; case <- NULL; alpha <- NULL
	shadow <- NULL
	gsc <- NULL
	
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
	
	xtsize <- g$size
	xtcol <- g$col
	xtext <- g$text
	
	# if (interactive) {
	# 	xtsize <- xtsize[1]
	# 	xtcol <- xtcol[1]
	# 	xtext <- xtext[1]
	# }

	if (is.null(g$colorNA)) g$colorNA <- "#00000000"
	if (is.na(g$colorNA)[1]) g$colorNA <- gt$aes.colors["na"]
	if (g$colorNA=="#00000000") g$showNA <- FALSE
	
	if (!is.na(g$alpha) && !is.numeric(g$alpha)) stop("alpha argument in tm_text is not a numeric", call. = FALSE)
	
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
	
	if ((varysize || identical(xtsize, "AREA")) && interactive && !gt$text.size.variable) {
		message("Text size will be constant in view mode. Set tm_view(text.size.variable = TRUE) to enable text size variables.")
		varysize <- FALSE
		nxtsize <- 1
		xtsize <- 1
	}
	
	varycol <- all(xtcol %in% shpcols) && !is.null(xtcol) && !(is.na(xtcol[1]))
	
	nx <- max(nxtcol, nxtsize, nxtext)
	if (nxtcol<nx) xtcol <- rep(xtcol, length.out=nx)
	if (nxtsize<nx) xtsize <- rep(xtsize, length.out=nx)
	if (nxtext<nx) xtext <- rep(xtext, length.out=nx)
	
	if (!varysize) {
		if (!all(is.numeric(xtsize) | xtsize=="AREA")) stop("Incorrect text sizes.", call. = FALSE)
		if (is.numeric(xtsize[1])) {
			g$size.lowerbound <- 0
		}
		if (any(xtsize=="AREA") && !("SHAPE_AREAS" %in% shpcols)) stop("size=\"AREA\" only valid for spatial polygons.", call.=FALSE)
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
		if (!is.colors) stop("Invalid text colors", call. = FALSE)
		if (is.na(xtcol)[1]) {
			if (is.matrix(fill)) {
				cols <- apply(fill, MARGIN=2, function(f) {
					light <- is_light(f)
					cl <- rep(ifelse(light, coldark, collight), length.out=npol)
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
			cols <- matrix(cols, nrow=npol,ncol=nx)
		} else {
			if (ncol(cols)!=nx) {
				cols <- cols[,rep(1:ncol(cols), length.out=nx)]
			}
		}

		for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- cols[, i]
		xtcol <- paste("COLOR", 1:nx, sep="_")
	}
	
	nx <- max(nx, nlevels(by))
	
	# update legend format from tm_layout
	g$legend.format <- process_legend_format(g$legend.format, gt$legend.format, nx)
	
	dtcol <- process_data(data[, xtcol, drop=FALSE], by=by, free.scales=gby$free.scales.text.col, is.colors=is.colors)	
	dtsize <- process_data(data[, xtsize, drop=FALSE], by=by, free.scales=gby$free.scales.text.size, is.colors=FALSE)
	
	if (nlevels(by)>1) if (is.na(g$showNA)) g$showNA <- attr(dtcol, "anyNA")
	
	##
	if (!all(xtext %in% shpcols)) stop("Incorrect data variable used for the text", call. = FALSE)

	text <- if (nx > 1) matrix(unlist(lapply(data[, xtext], as.character)), nrow=npol, ncol=nx) else as.character(data[[xtext]])
	if (!is.na(g$case)) text <- if(case=="upper") toupper(text) else tolower(text)
	
	
	if (is.list(dtsize)) {
		# multiple variables for size are defined
		gss <- split_g(g, n=nx)
		res <- mapply(process_text_size_vector, dtsize, as.list(as.data.frame(text)), gss, MoreArgs = list(rescale=varysize, gt), SIMPLIFY = FALSE)
		size <- sapply(res, function(r)r$size)
		text_sel <- sapply(res, function(r)r$text_sel)
		size.legend.labels <- lapply(res, function(r)r$size.legend.labels)
		size.legend.values <- lapply(res, function(r)r$size.legend.values)
		legend.sizes <- lapply(res, function(r)r$legend.sizes)
		max.size <- lapply(res, function(r)r$max.size)
	} else {
		res <- process_text_size_vector(dtsize, text, g, rescale=varysize, gt)
		size <- matrix(res$size, nrow=npol)
		text_sel <- matrix(res$text_sel, nrow=npol)
		
		if (varysize) {
			size.legend.labels <- res$size.legend.labels
			size.legend.values <- res$size.legend.values
			legend.sizes <- res$legend.sizes
			max.size <- res$max.size
		} else {
			size.legend.labels <- NA
			size.legend.values <- NA
			size.legend.text <- NA
			legend.sizes <- NA
			max.size <- res$max.size
			xtsize <- rep(NA, nx)
			size.legend.title <- rep(NA, nx)
		}
	}
	
	if (is.list(dtsize) || varysize) {
		# process legend text
		size.legend.text <- mapply(function(txt, v, l, ls, gssi) {
			if (is.na(gssi$sizes.legend.text[1])) {
				nl <- nlevels(v)
				lss <- ls[-1] - ls[-length(ls)]
				lss <- c(lss[1], (lss[-1] + lss[-length(lss)])/2, lss[length(lss)])
				ix <- mapply(function(i, j) {
					r <- which.min(abs(v-i))[1]
					if (which.min(abs(v[r]-ls))[1]==j) r else NA
				}, ls, 1:length(ls), SIMPLIFY=TRUE)
				sizetext <- txt[ix]
				sizetext[is.na(sizetext)] <- "NA"
			} else {
				sizetext <- rep(gssi$sizes.legend.text, length.out=length(l))
			}
			sizetext
		}, as.data.frame(text, stringsAsFactors = FALSE), as.data.frame(size), if (is.list(size.legend.labels)) size.legend.labels else list(size.legend.labels), if (is.list(legend.sizes)) legend.sizes else list(legend.sizes), if (is.list(dtsize)) gss else list(g), SIMPLIFY=FALSE)
	}
	
	sel <- if (is.list(dtcol)) as.list(as.data.frame(text_sel)) else as.vector(text_sel)
	
	# 
	# # selection: which line widths are NA?
	# sel <- if (is.list(dtsize)) {
	# 	lapply(dtsize, function(i)!is.na(i))
	# } else {
	# 	if (is.list(dtcol)) {
	# 		cnts <- vapply(dtcol, length, integer(1))
	# 		cnts2 <- 1:length(dtcol)
	# 		f <- factor(unlist(mapply(rep, cnts2, cnts, SIMPLIFY = FALSE)))
	# 		split(dtsize, f = f)
	# 	} else !is.na(dtsize)
	# } 
	
	dcr <- process_dtcol(dtcol, sel=sel, g, gt, nx, npol)
	if (dcr$is.constant) {
		xtcol <- rep(NA, nx)
		col.legend.text <- NA
		col.legend.title <- rep(NA, nx)
	}
	col <- dcr$col
	col.legend.labels <- dcr$legend.labels
	col.legend.values <- dcr$legend.values
	col.legend.palette <- dcr$legend.palette
	col.neutral <- gt$aes.colors[["text"]] # preferable over dcr$col.neutral to match examples
	breaks <- dcr$breaks
	values <- dcr$values
	
	if (is.list(dtcol)) {
		gsc <- split_g(g, n=nx)
	}
	
	if (is.list(values)) {
		# process legend text
		col.legend.text <- mapply(function(txt, v, b, s, l, gsci) {
			if (is.na(gsci$labels.text[1])) {
				
				if (is.na(b[1])) {
					# categorical
					nl <- nlevels(v)
					ids <- as.integer(v)
				} else {
					# numeric
					nl <- length(b) - 1
					ids <- as.integer(cut(v, breaks=b, include.lowest = TRUE, right = FALSE))
				}
				ix <- sapply(1:nl, function(i)which(ids==i & s)[1])
				if (length(l)==nl+1) {
					ix <- c(ix, which(is.na(v))[1])
				}
				coltext <- txt[ix]
				coltext[is.na(coltext)] <- "NA"
				
			} else {
				if (length(gsci$labels.text) == length(l)-1) {
					coltext <- c(gsci$labels.text, "NA")
				} else {
					coltext <- rep(gsci$labels.text, length.out=length(l))
				}
			}
			coltext
		}, as.data.frame(text, stringsAsFactors = FALSE), values, breaks, as.list(as.data.frame(text_sel)), if (is.list(col.legend.labels)) col.legend.labels else list(col.legend.labels), if (is.list(dtcol)) gsc else list(g), SIMPLIFY=FALSE)
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

 	text.just <- g$just
	xmod <- if (is.character(g$xmod)) data[[g$xmod]] else rep(g$xmod, length.out=npol)
	ymod <-  if (is.character(g$ymod)) data[[g$ymod]] else rep(g$ymod, length.out=npol)
	

	if (is.na(g$fontface)) g$fontface <- gt$fontface
	if (is.na(g$fontfamily)) g$fontfamily <- gt$fontfamily

	text.bg.color <- do.call("process_color", c(list(col=g$bg.color, alpha=g$bg.alpha), gt$pc))
	text.shadowcol <- do.call("process_color", c(list(col=g$shadowcol), gt$pc))

	text.size.legend.title <- if (is.ena(g$title.size)[1]) xtsize else g$title.size
	text.col.legend.title <- if (is.ena(g$title.col)[1]) xtcol else g$title.col
	text.size.legend.z <- if (is.na(g$legend.size.z)) z else g$legend.size.z
	text.col.legend.z <- if (is.na(g$legend.col.z)) z+.33 else g$legend.col.z
	text.legend.hist.z <- if (is.na(g$legend.hist.z)) z+.66 else g$legend.hist.z
	
	if (g$legend.hist && is.ena(g$legend.hist.title) && text.col.legend.z>text.legend.hist.z) {
		# histogram is drawn between title and legend enumeration
		text.col.legend.hist.title <- text.col.legend.title
		text.col.legend.title <- ""
	} else if (g$legend.hist && !is.na(g$legend.hist.title)) {
		text.col.legend.hist.title <- g$legend.hist.title
	} else text.col.legend.hist.title <- ""

	if (!g$legend.size.show) text.size.legend.title <- NA
	if (!g$legend.col.show) text.col.legend.title <- NA
	
	
	list(text=text,
		 text.size=size,
		 #root=g$root,
		 text.color=col,
		 text.fontface=g$fontface,
		 text.fontfamily=g$fontfamily,
		 text.shadow=g$shadow,
		 text.shadowcol=text.shadowcol,
		 text.bg.color=text.bg.color,
		 text.scale=g$scale,
		 text.auto.placement=g$auto.placement,
		 text.remove.overlap=g$remove.overlap,
		 text.along.lines=g$along.lines,
		 text.overwrite.lines=g$overwrite.lines,
		 text.col.legend.labels=col.legend.labels,
		 text.col.legend.values=col.legend.values,
		 text.col.legend.text=col.legend.text,
		 text.col.legend.palette=col.legend.palette,
		 text.col.legend.sizes = max.size,
		 text.col.legend.misc=list(),
		 text.size.legend.labels=size.legend.labels,
		 text.size.legend.values=size.legend.values,
		 text.size.legend.text=size.legend.text,
		 text.size.legend.palette= col.neutral,
		 text.size.legend.sizes = legend.sizes,
		 text.size.legend.misc=list(),
		 text.col.legend.hist.misc=list(values=values, breaks=breaks),
		 xtext=xtext,
		 xtsize=xtsize,
		 xtcol=xtcol,
 		 text.just=text.just,
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
		 text.col.legend.hist.z=text.legend.hist.z)
	
}
