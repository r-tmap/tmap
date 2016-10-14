check_aes_args <- function(g) {
	nms <- names(g)
	if ("style" %in% nms) {
		if (length(g$style) != 1) stop("Only one value for style allowed per small multiple (unless free.scales=TRUE)", call.=FALSE)
		if (!is.character(g$style)) stop("Style is not a character", call.=FALSE)
		if (!g$style %in% c("cat", "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "cont", "order")) stop("Invalid style value(s)", call.=FALSE)
	}
	
	if (!is.null(g$palette)) {
		if (is.list(g$palette)) stop("Only one palette can be defined per small multiple (unless free.scales=TRUE)", call.=FALSE)
		if (!is.character(g$palette)) stop("Palette should be a character", call.=FALSE)
		if (length(g$palette)==1) {
			if (!g$palette %in% rownames(brewer.pal.info) && !valid_colors(g$palette)) stop("Invalid palette", call.=FALSE)
		} else {
			if (!all(valid_colors(g$palette))) stop("Invalid palette", call.=FALSE)
		}
	}
	
	if (!is.null(g$labels)) {
		if (is.list(g$labels)) stop("Only one label vector can be defined per small multiple (unless free.scales=TRUE)", call.=FALSE)
		if (!is.character(g$labels)) stop("Labels should be a character vector", call.=FALSE)
	}
	
	NULL
}

process_col_vector <- function(x, sel, g, gt) {
	values <- x
	#textNA <- ifelse(any(is.na(values[sel])), g$textNA, NA)
	#showNA <- if (is.na(g$showNA)) any(is.na(values[sel])) else FALSE
	
	x[!sel] <- NA
	
	check_aes_args(g)
	
	if (length(na.omit(unique(x)))==1 && g$style!="fixed") g$style <- "cat"
	
	if (is.factor(x) || g$style=="cat") {
		
		if (is.null(g$palette)) {
			palette.type <- ifelse(is.ordered(x), "seq", "cat")
			palette <- gt$aes.palette[[palette.type]] 
		} else if (g$palette[1] %in% c("seq", "div", "cat")) {
			palette.type <- g$palette[1]
			palette <- gt$aes.palette[[palette.type]]
		} else {
			palette <- g$palette
			palette.type <- palette_type(palette)
		}
		colsLeg <- cat2pal(x,
						   palette = palette,
						   auto.palette.mapping = g$auto.palette.mapping,
						   contrast = g$contrast,
						   colorNA = g$colorNA,
						   legend.labels=g$labels,
						   max_levels=g$max.categories,
						   legend.NA.text = g$textNA,
						   showNA = g$showNA,
						   process.colors=c(list(alpha=g$alpha), gt$pc))
		breaks <- NA
		
			
			
		neutralID <- if (palette.type=="div") round(((length(colsLeg$legend.palette)-1)/2)+1) else 1
		col.neutral <- colsLeg$legend.palette[1]
		
	} else {
		is.diverging <- use_diverging_palette(x, g$breaks)
		palette <- if (is.null(g$palette)) {
			gt$aes.palette[[ifelse(is.diverging, "div", "seq")]] 
		} else if (g$palette[1] %in% c("seq", "div", "cat")) {
			gt$aes.palette[[g$palette[1]]]
		} else g$palette
		colsLeg <- num2pal(x, g$n, style=g$style, breaks=g$breaks, 
						   interval.closure=g$interval.closure,
						   palette = palette,
						   auto.palette.mapping = g$auto.palette.mapping,
						   contrast = g$contrast, legend.labels=g$labels,
						   colorNA=g$colorNA, 
						   legend.NA.text = g$textNA,
						   showNA = g$showNA,
						   process.colors=c(list(alpha=g$alpha), gt$pc),
						   legend.format=g$legend.format)
		breaks <- colsLeg$breaks
		breakspal <- colsLeg$breaks.palette
		col.neutral <- colsLeg$legend.neutral.col
		
	}
	cols <- colsLeg$cols
	legend.labels <- colsLeg$legend.labels
	legend.palette <- colsLeg$legend.palette

	## color tiny
	if (!is.na(breaks[1]) && any(!sel)) {
		tmp_breaks <- breaks
		tmp_breaks[1] <- -Inf
		tmp_breaks[length(tmp_breaks)] <- Inf
		tmp_int <- findInterval(values[!sel], tmp_breaks)
		tmp_int[is.na(tmp_int)] <- g$colorNA
		cols[!sel] <- breakspal[tmp_int]
	}
	return(list(cols=cols, 
				legend.labels=legend.labels,
				legend.palette=legend.palette,
				col.neutral=col.neutral,
				breaks=breaks))
}


process_dtcol <- function(dtcol, sel=NA, g, gt, nx, npol, check_dens=FALSE, show_warning=FALSE, areas=NULL) {
	is.constant <- is.matrix(dtcol)
	if (is.constant) {
		col <- dtcol
		legend.labels <- NA
		legend.palette <- NA
		col.neutral <- apply(col, 2, function(bc) na.omit(bc)[1])
		breaks <- NA
		values <- NA
	} else if (is.list(dtcol)) {
		# multiple variables for col are defined
		if (is.na(sel[1])) sel <- rep(TRUE, nx)
		gsc <- split_g(g, n=nx)
		
		if (check_dens) {
			isNum <- sapply(dtcol, is.numeric)
			isDens <- sapply(gsc, "[[", "convert2density")
			
			if (any(isNum & isDens) && show_warning) warning("Density values are not correct, because the shape coordinates are not projected.", call. = FALSE)
			dtcol[isNum & isDens] <- lapply(dtcol[isNum & isDens], function(d) {
				d / areas
			})
		}
		
		res <- mapply(process_col_vector, dtcol, sel, gsc, MoreArgs=list(gt), SIMPLIFY=FALSE)
		col <- sapply(res, function(r)r$cols)
		legend.labels <- lapply(res, function(r)r$legend.labels)
		legend.palette <- lapply(res, function(r)r$legend.palette)
		col.neutral <- lapply(res, function(r)r$col.neutral)
		breaks <- lapply(res, function(r)r$breaks)
		values <- dtcol
	} else {
		if (check_dens) {
			if (is.numeric(dtcol) && g$convert2density) {
				if (show_warning) warning("Density values are not correct, because the shape coordinates are not projected.", call. = FALSE)
				dtcol <- dtcol / areas
			}
		}
		
		if (is.na(sel[1])) sel <- TRUE
		res <- process_col_vector(dtcol, sel, g, gt)
		col <- matrix(res$cols, nrow=npol)
		legend.labels <- res$legend.labels
		legend.palette <- res$legend.palette
		col.neutral <- res$col.neutral
		breaks <- res$breaks
		values <- split(dtcol, rep(1:nx, each=npol))
	}
	list(is.constant=is.constant,
		 col=col,
		 legend.labels=legend.labels,
		 legend.palette=legend.palette,
		 col.neutral=col.neutral,
		 breaks=breaks,
		 values=values)
}




