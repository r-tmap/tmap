check_aes_args <- function(g) {
	nms <- names(g)
	if ("style" %in% nms) {
		if (length(g$style) != 1) stop("Only one value for style allowed per small multiple (unless free.scales=TRUE)", call.=FALSE)
		if (!is.character(g$style)) stop("Style is not a character", call.=FALSE)
		if (!g$style %in% c("cat", "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", "cont", "order", "log10", "log10_pretty")) stop("Invalid style value(s)", call.=FALSE)
	}
	
	if (!is.null(g$shapes.style)) {
		if (!g$shapes.style %in% c("cat", "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", "cont", "order", "log10", "log10_pretty")) stop("Invalid style value(s)", call.=FALSE)
	}
	
	if (!is.null(g$palette)) {
		gpal <- g$palette
		if (is.list(gpal)) stop("Only one palette can be defined per small multiple (unless free.scales=TRUE)", call.=FALSE)
		if (!is.character(gpal)) stop("Palette should be a character", call.=FALSE)
		if (length(gpal)==1) {
			if (substr(gpal, 1, 1)=="-") gpal <- substr(gpal, 2, nchar(gpal))
			if (!gpal %in% c(rownames(tmap.pal.info), "seq", "div", "cat") && !valid_colors(gpal)) stop("Invalid palette", call.=FALSE)
		} else {
			if (!all(valid_colors(gpal))) stop("Invalid palette", call.=FALSE)
		}
	}
	
	if (!is.null(g$labels)) {
		if (is.list(g$labels)) stop("Only one label vector can be defined per small multiple (unless free.scales=TRUE)", call.=FALSE)
		if (!is.character(g$labels)) stop("Labels should be a character vector", call.=FALSE)
	}
	
	NULL
}

process_col_vector <- function(x, sel, g, gt, reverse) {
	
	values <- x
	#textNA <- ifelse(any(is.na(values[sel])), g$textNA, NA)
	#showNA <- if (is.na(g$showNA)) any(is.na(values[sel])) else FALSE
	
	x[!sel] <- NA
	
	attr(x, "sel") <- as.vector(attr(x, "sel")) & as.vector(sel)
	
	check_aes_args(g)
	
	allNA <- all(is.na(x)) 
	isNUM <- is.numeric(x)

	if (isNUM) {
		if (allNA) {
			g$style <- "cat"
		} else {
			rng <- range(x, na.rm = TRUE)
			if (abs(rng[2] - rng[1]) < 1e-9 && rng[1] != rng[2]) {
				warning("The value range of the variable \"", g$col, "\" is less than 1e-9", call. = FALSE)
				x[!is.na(x)] <- round(rng[1], 9)
			}
		}
	} else if (allNA) {
		g$style <- "cat"
	}
	
	if (length(na.omit(unique(x)))==1 && g$style!="fixed") g$style <- "cat"
	
	if (is.factor(x) || g$style=="cat") {
		
		if (is.null(g$palette)) {
			clrs <- attr(x, "clrs") # stars color table
			if (!is.null(clrs)) {
				palette <- clrs
				palette.type <- "cat"
			} else {
				palette.type <- ifelse(is.ordered(x) || (isNUM), "seq", "cat")
				palette <- gt$aes.palette[[palette.type]] 
			}
		} else if (g$palette[1] %in% c("seq", "div", "cat")) {
			palette.type <- g$palette[1]
			palette <- gt$aes.palette[[palette.type]]
		} else {
			palette <- g$palette
			palette.type <- palette_type(palette)
		}
		colsLeg <- cat2pal(x,
						   var = g$col,
						   palette = palette,
						   drop.levels = g$drop.levels,
						   stretch.palette = g$stretch.palette,
						   contrast = g$contrast,
						   colorNA = g$colorNA,
						   colorNULL=g$colorNULL,
						   legend.labels=g$labels,
						   max_levels=gt$max.categories,
						   legend.NA.text = g$textNA,
						   showNA = g$showNA,
						   process.colors=c(list(alpha=g$alpha), gt$pc),
						   legend.format=g$legend.format,
						   reverse=reverse)
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
		colsLeg <- num2pal(x, 
						   var = g$col,
						   call = g$call,
						   g$n, style=g$style, 
						   style.args=g$style.args,
						   as.count = g$as.count,
						   breaks=g$breaks, 
						   interval.closure=g$interval.closure,
						   palette = palette,
						   midpoint = g$midpoint, #auto.palette.mapping = g$auto.palette.mapping,
						   contrast = g$contrast, legend.labels=g$labels,
						   colorNA=g$colorNA, 
						   colorNULL=g$colorNULL,
						   legend.NA.text = g$textNA,
						   showNA = g$showNA,
						   process.colors=c(list(alpha=g$alpha), gt$pc),
						   legend.format=g$legend.format,
						   reverse=reverse)
		breaks <- colsLeg$breaks
		breakspal <- colsLeg$breaks.palette
		col.neutral <- colsLeg$legend.neutral.col
		
	}
	cols <- colsLeg$cols
	legend.labels <- colsLeg$legend.labels
	legend.values <- colsLeg$legend.values
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
				legend.values=legend.values,
				legend.palette=legend.palette,
				col.neutral=col.neutral,
				breaks=breaks))
}


process_dtcol <- function(xname, dtcol, sel=NA, g, gt, nx, npol, areas=NULL, areas_unit=NULL, text = NULL, text_sel = NULL) {
	## dtcol = matrix if direct colors are given
	## dtcol = list in case of disjoint small multiples
	## dtcol = vector in case of small multiples processed once (i.e. they share the legend)
	
	reverse <- g[[aname("legend.reverse", xname)]]
	
	## return as color matrix (object col)
	if (is.na(sel[1])) sel <- rep(TRUE, npol * nx)
	sel[is.na(sel)] <- TRUE
	
	is.constant <- is.matrix(dtcol)
	if (is.constant) {
		col <- dtcol
		col.neutral <- apply(col, 2, function(bc) na.omit(bc)[1])
		
		if (all(is.na(col.neutral))) {
			col.neutral[is.na(col.neutral)] <- "#000000" #dummy for empty facets
		} else {
			col.neutral[is.na(col.neutral)] <- col.neutral[which(!is.na(col.neutral))[1]]	## impute first non-NA value (in case of !free.scales, the legend of the first facet is taken)
		}

		col[is.na(col)] <- g$colorNULL
		
		# apply color processing if needed
		if ((!is.na(g$alpha) && (g$alpha < 1)) || (gt$pc$sepia.intensity != 0) || (gt$pc$saturation != 1)) {
			ucol <- unique(as.vector(col))
			ucol2 <- do.call("process_color", c(list(col=ucol, alpha=g$alpha), gt$pc))
			col[] <- ucol2[match(col, ucol)]
		}

		legend.labels <- NA
		legend.values <- NA
		legend.palette <- NA
		breaks <- NA
		values <- NA
		title_append <- ""
	} else if (is.list(dtcol)) {
		# multiple variables for col are defined
		
		gsc <- split_g(g, n=nx)
		
		dtcol_title_append <- mapply(check_num_col, dtcol, gsc, SIMPLIFY = FALSE, MoreArgs = list(areas, areas_unit))
		sel_attr <- as.list(as.data.frame(attr(dtcol, "sel")))
		dtcol <- lapply(dtcol_title_append, "[[", "col")
		
		# add sel attribute to dtcol elements
		dtcol <- mapply(function(d, s) {
			attr(d, "sel") <- s
			d
		}, dtcol, sel_attr, SIMPLIFY = FALSE)
		title_append <- vapply(dtcol_title_append, "[[", character(1), "title_append")

		sel <- split(sel, f = rep(1L:nx, each = npol))
		res <- mapply(process_col_vector, dtcol, sel, gsc, MoreArgs=list(gt=gt, reverse=reverse), SIMPLIFY=FALSE)
		col <- sapply(res, function(r)r$cols)
		legend.labels <- lapply(res, function(r)r$legend.labels)
		legend.values <- lapply(res, function(r)r$legend.values)
		legend.palette <- lapply(res, function(r)r$legend.palette)
		col.neutral <- lapply(res, function(r)r$col.neutral)
		breaks <- lapply(res, function(r)r$breaks)
		values <- dtcol
		
		## remove legend from facets with empty selection (due to zero size or lwd)
		noData <- !sapply(sel, function(s) any(s))
		legend.labels[noData] <- NA
	} else {

		dtcol_title_append <- check_num_col(dtcol, g, areas, areas_unit)
		dtcol <- dtcol_title_append$col
		title_append <- dtcol_title_append$title_append

		
		#if (is.na(sel[1])) sel <- TRUE
		
		
		res <- process_col_vector(dtcol, sel, g, gt, reverse)
		col <- matrix(res$cols, nrow=npol)
		legend.labels <- res$legend.labels
		legend.values <- res$legend.values
		legend.palette <- res$legend.palette
		col.neutral <- res$col.neutral
		breaks <- res$breaks
		values <- split(dtcol, rep(1:nx, each=npol))
	}
	
	if (xname == "fill") {
		legend.misc <- list(lwd=g$gborders$lwd, border.col=g$gborders$col)	
	} else if (xname == "line.col") {
		legend.misc <- list(line.legend.lty = g$lty, line.legend.alpha = g$alpha) # legend.lwd added later
	} else if (xname == "symbol.col") {
		legend.misc <- list(symbol.border.lwd=g$border.lwd, symbol.normal.size=g$legend.max.symbol.size) # symbol.border.col added later
	} else if (xname == "raster") {
		legend.misc <- list()
		if (is.constant) {
			#legend.palette <- as.list(col.neutral)
			legend.palette <- lapply(as.data.frame(dtcol, stringsAsFactors = FALSE), unique)
		}
	} else if (xname == "text.col") {
		if (is.list(dtcol)) {
			gsc <- split_g(g, n=nx)
		}
		if (is.list(values)) {
			# process legend text
			legend.text <- mapply(function(txt, v, b, s, l, gsci) {
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
			}, as.data.frame(text, stringsAsFactors = FALSE), values, breaks, as.list(as.data.frame(text_sel)), if (is.list(legend.labels)) legend.labels else list(legend.labels), if (is.list(dtcol)) gsc else list(g), SIMPLIFY=FALSE)
		} else {
			legend.text <- NA
		}
		legend.misc <- list(legend.text = legend.text)
	}
	

	nonemptyFacets <- if (is.constant) NULL else if(is.list(values)) sapply(values, function(v) !all(is.na(v))) else rep(TRUE, nx)
	list(is.constant=is.constant,
		 col=col,
		 legend.labels=legend.labels,
		 legend.values=legend.values,
		 legend.palette=legend.palette,
		 col.neutral=col.neutral,
		 legend.misc = legend.misc,
		 legend.hist.misc=list(values=values, breaks=breaks, densities=g$convert2density),
		 nonemptyFacets=nonemptyFacets,
		 title_append=title_append)
}




