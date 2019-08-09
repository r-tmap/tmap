process_dttsize <- function(dtsize, text, g, gt, nx, npol, varysize, varycol) {
	is.constant <- FALSE
	if (is.list(dtsize)) {
		# multiple variables for size are defined
		gss <- split_g(g, n=nx)
		
		# only get title_append from columns
		title_append <- vapply(mapply(check_num_col, dtsize, gss, SIMPLIFY = FALSE), "[[", character(1), "title_append")
		
		res <- mapply(process_text_size_vector, dtsize, as.list(as.data.frame(text)), gss, MoreArgs = list(rescale=varysize, gt=gt, reverse=g$legend.size.reverse), SIMPLIFY = FALSE)
		size <- sapply(res, function(r)r$size)
		text_sel <- sapply(res, function(r)r$text_sel)
		size.legend.labels <- lapply(res, function(r)r$size.legend.labels)
		size.legend.values <- lapply(res, function(r)r$size.legend.values)
		legend.sizes <- lapply(res, function(r)r$legend.sizes)
		max.size <- lapply(res, function(r)r$max.size)
	} else {
		title_append <- check_num_col(dtsize, g)$title_append
		
		
		res <- process_text_size_vector(dtsize, text, g, rescale=varysize, gt=gt, reverse=g$legend.size.reverse)
		size <- matrix(res$size, nrow=npol)
		text_sel <- matrix(res$text_sel, nrow=npol)
		
		if (varysize) {
			size.legend.labels <- res$size.legend.labels
			size.legend.values <- res$size.legend.values
			legend.sizes <- res$legend.sizes
			max.size <- res$max.size
		} else {
			is.constant <- TRUE
			size.legend.labels <- NA
			size.legend.values <- NA
			size.legend.text <- NA
			legend.sizes <- NA
			max.size <- res$max.size
			#xtsize <- rep(NA, nx)
			#size.legend.title <- rep(NA, nx)
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
	
	list(is.constant = is.constant,
		 size = size,
		 legend.labels = size.legend.labels,
		 legend.values = size.legend.values,
		 legend.palette = gt$aes.colors[["text"]],
		 legend.text = size.legend.text,
		 legend.sizes = legend.sizes,
		 max.size = max.size,
		 legend.misc = list(),
		 #xtsize = xtsize,
		 #legend.title = size.legend.title,
		 text_sel = text_sel,
		 title_append = title_append)
}

process_text_size_vector <- function(x, text, g, rescale, gt, reverse) {
	check_aes_args(g)
	
	if (all(is.na(x))) {
		return(list(size=x,
			 text_sel=rep(FALSE, length(x)),
			 size.legend.labels=NA,
			 size.legend.values=NA,
			 legend.sizes=NA,
			 max.size=NA))
	}
	
	
	if (!is.na(g$size.lim[1])) {
		x[x<g$size.lim[1]] <- NA
		x[x>g$size.lim[2]] <- g$size.lim[2]
	}
	
	if (is.null(g$sizes.legend)) {
		x_legend <- pretty(x, 5)
		x_legend <- x_legend[x_legend!=0]
		nxl <- length(x_legend)
		if (nxl>5) x_legend <- x_legend[-c(nxl-3, nxl-1)]
	} else {
		x_legend <- g$sizes.legend
	}
	
	size.legend.values <- x_legend
	
	if (is.null(g$sizes.legend.labels)) {
		size.legend.labels <- do.call("fancy_breaks", c(list(vec=x_legend, intervals=FALSE), g$legend.format))
	} else {
		if (length(g$sizes.legend.labels) != length(x_legend)) stop("length of sizes.legend.labels is not equal to the number of texts in the legend", call. = FALSE)
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
		size[!text_sel & !text_empty] <- g$size.lowerbound
		text_sel <- !text_empty
	} else {
		text_sel <- text_sel & !text_empty
	}
	
	size <- size * g$scale
	max.size <- max.size * g$scale
	legend.sizes <- legend.sizes * g$scale
	
	if (reverse) {
		size.legend.labels <- rev(size.legend.labels)
		size.legend.values <- rev(size.legend.values)
		legend.sizes <- legend.sizes
	}
	attr(size.legend.labels, "align") <- g$legend.format$text.align
	
	list(size=size,
		 text_sel=text_sel,
		 size.legend.labels=size.legend.labels,
		 size.legend.values=size.legend.values,
		 legend.sizes=legend.sizes,
		 max.size=max.size)
}
