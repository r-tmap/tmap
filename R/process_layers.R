process_layers <- function(g, z, gt, gf, interactive) {
	if (dupl <- anyDuplicated(names(g))) {
		warning("One tm layer group has duplicated layer types, which are omitted. To draw multiple layers of the same type, use multiple layer groups (i.e. specify tm_shape prior to each of them).", call. = FALSE)
		g <- g[-dupl]	
	} 
	
	type <- g$tm_shape$type
	
	if ((type=="tiles") && any(c("tm_fill", "tm_borders", "tm_lines", "tm_symbols", "tm_raster") %in% names(g))) {
		stop("tm layer called without tm_shape", call. = FALSE)
	} else if (type=="polygons" && "tm_lines" %in% names(g)) {
		stop(g$tm_shape$shp_name, " consists of polygons, so it cannot accept tm_lines.", call. = FALSE)
	} else if (type=="polygons" && "tm_raster" %in% names(g)) {
		stop(g$tm_shape$shp_name, " consists of polygons, so it cannot accept tm_raster.", call. = FALSE)
	} else if (type=="raster" && any(c("tm_fill", "tm_borders") %in% names(g))) {
		stop(g$tm_shape$shp_name, " is a raster, so it cannot accept tm_fill/tm_borders/tm_polygons.", call. = FALSE)
	} else if (type=="raster" && "tm_lines" %in% names(g)) {
		stop(g$tm_shape$shp_name, " is a raster, so it cannot accept tm_lines.", call. = FALSE)
	} else if (type=="raster" && "tm_symbols" %in% names(g)) {
		stop(g$tm_shape$shp_name, " is a raster, so it cannot accept tm_symbols/tm_dots.", call. = FALSE)
	} else if (type=="points" && "tm_lines" %in% names(g)) {
		stop(g$tm_shape$shp_name, " consists of spatial points, so it cannot accept tm_lines.", call. = FALSE)
	} else if (type=="points" && "tm_raster" %in% names(g)) {
		stop(g$tm_shape$shp_name, " consists of spatial points, so it cannot accept tm_raster.", call. = FALSE)
	} else if (type=="points" && any(c("tm_fill", "tm_borders") %in% names(g))) {
		stop(g$tm_shape$shp_name, " consists of spatial points, so it cannot accept tm_fill/tm_borders/tm_polygons.", call. = FALSE)
	} else if (type=="lines" && any(c("tm_fill", "tm_borders") %in% names(g))) {
		stop(g$tm_shape$shp_name, " consists of spatial lines, so it cannot accept tm_fill/tm_borders/tm_polygons.", call. = FALSE)
	} else if (type=="lines" && "tm_raster" %in% names(g)) {
		stop(g$tm_shape$shp_name, " consists of spatial lines, so it cannot accept tm_raster.", call. = FALSE)
	}
	
	data <- g$tm_shape$data
	
	scale <- gt$scale
	
	if (!is.null(data) && attr(data, "treat_as_by")) {
		data$GROUP_BY <- factor("_NA_")
		by <- NA
		ncol <- NA
		nrow <- NA
		panel.names <- setdiff(names(data), c("tmapfilter", "GROUP_BY"))
		treat_by_count <- length(panel.names)
	} else if (g$tm_shape$by[1]=="") {
		data$GROUP_BY <- factor("_NA_")
		by <- NA
		ncol <- NA
		nrow <- NA
		panel.names <- NA
		treat_by_count <- 1
	} else {
		if (!all(g$tm_shape$by %in% names(data))) stop("Variable(s) \"", paste(setdiff(g$tm_shape$by, names(data)), collapse=", "), "\" not found in ", g$tm_shape$shp_name, call.=FALSE)
		
		d <- data[, g$tm_shape$by, drop=FALSE]
		d2 <- lapply(d, function(dcol) {
			showNA <- ifelse(is.na(gf$showNA), any(is.na(dcol)), gf$showNA)
			if (is.factor(dcol)) {
				lev <- if (gf$drop.empty.facets) levels(dcol)[table(dcol)>0] else levels(dcol)
				dcol <- as.character(dcol)
			} else {
				lev <- as.character(sort(unique(dcol)))
			}
			if (showNA) {
				lev <- c(lev, gf$textNA)
				dcol[is.na(dcol)] <- gf$textNA
			}
			factor(dcol, levels=lev)
		})
		if (length(g$tm_shape$by)==1) {
			data$GROUP_BY <- d2[[1]]
			by <- levels(data$GROUP_BY)
			ncol <- NA
			nrow <- NA
			panel.names <- by
		} else {
			by <- paste(rep(levels(d2[[1]]), each=nlevels(d2[[2]])),
						rep(levels(d2[[2]]), times=nlevels(d2[[1]])), sep="__")
			data$GROUP_BY <- factor(paste(d2[[1]], d2[[2]], sep="__"), levels = by)
			ncol <- nlevels(d2[[2]])
			nrow <- nlevels(d2[[1]])
			panel.names <- list(levels(d2[[1]]), levels(d2[[2]]))
		}
		treat_by_count <- 1
	}
	
	a <- g$tm_shape$along
	if (a=="" || interactive) {
		if (a!="") warning("along not supported in view mode", call. = FALSE)
		alev <- NA
		data$ALONG <- factor("_NA_")
		along.names <- NA
	} else {
		if (!(a %in% names(data))) stop("Variable \"", a, "\" not found in ", g$tm_shape$shp_name, call.=FALSE)

		acol <- data[[a]]
		showNA <- ifelse(is.na(gf$showNA), any(is.na(acol)), gf$showNA)
		if (is.factor(acol)) {
			alev <- levels(acol)
			acol <- as.character(acol)
		} else {
			alev <- as.character(sort(unique(acol)))
		}
		if (showNA) {
			alev <- c(alev, gf$textNA)
			acol[is.na(acol)] <- gf$textNA
		}
		
		data$ALONG <- factor(acol, levels=alev)
		along.names <- alev
	}
	

	
	if (is.na(by[1])) {
		data$GROUP_BY <- data$ALONG
		by <- alev
	} else if (!is.na(alev[1])) {
		ablev <- apply(expand.grid(by, alev, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE), MARGIN=1, paste, collapse="__")
		data$GROUP_BY <- factor(paste(as.character(data$GROUP_BY),
							   as.character(data$ALONG),
							   sep = "__"), levels=ablev)
				
		by <- ablev		   
	}
	

	# determine plotting order 
	plot.order <- names(g)[names(g) %in% c("tm_fill", "tm_borders", "tm_text", "tm_symbols", "tm_lines", "tm_raster", "tm_tiles") | substr(names(g), 1, 13)=="tm_add_legend"]
	plot.order[plot.order=="tm_borders"] <- "tm_fill"
	plot.order <- unique(plot.order)
	
	# border info
	gborders <- if (is.null(g$tm_borders)) {
		list(col=NULL, lwd=1, lty="blank", alpha=NA)
	} else g$tm_borders
	if (!is.null(gborders$col)) {
		if (is.na(gborders$col)) {
			gborders$col <- gt$aes.colors["borders"]
		}
	} else {
		gborders$col <- NA
	}
	gborders$col <- do.call("process_color", c(list(col=gborders$col, alpha=gborders$alpha), gt$pc))
	
# 	gborders$lwd <- gborders$lwd * scale
	
	
	# fill info
	if (is.null(g$tm_fill)) {
		fill.group <- if (is.null(g$tm_borders)) NA else g$tm_borders$group
		gfill <- list(fill=NULL, xfill=NA, fill.legend.title=NA, fill.id=NA, fill.group = fill.group, fill.zindex = NA) 
	} else {
		g$tm_fill$gborders <- gborders
		gfill <- process_fill(data, g$tm_fill, gt, gf, z=z+which(plot.order=="tm_fill"), interactive=interactive)
	}
	# symbol info
	if (is.null(g$tm_symbols)) {
		gsymbol <- list(symbol.size=NULL, xsize=NA, xcol=NA, xshape=NA, symbol.size.legend.title=NA, symbol.col.legend.title=NA, symbol.shape.legend.title=NA, symbol.id=NA, symbol.group = NA)
	} else {
		gsymbol <- process_symbols(data, g$tm_symbols, gt, gf, z=z+which(plot.order=="tm_symbols"), interactive=interactive)
	}

	# lines info
	if (is.null(g$tm_lines)) {
		glines <- list(line.lwd=NULL, xline=NA, xlinelwd=NA, line.col.legend.title=NA, line.lwd.legend.title=NA, line.id=NA, line.group = NA) 
	} else {
		glines <- process_lines(data, g$tm_lines, gt, gf, z=z+which(plot.order=="tm_lines"), interactive=interactive)	
	} 

	# raster info
	if (is.null(g$tm_raster)) {
		graster <- list(raster=NULL, xraster=NA, raster.legend.title=NA, raster.group = NA) 
	} else {
		graster <- process_raster(data, g$tm_raster, gt, gf, z=z+which(plot.order=="tm_raster"), interactive=interactive)
	}	
	
	
	# text info
	if (is.null(g$tm_text)) {
		gtext <- list(text=NULL, xtext=NA, xtsize=NA, xtcol=NA, text.size.legend.title=NA, text.col.legend.title=NA, text.group = NA)
	}  else {
		gtext <- process_text(data, g$tm_text, if (is.null(gfill$fill)) NA else gfill$fill, gt, gf, z=z+which(plot.order=="tm_text"), interactive=interactive)
	}
	
	# tiles info
	if (is.null(g$tm_tiles)) {
		gtiles <- list(tile.server = NULL, tile.alpha = NA, tile.group = NA, tile.tms = FALSE)	
	} else {
		gtiles <- process_tiles(g$tm_tiles, gt)
	}
	

	legend.titles <- c(gfill$fill.legend.title, gsymbol$symbol.size.legend.title, gsymbol$symbol.col.legend.title, gsymbol$symbol.shape.legend.title, glines$line.col.legend.title, glines$line.lwd.legend.title, graster$raster.legend.title, gtext$text.size.legend.title, gtext$text.col.legend.title)
	
	legend.shows <- c(gfill$fill.legend.show, gsymbol$symbol.size.legend.show, gsymbol$symbol.col.legend.show, gsymbol$symbol.shape.legend.show, glines$line.col.legend.show, glines$line.lwd.legend.show, graster$raster.legend.show, gtext$text.size.legend.show, gtext$text.col.legend.show)
	
	# see also process_aes 278
	any.legend <- any(legend.shows) # any(!is.ena(legend.titles))
	# 	glines$line.lwd.legend.title
	
	
	
	legids <- which(substr(names(g), 1, 13)=="tm_add_legend")
	if (length(legids)) {
		add_legends <- mapply(function(gal, name) {
			if (is.na(gal$z)) gal$z <- z + which(plot.order==name)
			gal
		}, g[legids], names(g[legids]), SIMPLIFY=FALSE)
		
	} else {
		add_legends <- list()
	}
	
	plot.order <- plot.order[substr(plot.order, 1, 13)!="tm_add_legend"]
	

	## drop NA facets (when drop.NA.facets==TRUE)
	if (!is.na(by[1]) && gf$drop.NA.facets) { #identical(gf$showNA, FALSE)
		neFL <- list(gfill$fill.nonemptyFacets,
			 glines$line.nonemptyFacets,
			 gsymbol$symbol.nonemptyFacets,
			 gtext$text.nonemptyFacets,
			 graster$raster.nonemptyFacets)
		neFLS <- vapply(neFL, function(nef) length(nef)==length(by), logical(1))

		if (any(neFLS)) {
			neFL <- neFL[neFLS]
			neFM <- do.call(cbind, neFL)
			eF <- rowSums(neFM) == 0 & (as.integer(table(data$GROUP_BY)) != 0) # the latter is controlled by drop.empty.facets
			#data$GROUP_BY[eF] <- NA

			lev <- levels(data$GROUP_BY)[!eF]


			
			data$GROUP_BY <- factor(as.character(data$GROUP_BY), levels = lev)
			by <- by[!eF]
			if (neFLS[1]) gfill <- within(gfill, {
				fill <- fill[, !eF, drop=FALSE]
				xfill <- truncate_vec(xfill, !eF)
				fill.nonemptyFacets <- fill.nonemptyFacets[!eF]
				if (!is.na(xfill[1])) {
					fill.legend.labels <- truncate_labels(fill.legend.labels, !eF)
					fill.legend.values <- truncate_other(fill.legend.values, !eF)
					fill.legend.palette <- truncate_other(fill.legend.palette, !eF)
					fill.legend.hist.misc$values <- truncate_other(fill.legend.hist.misc$values, !eF)
					fill.legend.hist.misc$breaks <- truncate_other(fill.legend.hist.misc$breaks, !eF)
					fill.legend.show <- fill.legend.show[!eF]
					fill.legend.title <- fill.legend.title[!eF]
				}
			})
			if (neFLS[2]) glines <- within(glines, {
				line.col <- line.col[, !eF, drop=FALSE]
				line.lwd <- line.lwd[, !eF]
				line.nonemptyFacets <- line.nonemptyFacets[!eF]
				line.col.legend.labels <- truncate_labels(line.col.legend.labels, !eF)
				line.col.legend.values <- truncate_other(line.col.legend.values, !eF)
				line.col.legend.palette <- truncate_other(line.col.legend.palette, !eF)
				line.col.legend.misc$line.legend.lwd <- truncate_vec(line.col.legend.misc$line.legend.lwd, !eF)
				line.col.legend.hist.misc$values <- truncate_other(line.col.legend.hist.misc$values, !eF)
				line.col.legend.hist.misc$breaks <- truncate_other(line.col.legend.hist.misc$breaks, !eF)
				line.col.legend.show <- line.col.legend.show[!eF]
				line.col.legend.title <- line.col.legend.title[!eF]
				
				line.lwd.legend.labels <- truncate_labels(line.lwd.legend.labels, !eF)
				line.lwd.legend.values <- truncate_other(line.lwd.legend.values, !eF)
				line.lwd.legend.palette <- truncate_vec(line.lwd.legend.palette, !eF)
				line.lwd.legend.misc$legend.lwds <- truncate_other(line.lwd.legend.misc$legend.lwds, !eF)
				xline <- truncate_vec(xline, !eF)
				xlinelwd <- truncate_vec(xlinelwd, !eF)
				line.lwd.legend.show <- line.lwd.legend.show[!eF]
				line.lwd.legend.title <- line.lwd.legend.title[!eF]
			})
			if (neFLS[3]) gsymbol <- within(gsymbol, {
				symbol.size <- symbol.size[, !eF, drop=FALSE]
				symbol.col <- symbol.col[, !eF, drop=FALSE]
				symbol.shape <- symbol.shape[, !eF, drop=FALSE]
				
				symbol.nonemptyFacets <- symbol.nonemptyFacets[!eF]
				
				symbol.col.legend.labels <- truncate_labels(symbol.col.legend.labels, !eF)
				symbol.col.legend.values <- truncate_other(symbol.col.legend.values, !eF)
				symbol.col.legend.palette <- truncate_other(symbol.col.legend.palette, !eF)
				symbol.col.legend.hist.misc$values <- truncate_other(symbol.col.legend.hist.misc$values, !eF)
				symbol.col.legend.hist.misc$breaks <- truncate_other(symbol.col.legend.hist.misc$breaks, !eF)
				symbol.col.legend.show <- symbol.col.legend.show[!eF]
				symbol.col.legend.title <- symbol.col.legend.title[!eF]
				
				symbol.size.legend.labels <- truncate_labels(symbol.size.legend.labels, !eF)
				symbol.size.legend.values <- truncate_other(symbol.size.legend.values, !eF)
				symbol.size.legend.palette <- truncate_vec(symbol.size.legend.palette, !eF)
				symbol.size.legend.sizes <- truncate_other(symbol.size.legend.sizes, !eF)
				symbol.size.legend.shapes <- truncate_other(symbol.size.legend.shapes, !eF)
				symbol.size.legend.show <- symbol.size.legend.show[!eF]
				symbol.size.legend.title <- symbol.size.legend.title[!eF]

				symbol.shape.legend.labels <- truncate_labels(symbol.shape.legend.labels, !eF)
				symbol.shape.legend.values <- truncate_other(symbol.shape.legend.values, !eF)
				symbol.shape.legend.palette <- truncate_other(symbol.shape.legend.palette, !eF)
				symbol.shape.legend.sizes <- truncate_other(symbol.shape.legend.sizes, !eF)
				symbol.shape.legend.shapes <- truncate_other(symbol.shape.legend.shapes, !eF)
				symbol.shape.legend.show <- symbol.shape.legend.show[!eF]
				symbol.shape.legend.title <- symbol.shape.legend.title[!eF]
				
				symbol.xmod <- symbol.xmod[, !eF, drop = FALSE]
				symbol.ymod <- symbol.ymod[, !eF, drop = FALSE]
				
				xsize <- truncate_vec(xsize, !eF)
				xshape <- truncate_vec(xshape, !eF)
				xcol <- truncate_vec(xcol, !eF)
			})
			if (neFLS[4]) gtext <- within(gtext, {
				text.size <- text.size[, !eF, drop=FALSE]
				text.color <- text.color[, !eF, drop=FALSE]
				text <- text[, !eF, drop=FALSE]
				text_sel <- text_sel[, !eF, drop=FALSE]
				
				text.nonemptyFacets <- text.nonemptyFacets[!eF]
				
				text.col.legend.labels <- truncate_labels(text.col.legend.labels, !eF)
				text.col.legend.values <- truncate_other(text.col.legend.values, !eF)
				text.col.legend.palette <- truncate_other(text.col.legend.palette, !eF)
				text.col.legend.text <- truncate_other(text.col.legend.text, !eF)
				text.col.legend.hist.misc$values <- truncate_other(text.col.legend.hist.misc$values, !eF)
				text.col.legend.hist.misc$breaks <- truncate_other(text.col.legend.hist.misc$breaks, !eF)
				text.col.legend.show <- text.col.legend.show[!eF]
				text.col.legend.title <- text.col.legend.title[!eF]
				
				text.size.legend.labels <- truncate_labels(text.size.legend.labels, !eF)
				text.size.legend.values <- truncate_other(text.size.legend.values, !eF)
				text.size.legend.palette <- truncate_vec(text.size.legend.palette, !eF)
				text.size.legend.sizes <- truncate_other(text.size.legend.sizes, !eF)
				text.size.legend.text <- truncate_other(text.size.legend.text, !eF)
				text.size.legend.show <- text.size.legend.show[!eF]
				text.size.legend.title <- text.size.legend.title[!eF]

				text.xmod <- text.xmod[, !eF, drop = FALSE]
				text.ymod <- text.ymod[, !eF, drop = FALSE]
				
				xtsize <- truncate_vec(xtsize, !eF)
				xtcol <- truncate_vec(xtcol, !eF)
				xtext <- truncate_vec(xtext, !eF)
			})
			panel.names <- panel.names[!eF]
		}
	}

	c(list(npol=nrow(data), varnames=list(by=by, fill=gfill$xfill, symbol.size=gsymbol$xsize, symbol.col=gsymbol$xcol, symbol.shape=gsymbol$xshape, line.col=glines$xline, line.lwd=glines$xlinelwd, raster=graster$xraster, text.size=gtext$xtsize, text.col=gtext$xtcol), idnames=list(fill=gfill$fill.id, symbol=gsymbol$symbol.id, line=glines$line.id), treat_by_count = treat_by_count, data_by=data$GROUP_BY, nrow=nrow, ncol=ncol, panel.names=panel.names, along.names=along.names, plot.order=plot.order, any.legend=any.legend), gborders, gfill, glines, gsymbol, gtext, graster, gtiles, list(add_legends=add_legends))
}

###############################
###### to do: tidy up, e.g:
###############################
# remove_NA_facets <- function(g, type, aes, xs) {
# 	g[xs] <- lapply(g[xs], function(gi) gi[, !eF, drop = FALSE])
# 
# 	lname <- sapply(aes, function(a) paste(a, "legend.labels", sep = "."))
# 	oname <- unlist(lapply(aes, function(a) paste(a, c("legend.labels", "legend.values"))))
# 	hname <- paste(aes[1], "legend.hist.misc", sep = ".")
# 	vname <- unlist(lapply(aes, function(a) paste(a, c("legend.show", "legend.title"))))
# 
# 	fname <- paste(type, "nonemptyFacets", sep = ".")
# 	
# 	g[lname] <- lapply(g[lname], function(gi) truncate_labels(gi, !eF))
# 	g[oname] <- lapply(g[oname], function(gi) truncate_other(gi, !eF))
# 	g[[hname]] <- local({
# 		gi <- g[[hname]]
# 		gi$values <- truncate_other(gi$values, !eF)
# 		gi$breaks <- truncate_other(gi$breaks, !eF)
# 		gi
# 	})
# 	g[vname] <- lapply(g[vname], function(gi) gi[!eF])
# 	g[[fname]] <- g[[fname]][!eF]
# 	
# 	g[xs] <- lapply(g[xs], function(gi) truncate_vec(gi, !eF))
# }
# 
# gfill <- remove_NA_facets(gfill, "fill", "fill", "xfill")
# glines <- remove_NA_facets(glines, "line", c("line.col", "line.lwd"), c("xline", "xlinelwd"))
# gsymbol <- remove_NA_facets(gsymbol, "symbol", c("symbol.col", "symbol.size", "symbol.shape"), c("xcol", "xsize", "xshape"))


truncate_label_vec <- function(label, sel) {
	structure(label[sel], align = attr(label, "align"))	
}

truncate_labels <- function(labels, sel) {
	if (is.list(labels)) {
		labels[sel]
	} else {
		labels #truncate_label_vec(labels, sel)
	}
}
truncate_vec <- function(x, sel) {
	if (length(x) > 1) {
		x[sel]
	} else {
		x
	}
}


truncate_other <- function(x, sel) {
	if (is.list(x)) {
		x[sel]
	} else {
		x
	}
}
