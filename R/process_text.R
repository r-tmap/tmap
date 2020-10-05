check_text_specials <- function(fill, xtcol, xtsize, g, gt, gby, xvary, data, shpcols, nx, npol, interactive) {
	## text-specific aesthetic defaults
	if (gt$aes.colors.light["text"]) {
		collight <- gt$aes.colors["text"]
		coldark <- "black"
	} else {
		collight <- "white"
		coldark <- gt$aes.colors["text"]
	}
	
	## determine background of the text (normally defined by polygons below), such that text color will be collight or coldark
	if (is.na(fill[1])) fill <- ifelse(gt$aes.colors.light["text"], "black", "white")
	
	

	
	
	## set dummy variable for size aesthetic (if not variable)
	if (!xvary["text.size"]) {
		if (!all(is.numeric(xtsize) | xtsize=="AREA")) stop("Incorrect text sizes.", call. = FALSE)
		if (is.numeric(xtsize[1])) {
			g$size.lowerbound <- 0
		}
		if (any(xtsize=="AREA") && !("SHAPE_AREAS" %in% shpcols)) stop("size=\"AREA\" only valid for spatial polygons.", call.=FALSE)
		for (i in 1:nx) data[[paste("SIZE", i, sep="_")]] <- if (is.numeric(xtsize[i])) xtsize[i] else {
			tmp <- data$SHAPE_AREAS
			mx <- max(tmp, na.rm=TRUE)
			tmp2 <- (tmp / mx) ^ (1/g$root)
			isnan <- is.nan(tmp2)
			if (any(isnan)) tmp2[isnan] <- mx
			tmp2
		}
		xtsize <- paste("SIZE", 1:nx, sep="_")
		gby$free.scales.size <- FALSE
	}
	
	# check for direct color input
	is.colors <- all(valid_colors(xtcol)) || is.na(xtcol[1])
	if (!xvary["text.col"]) {
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
	
	xtext <- g$text
	
	if (!all(xtext %in% shpcols)) stop("Incorrect data variable used for the text", call. = FALSE)
	
	list(xtcol = xtcol, xtsize = xtsize, g=g, gby = gby, data = data, is.colors = is.colors, fill = fill, collight = collight, coldark = coldark, xtext = xtext)	
	
}


postprocess_text <- function(res, g, gt, data, npol, nx, just, interactive, text, collight, coldark, xtext) {
	if (g$shadow) {
		g$shadowcol <- if (is.matrix(res$text.col)) {
			apply(res$text.col, MARGIN=2, function(f) {
				light <- is_light(f)
				rep(ifelse(light, coldark, collight), length.out=npol)
			})
		} else {
			light <- is_light(res$text.col)
			rep(ifelse(light, coldark, collight), length.out=npol)
		}
	}
	
	text.just <- process_text_just(g$just, interactive)
	xmod <- if (is.character(g$xmod)) data[[g$xmod]] else rep(g$xmod, length.out=npol)
	ymod <-  if (is.character(g$ymod)) data[[g$ymod]] else rep(g$ymod, length.out=npol)
	
	xmod <- matrix(xmod, nrow=npol, ncol=nx)
	ymod <- matrix(ymod, nrow=npol, ncol=nx)
	
	if (is.na(g$fontface)) g$fontface <- gt$fontface
	if (is.na(g$fontfamily)) g$fontfamily <- gt$fontfamily
	
	text.bg.color <- do.call("process_color", c(list(col=g$bg.color, alpha=g$bg.alpha), gt$pc))
	text.shadowcol <- do.call("process_color", c(list(col=g$shadowcol), gt$pc))
	
	clustering <- g$clustering
	if (identical(clustering, FALSE)) {
		clustering <- NULL
	} else if (identical(clustering, TRUE)) {
		clustering <- leaflet::markerClusterOptions()	
	}
	
	res$text.col.legend.text <- res$text.col.legend.misc$legend.text
	res$text.col.legend.misc <- list()
	
	names(res)[names(res)== "text.size.max.size"] <- "text.col.legend.sizes"
	names(res)[names(res)== "text.size.text_sel"] <- "text_sel"
	names(res)[names(res)== "text.col"] <- "text.color"
	
	extra <- list(text = text,
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
				  text.just = text.just,
				  text.xmod=xmod,
				  text.ymod=ymod,
				  text.misc = list(clustering = clustering),
				  xtext = xtext)
	c(res, extra)
}


process_text <- function(data, g, fill, gt, gby, z, interactive) {
	# aesthetics
	xs <- list(text.size = g$size, text.col = g$col)
	process_aes(type = "text", xs, c("xtsize", "xtcol"), "text", data, g, gt, gby, z, interactive, fill)
}
