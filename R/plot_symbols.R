plot_symbols <- function(co.native, g, gt, lineInch, lineNatH, lineNatW, i, k) {
	symbolH <- lineNatH * gt$scale
	symbolW <- lineNatW * gt$scale
	shapeLib <- get("shapeLib", envir = .TMAP_CACHE)
	justLib <- get("justLib", envir = .TMAP_CACHE)
	
	with(g, {
		npol <- nrow(co.native)
		if (length(symbol.size)!=npol) {
			if (length(symbol.size)!=1) warning("less symbol size values than objects", call. = FALSE)
			symbol.size <- rep(symbol.size, length.out=npol)
		}
		
		size.native.w <- convertWidth(unit(symbol.size, "inch"), "native", valueOnly = TRUE)
		size.native.h <- convertHeight(unit(symbol.size, "inch"), "native", valueOnly = TRUE)
		
		# determine justification per symbol
		just <- g$symbol.misc$just
		justs <- lapply(symbol.shape, function(ss) {
			if (!is.na(ss) && ss>999) {
				js <- justLib[[ss-999]]
				if (is.na(js[1])) just else js
			} else just
		})
		justs.x <- vapply(justs, "[[", numeric(1), 1)
		justs.y <- vapply(justs, "[[", numeric(1), 2)
		justx <- size.native.w * (justs.x-.5)
		justy <- size.native.h * (justs.y-.5)
		
		# adjust the coordinates
		co.native[, 1] <- co.native[, 1] + symbol.xmod * symbolW + justx * lineNatW * 2 / 3
		co.native[, 2] <- co.native[, 2] + symbol.ymod * symbolH + justy * lineNatH * 2 / 3
		
		sel <- !is.na(symbol.size) & !is.na(symbol.col) & !is.na(symbol.shape)
		
		# return NULL is no symbols are selected (see tm_facets example)
		if (!any(sel)) return(NULL)
		
		if (!all(sel)) {
			co.native <- co.native[sel, , drop=FALSE]
			symbol.size <- symbol.size[sel]
			symbol.col <- symbol.col[sel]
			symbol.shape <- symbol.shape[sel]
			npol <- sum(sel)
		}
		symbol.size <- symbol.size * lineInch
		
		if (length(symbol.size)!=1) {
			decreasing <- order(-symbol.size)
			co.native2 <- co.native[decreasing,,drop=FALSE]
			symbol.size2 <- symbol.size[decreasing]
			symbol.shape2 <- symbol.shape[decreasing]
			symbol.col2 <- symbol.col[decreasing]
		} else {
			co.native2 <- co.native
			symbol.size2 <- symbol.size
			symbol.shape2 <- symbol.shape
			symbol.col2 <- symbol.col
		}
		
		bordercol <- symbol.border.col
		idName <- paste("tm_symbols", i, k, sep="_")
		
		if (any(!is.na(symbol.shape2) & symbol.shape2>999)) {
			gpars <- get_symbol_gpar(x=symbol.shape2,
									 fill=symbol.col2,
									 col=bordercol,
									 lwd=symbol.border.lwd,
									 separate=TRUE)
			grobs <- lapply(1:npol, function(i) {
				if (!is.na(symbol.shape2[i]) && symbol.shape2[i]>999) {
					grbs <- if (is.na(bordercol)) {
						gList(shapeLib[[symbol.shape2[i]-999]])	
					} else {
						gList(shapeLib[[symbol.shape2[i]-999]], rectGrob(gp=gpar(fill=NA, col=bordercol, lwd=symbol.border.lwd)))	
					}
					gTree(children=grbs, vp=viewport(x=unit(co.native2[i,1], "native"), 
													 y=unit(co.native2[i,2], "native"),
													 width=unit(symbol.size2[i]*2/3, "inch"),
													 height=unit(symbol.size2[i]*2/3, "inch")))
				} else {
					pointsGrob(x=unit(co.native2[i,1], "native"), y=unit(co.native2[i,2], "native"),
							   size=unit(symbol.size2[i], "inch"),
							   pch=symbol.shape2[i],
							   gp=gpars[[i]])
				}
			})
			x <- gTree(children=do.call(gList, grobs), name=idName)
		} else {
			pointsGrob(x=unit(co.native2[,1], "native"), y=unit(co.native2[,2], "native"),
					   size=unit(symbol.size2, "inch"),
					   pch=symbol.shape2,
					   gp=get_symbol_gpar(x=symbol.shape2,
					   				   fill=symbol.col2,
					   				   col=bordercol,
					   				   lwd=symbol.border.lwd), 
					   name=idName)
		}
		
	})
}
