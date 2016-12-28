add_data_to_gps <- function(gps, gm, datasets, matchIDs, interactive) {
	# append data to gps
	mapply(function(gp, masterID) {
		gp[-length(gp)] <- mapply(function(gpl, indices, dt) {
			dt$SHAPE_AREAS <- NULL
			
			if (interactive) {
				
				# add density values
				if (!is.na(gpl$xfill[1])) {
					if (gpl$fill.legend.hist.misc$densities) {
						densvar <- paste(gpl$xfill[1], "density", sep="_")
						dt[[densvar]] <- gpl$fill.legend.hist.misc$values
						gplxfill <- c(gpl$xfill, densvar)
					} else {
						gplxfill <- gpl$xfill
					}
				} else gplxfill <- NA
				
				if (is.null(gpl$fill.popup.vars) || identical(gpl$fill.popup.vars, FALSE)) {
					gpl$fill.popup.vars <- NA
				} else if (identical(gpl$fill.popup.vars, TRUE)) {
					gpl$fill.popup.vars <- names(dt)
				} else if (is.na(gpl$fill.popup.vars[1])) {
					gpl$fill.popup.vars <- if (is.na(gplxfill[1])) names(dt) else gplxfill
				} else {
					if (!all(gpl$fill.popup.vars %in% names(dt))) stop("Not all popup variables are contained in the data", call.=FALSE)
				}
				
				if (is.null(gpl$line.popup.vars) || identical(gpl$line.popup.vars, FALSE)) {
					gpl$line.popup.vars <- NA
				} else if (identical(gpl$line.popup.vars, TRUE)) {
					gpl$line.popup.vars <- names(dt)
				} else if (is.na(gpl$line.popup.vars[1])) {
					gpl$line.popup.vars <- unique(na.omit(c(gpl$xline, gpl$xlinelwd)))
					if (length(gpl$line.popup.vars) == 0) gpl$line.popup.vars <- names(dt)
				} else {
					if (!all(gpl$line.popup.vars %in% names(dt))) stop("Not all popup variables are contained in the data", call.=FALSE)
				}
				
				if (is.null(gpl$symbol.popup.vars) || identical(gpl$symbol.popup.vars, FALSE)) {
					gpl$symbol.popup.vars <- NA
				} else if (identical(gpl$symbol.popup.vars, TRUE)) {
					gpl$symbol.popup.vars <- names(dt)
				} else if (is.na(gpl$symbol.popup.vars[1])) {
					gpl$symbol.popup.vars <- unique(na.omit(c(gpl$xsize, gpl$xcol, gpl$xshape)))
					if (length(gpl$symbol.popup.vars) == 0) gpl$symbol.popup.vars <- names(dt)
				} else {
					if (!all(gpl$symbol.popup.vars %in% names(dt))) stop("Not all popup variables are contained in the data", call.=FALSE)
				}
				
				gpl$fill.names <- gpl$idnames$fill
				gpl$line.names <- gpl$idnames$line
				gpl$symbol.names <- gpl$idnames$symbol
			}
			
			gpl$data <- dt[indices, , drop=FALSE]
			gpl
		}, gp[-length(gp)], masterID, datasets, SIMPLIFY = FALSE)
		gp
	}, gps, matchIDs, SIMPLIFY=FALSE)
}