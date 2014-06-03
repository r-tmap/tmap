process_data <- function(data, by, free.scales) {
	nby <- nlevels(by)
	cls <- check_geo_classes(data)
	if (nby > 1) {
		nx <- nby
		dat <- data[[1]]
		if (cls[1]=="fac") xlvls <- levels(dat)
		X <- lapply(levels(by), function(l) {
			if (cls[1]=="fac") {
				ch <- ifelse(by==l, as.character(dat), NA) 
				lvls <- if (free.scales) intersect(xlvls, ch) else xlvls
				factor(ch, levels=lvls)
			} else {
				ifelse(by==l, dat, NA)
			}
		})
		names(X) <- x <- levels(data$GROUP_BY)
		if (cls[1]=="col") {
			return(matrix(unlist(X), ncol=nby))
		}
		return(X)			
	} else {
		if (all(cls=="col")) return(as.matrix(data))
		
		if (!free.scales) {
			if (all(cls=="num")) {
				return(unlist(data))
			} else {
				xlvls <- if (cls[1]=="fac") levels(xlvls) else unique(data[[1]])
				if (ncol(data)>1) xlvls <- intersect(xlvls, unique(unlist(lapply(data, as.character))))
				return(factor(unlist(lapply(dat, as.character)), levels=xlvls))
			}
		} else {
			if (any(cls=="cha")) data[, cls=="cha"] <- lapply(data[, cls=="cha"], as.factor)
			return(as.list(data))
		}
	}
}


check_geo_classes <- function(x) {
	sapply(x, function(y) {
		if (is.numeric(y)) { 
			"num"
		} else if (is.factor(y)) {
			"fac"
		} else if (all(valid_colors(y))) {
			"col"
		} else "cha"
	})
}
