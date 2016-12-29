process_data <- function(data, by, free.scales, is.colors, split.by=TRUE) {
	
	nby <- nlevels(by)
	cls <- check_tm_classes(data, is.colors)
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
				if (split.by) {
					ifelse(by==l, dat, NA)	
				} else dat
			}
		})
		names(X) <- levels(by)
		if (cls[1]=="col") {
			return(matrix(unlist(X), ncol=nby))
		} else if (cls[1]=="num" && !free.scales) {
			Y <- unlist(X)
		} else Y <- X
		attr(Y, "anyNA") <- any(is.na(dat))
		return(Y)
	} else {
		if (all(cls=="col")) return(as.matrix(data))
		
		if (!free.scales) {
			if (all(cls=="num")) {
				return(unlist(data))
			} else {
				xlvls_list <- mapply(function(d, cl){
					if (cl=="fac") levels(d) else na.omit(unique(d))
				}, data, cls, SIMPLIFY=FALSE)
				
				xlvls <- unique(unlist(xlvls_list))
				return(factor(unlist(lapply(data, as.character)), levels=xlvls))
			}
		} else {
			if (any(cls=="cha")) data[cls=="cha"] <- lapply(data[cls=="cha"], as.factor)
			if (ncol(data)==1) {
				return(data[[1]])
			} else return(as.list(data))
		}
	}
}


check_tm_classes <- function(x, is.colors) {
	if (is.colors) {
		rep("col", ncol(x))	
	} else {
		sapply(x, function(y) {
			if (is.numeric(y)) { 
				"num"
			} else if (is.factor(y)) {
				"fac"
			} else if (is.logical(y)) {
				"cha"
			} else if (all(valid_colors(y))) {
				"col"
			} else "cha"
		})
	}
}
