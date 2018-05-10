process_data <- function(data, filter, by, free.scales, is.colors, split.by=TRUE) {
	
	nby <- nlevels(by)
	cls <- check_tm_classes(data, is.colors)
	data[!filter, ] <- NA
	by[!filter] <- NA
	if (nby > 1) {
		nx <- nby
		dat <- data[[1]]
		if (cls[1]=="cha") {
			dat <- as.factor(dat)
			cls[1] <- "fac"
		}
		if (cls[1]=="fac") xlvls <- levels(dat)
		X <- lapply(levels(by), function(l) {
			sel <- by==l
			sel[is.na(sel)] <- FALSE
			dat2 <- if (cls[1]=="fac") {
				ch <- ifelse(sel, as.character(dat), NA) 
				lvls <- if (free.scales) intersect(xlvls, ch) else xlvls
				factor(ch, levels=lvls)
			} else {
				if (split.by) {
					ifelse(sel, dat, NA)	
				} else dat
			}
			
			attr(dat2, "sel") <- sel
			dat2
		})
		
		Xsel <- lapply(X, attr, "sel")
		
		names(X) <- levels(by)
		if (cls[1]=="col") {
			M <- matrix(unlist(X), ncol=nby)
			sel  <- matrix(unlist(Xsel), ncol=nby)
			
			attr(M, "sel") <- sel
			attr(M, "anyNA") <- apply(is.na(M) & sel, MARGIN = 2, any)
			attr(M, "allNA") <- !apply(!is.na(M) & sel, MARGIN = 2, any)

			return(M)
		} else if (cls[1]=="num" && !free.scales) {
			Y <- unlist(X)
			attr(Y, "sel") <- unlist(Xsel)
		} else {
			Y <- X
			attr(Y, "sel") <- matrix(unlist(Xsel), ncol=nby)
		}
		
		attr(Y, "anyNA") <- sapply(X, function(i) any(is.na(i) & attr(i, "sel")))
		attr(Y, "allNA") <- sapply(X, function(i) all(is.na(i)[attr(i, "sel")]))
		return(Y)
	} else {
		#data[!filter, ] <- NA
		
		sel <- matrix(filter, nrow = nrow(data), ncol=ncol(data))
		anyNA <- !apply(sel, MARGIN = 2, all)
		allNA <- !apply(sel, MARGIN = 2, any)
		
		if (all(cls=="col")) {
			m <- as.matrix(data)
			attr(m, "sel") <- sel
			attr(m, "anyNA") <- anyNA
			attr(m, "allNA") <- allNA
			return(m)
		}
		
		if (!free.scales) {
			if (all(cls=="num")) {
				datavec <- unlist(data)
				attr(datavec, "sel") <- sel
				attr(datavec, "anyNA") <- anyNA
				attr(datavec, "allNA") <- allNA
				return(datavec)
			} else {
				xlvls_list <- mapply(function(d, cl){
					if (cl=="fac") levels(d) else na.omit(unique(d))
				}, data, cls, SIMPLIFY=FALSE)
				
				xlvls <- unique(unlist(xlvls_list))
				datavec <- factor(unlist(lapply(data, as.character)), levels=xlvls)
				attr(datavec, "sel") <- sel
				attr(datavec, "anyNA") <- anyNA
				attr(datavec, "allNA") <- allNA
				return(datavec)
			}
		} else {
			if (any(cls=="cha")) data[cls=="cha"] <- lapply(data[cls=="cha"], as.factor)
			if (ncol(data)==1) {
				datavec <- data[[1]]
				attr(datavec, "sel") <- sel
				attr(datavec, "anyNA") <- anyNA
				attr(datavec, "allNA") <- allNA
				return(datavec)
			} else {
				datalist <- as.list(data)
				attr(datalist, "sel") <- sel #as.list(as.data.frame(sel))
				attr(datalist, "anyNA") <- anyNA
				attr(datalist, "allNA") <- allNA
				return(datalist)
			}
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
