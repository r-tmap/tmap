process_data <- function(data, filter, by, free.scales, is.colors, split.by=TRUE, vary) {
	
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
				ch <- as.character(dat)
				ch[!sel] <- NA
				lvls <- if (free.scales) intersect(xlvls, ch) else xlvls
				factor(ch, levels=lvls)
			} else {
				if (split.by) dat[!sel] <- NA
				dat
			}
			
			attr(dat2, "sel") <- sel
			dat2
		})
		
		Xsel <- lapply(X, attr, "sel")
		
		names(X) <- levels(by)
		if (cls[1]=="col" || !vary) {
			M <- do.call(cbind, X) #matrix(unlist(X), ncol=nby)
			sel  <- do.call(cbind, Xsel) #matrix(unlist(Xsel), ncol=nby)
			
			attr(M, "sel") <- sel
			attr(M, "anyNA") <- apply(is.na(M) & sel, MARGIN = 2, any)
			attr(M, "allNA") <- !apply(!is.na(M) & sel, MARGIN = 2, any)

			return(M)
		} else if (!free.scales) {
			Y <- unlist(X, use.names = FALSE)
			attr(Y, "sel") <- unlist(Xsel, use.names = FALSE)
		} else {
			Y <- X
			attr(Y, "sel") <- do.call(cbind, Xsel) #matrix(unlist(Xsel), ncol=nby)
		}
		
		attr(Y, "anyNA") <- vapply(X, function(i) any(is.na(i) & attr(i, "sel")), logical(1))
		attr(Y, "allNA") <- vapply(X, function(i) all(is.na(i)[attr(i, "sel")]), logical(1))
		return(Y)
	} else {
		#data[!filter, ] <- NA
		
		sel <- matrix(filter, nrow = nrow(data), ncol=ncol(data))
		anyNA <- !apply(sel, MARGIN = 2, all)
		allNA <- !apply(sel, MARGIN = 2, any)
		
		if (all(cls=="col") || !vary) {
			m <- as.matrix(data)
			attr(m, "sel") <- sel
			attr(m, "anyNA") <- anyNA
			attr(m, "allNA") <- allNA
			return(m)
		}
		if (!free.scales) {
			if (all(cls %in% c("num", "uni"))) {
				if (all(cls == "uni")) {
					uni <- attr(data[[1]], "units")
					data <- lapply(data, function(dvec) {
						convert_to_num_with_unit_attr(dvec, uni)
					})
				}
				
				datavec <- unlist(data, use.names = FALSE)
				if (all(cls == "uni")) attr(datavec, "units") <- as.character(uni)
			} else {
				xlvls_list <- mapply(function(d, cl){
					if (cl=="fac") levels(d) else na.omit(unique(d))
				}, data, cls, SIMPLIFY=FALSE)
				
				xlvls <- unique(unlist(xlvls_list, use.names = FALSE))
				datavec <- factor(unlist(lapply(data, as.character), use.names = FALSE), levels=xlvls)
			}

			attr(datavec, "sel") <- sel
			attr(datavec, "anyNA") <- anyNA
			attr(datavec, "allNA") <- allNA
			return(datavec)
			
		} else {
			if (any(cls=="cha")) data[cls=="cha"] <- lapply(data[cls=="cha"], as.factor)
			if (ncol(data)==1) {
				datavec <- data[[1]]
				if (all(cls == "uni")) {
					datavec <- convert_to_num_with_unit_attr(datavec)
				} else if (any(cls == "uni")) {
					datavec <- as.numeric(datavec)
				}
				attr(datavec, "sel") <- sel
				attr(datavec, "anyNA") <- anyNA
				attr(datavec, "allNA") <- allNA
				return(datavec)
			} else {
				datalist <- as.list(data)
				
				datalist <- mapply(function(datavec, cl) {
					if (cl == "cha") {
						as.factor(datavec)
					} else if (cl == "uni") {
						datavec <- convert_to_num_with_unit_attr(datavec)
					} else datavec
				}, datalist, cls, SIMPLIFY = FALSE)
				
				attr(datalist, "sel") <- sel #as.list(as.data.frame(sel))
				attr(datalist, "anyNA") <- anyNA
				attr(datalist, "allNA") <- allNA
				return(datalist)
			}
		}
	}
}

convert_to_num_with_unit_attr <- function(x, uni = NULL) {
	if (is.null(uni)) {
		uni <- attr(x, "units")
	} else {
		x <- set_units(x, uni, mode = "standard")
	}
	
	uni_char <- as.character(uni)
	
	x <- as.numeric(x)
	attr(x, "units") <- uni_char
	x
}

check_tm_classes <- function(x, is.colors) {
	if (is.colors) {
		rep("col", ncol(x))	
	} else {
		vapply(x, function(y) {
			if (inherits(y, "units")) {
				"uni"
			} else if (is.numeric(y)) { 
				"num"
			} else if (is.factor(y)) {
				"fac"
			} else if (is.logical(y)) {
				"cha"
			} else if (all(valid_colors(y))) {
				"col"
			} else "cha"
		}, character(1))
	}
}
