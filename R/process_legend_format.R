process_legend_format <- function(glf, gtlf, nx) {
	
	# check if g$legend.format is list of lists or functions
	islist <- is.list(glf) && length(glf)>0 && is.list(glf[[1]])

	# unlist if nx==1
	if (islist && nx==1) {
		glf <- glf[[1]]
		islist <- FALSE
	}
	
	if (islist) {
		glf <- rep(glf, length.out=nx)
		lapply(glf, function(lf) {
			
			if (is.function(lf)) {
				lf
			} else if (is.function(gtlf)) {
				gtlf
			} else {
				to_be_assigned <- setdiff(names(gtlf), names(lf))
				big.num.abbr.set <- "big.num.abbr" %in% names(lf)
				lf[to_be_assigned] <- gtlf[to_be_assigned]
				attr(lf, "big.num.abbr.set") <- big.num.abbr.set
				lf
			}
		})
	} else {
		if (is.function(glf)) {
			glf
		} else if (is.function(gtlf)) {
			gtlf
		} else {
			to_be_assigned <- setdiff(names(gtlf), names(glf))
			big.num.abbr.set <- "big.num.abbr" %in% names(glf)
			glf[to_be_assigned] <- gtlf[to_be_assigned]
			attr(glf, "big.num.abbr.set") <- big.num.abbr.set
			glf
		}
	}
}

process_popup_format <- function(gpf, gtlf, vars, show.warnings) {
	# check if g$legend.format is list of lists or functions
	islist <- is.list(gpf) && length(gpf)>0 && is.list(gpf[[1]])
	
	if (!islist) {
		process_legend_format(gpf, gtlf, nx=1)
	} else {
		nms <- names(gpf)
		if (is.na(vars[1])) {
			if (show.warnings) warning("popup.vars not specified whereas popup.format is a list", call. = FALSE)
			return(process_legend_format(gpf[[1]], gtlf, nx=1))
		}
		if (!all(nms %in% vars)) stop("popup.format names do not correspond to popup.vars", call. = FALSE)
		lapply(vars, function(v) {
			if (v %in% nms) {
				process_legend_format(gpf[[v]], gtlf, nx=1)
			} else {
				process_legend_format(list(), gtlf, nx=1)
			}
		})
	}
}
