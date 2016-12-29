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
			} else if (is.function(glf)) {
				gtlf
			} else {
				to_be_assigned <- setdiff(names(gtlf), names(lf))
				lf[to_be_assigned] <- gtlf[to_be_assigned]
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
			glf[to_be_assigned] <- gtlf[to_be_assigned]
			glf
		}
	}
}
