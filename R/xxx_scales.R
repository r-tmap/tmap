are_breaks_diverging <- function(brks) {
    # if !divx then c-Inf, 2, 5, 10) is considered sequential
    negb <- any(brks[brks!=-Inf]<0) || (brks[1] == -Inf && brks[2]<=0)
    nb <- length(brks)
    posb <- any(brks[brks!=Inf]>0) || (brks[nb] == Inf && brks[nb-1]>=0)
    negb && posb
}








# function to determine whether a diverging of sequential palette is used given the values and the breaks
# use_diverging_palette <- function(v, brks, midpoint = NULL) {
# 	if (!is.null(midpoint) && !is.na(midpoint)) return(TRUE)
# 	x <- na.omit(v)
# 	divx <- any(x<0) && any(x>0)
# 
# 	if (divx || is.null(brks)) {
# 		return(divx)
# 	} else {
# 		are_breaks_diverging(brks)
# 	}
# }
# 
use_div <- function(brks, midpoint = NULL) {
	if (!is.null(midpoint) && !is.na(midpoint)) return(TRUE)

	if (is.null(brks)) {
		return(NA)
	} else {
		are_breaks_diverging(brks)
	}
}


default_contrast_seq <- function(k) {
    c1 <- max((9-k) * (.15/6), 0)
    c2 <- min(.7 + (k-3) * (.3/6), 1)
    c(c1,c2)
}

default_contrast_div <- function(k) {
    c(0, min(.6 + (k-3) * (.4/8), 1))
}
