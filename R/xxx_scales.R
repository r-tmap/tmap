are_breaks_diverging <- function(brks) {
    # if !divx then c-Inf, 2, 5, 10) is considered sequential
    negb <- any(brks[brks!=-Inf]<0) || (brks[1] == -Inf && brks[2]<=0)
    nb <- length(brks)
    posb <- any(brks[brks!=Inf]>0) || (brks[nb] == Inf && brks[nb-1]>=0)
    negb && posb
}


# Map breaks to index numbers of a diverging colour scale
#
# Determines index numbers of a potential diverging colour scale given a vector of breaks.
#
# @param breaks vector of breaks
# @param n number of classes, i.e. the length of a diverging colour palette. This should preferable be an odd number, since it contains a neutral middle color.
# @param contrast value between 0 and 1 that determines how much of the \code{(1, n)} range is used. Value \code{contrast=1} means that the most extreme break value, i.e. \code{max(abs(breaks))} is maped to either 1 or n (depending on whether it is a minimum or maximum). There is no contrast at all for \code{contrast=0}, i.e. all index numbers will correspond to the middle class (which has index number \code{((n-1)/2)+1}.
# @return vector of index numbers
map2divscaleID <- function(breaks, n=101, contrast=1) {
    nbrks <- length(breaks)

    if (length(contrast)==1) {
        contrast <- c(0, contrast)
    }
    crange <- contrast[2] - contrast[1]

    lw <- breaks[1]
    hg <- breaks[nbrks]

    # omit infinity values
    if (lw==-Inf) lw <- breaks[2]
    if (hg==Inf) hg <- breaks[nbrks-1]
    mx <- max(abs(c(lw, hg)))


    is.div <- any(breaks<0) && any(breaks>0)

    cat0 <- !any(breaks==0)

    h <- ((n-1)/2)+1

    if (is.div && !cat0) {
        npos <- sum(breaks>0)
        nneg <- sum(breaks<0)
        step <- round((h-1)*crange/((max(npos, nneg)-.5)*2))
    } else {
        npos <- sum(breaks>=0) - !is.div
        nneg <- sum(breaks<=0) - !is.div
        step <- 0
    }

    pid <- h + step
    nid <- h - step

    ids <- rep(h, nbrks-1)
    if (npos>0) ids[(nbrks-npos):(nbrks-1)] <- pid +
        seq((n-pid)/mx*hg*contrast[1], (n-pid)/mx*hg*contrast[2], length.out=npos)
    if (nneg>0) ids[1:nneg] <- seq(nid-((nid-1)/mx*-lw*contrast[2]), nid-((nid-1)/mx*-lw*contrast[1]),
                                   length.out=nneg)
    if (is.div && cat0) ids[nneg] <- h
    round(ids)
}



# Map breaks to index numbers of a sequential colour scale
#
# Determines index numbers of a potential sequential colour scale given a vector of breaks.
#
# @param breaks vector of breaks
# @param n number of classes, i.e. the length of a sequential colour palette.
# @param contrast value between 0 and 1 that determines how much of the \code{(1, n)} range is used. Value \code{contrast=1} means that the most extreme break value, i.e. \code{max(abs(breaks))} is maped to n. There is no contrast at all for \code{contrast=0}, i.e. all index numbers will correspond to the first class (which has index number \code{1}.
# @param breaks.specified logical that determines whether breaks have been specified by the user. If so a warning is shown if breaks are diverging.
# @return vector of index numbers
map2seqscaleID <- function(breaks, n=101, contrast=1, breaks.specified=TRUE, impute=TRUE) {
	if (are_breaks_diverging(breaks) && breaks.specified) warning("Breaks contains positive and negative values. Better is to use diverging scale instead, or set auto.palette.mapping to FALSE.", call. = FALSE)
	m <- (n*2)-1
	mh <- ((m-1)/2)+1
	ids <- map2divscaleID(breaks, n=m, contrast=contrast)

	ids <- if (any(breaks>0)) {
		ids - mh + 1
	} else {
		(mh+1) - ids
	}

	# checks:
	if (any(ids>n)) {
		if (impute) {
			ids[ids>n] <- n
		} else {
			warning("Some index numbers exceed n and are replaced by NA", call. = FALSE)
			ids[ids>n] <- NA
		}

	} else if (any(ids<1)) {
		if (impute) {
			ids[ids<1] <- 1
		} else {
			warning("Some index numbers exceed 0 and are replaced by NA", call. = FALSE)
			ids[ids<1] <- NA
		}
	}
	round(ids)
}



# function to determine whether a diverging of sequential palette is used given the values and the breaks
use_diverging_palette <- function(v, brks) {
	x <- na.omit(v)
	divx <- any(x<0) && any(x>0)

	if (divx || is.null(brks)) {
		return(divx)
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
