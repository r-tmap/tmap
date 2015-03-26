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
		step <- round((h-1)*contrast/((max(npos, nneg)-.5)*2))
	} else {
		npos <- sum(breaks>=0) - !is.div
		nneg <- sum(breaks<=0) - !is.div
		step <- 0
	}
	
	pid <- h + step
	nid <- h - step
	
	ids <- rep(h, nbrks-1)
	if (npos>0) ids[(nbrks-npos):(nbrks-1)] <- pid + 
        seq(0, (n-pid)/mx*hg*contrast, length.out=npos)
	if (nneg>0) ids[1:nneg] <- seq(nid-((nid-1)/mx*-lw*contrast), nid, 
                                   length.out=nneg)	
	ids
}



# Map breaks to index numbers of a sequential colour scale
# 
# Determines index numbers of a potential sequential colour scale given a vector of breaks.
# 
# @param breaks vector of breaks
# @param n number of classes, i.e. the length of a sequential colour palette.
# @param contrast value between 0 and 1 that determines how much of the \code{(1, n)} range is used. Value \code{contrast=1} means that the most extreme break value, i.e. \code{max(abs(breaks))} is maped to n. There is no contrast at all for \code{contrast=0}, i.e. all index numbers will correspond to the first class (which has index number \code{1}.
# @return vector of index numbers
map2seqscaleID <- function(breaks, n=101, contrast=1) {
	if (any(breaks<0) && any(breaks>0)) stop("Breaks contains positive and negative values. Use diverging scale instead.")
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
		warning("some index numbers exceed n and are replaced by NA")
		ids[ids>n] <- NA
	} else if (any(ids<1)) {
		warning("some index numbers exceed 0 and are replaced by NA")
		ids[ids<1] <- NA
	}
	ids
}