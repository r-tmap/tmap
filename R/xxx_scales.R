are_breaks_diverging <- function(brks) {
    # if !divx then c-Inf, 2, 5, 10) is considered sequential
    negb <- any(brks[brks!=-Inf]<0) || (brks[1] == -Inf && brks[2]<=0)
    nb <- length(brks)
    posb <- any(brks[brks!=Inf]>0) || (brks[nb] == Inf && brks[nb-1]>=0)
    negb && posb
}

fancy_breaks <- function(vec, intervals=FALSE, interval.closure="left", fun=NULL, scientific=FALSE, text.separator="to", text.less.than=c("less", "than"), text.or.more=c("or", "more"), text.align="left", text.to.columns=FALSE, digits=NA, ...) {
    args <- list(...)
    n <- length(vec)

    if (!is.null(fun)) {
        x <- do.call(fun, list(vec))
    } else {
        ### analyse the numeric vector
    	if (all(is.infinite(vec))) {
    		x <- as.character(vec)
    	} else {
    		vec_fin <- unique(vec[!is.infinite(vec)])
    		frm <- gsub(" ", "", sprintf("%20.10f", abs(vec_fin)))
    		
    		# get width before decimal point
    		#if (length(frm)==0) browser()
    		mag <- max(nchar(frm)-11)
    		
    		# get number of decimals (which is number of decimals in vec, which is reduced when mag is large)
    		ndec <- max(10 - nchar(frm) + nchar(sub("0+$","",frm)))
    		if (is.na(digits)) {
    			digits <- max(min(ndec, 4-mag), 0)
    			
    			# add sign to frm
    			frm_sign <- paste0(ifelse(vec_fin<0, "-", "+"), frm)
    			
    			# test if number of digits is sufficient for unique labels
    			if (!scientific) {
    				while (anyDuplicated(substr(frm_sign, 1, nchar(frm_sign)-10 + digits)) && (digits < 10)) {
    					digits <- digits + 1
    				}
    			}
    			
    		}
    		
    		if (!scientific) {
    			if (mag>11 || (mag > 9 && all(vec - floor(vec/1e9)*1e9 < 1))) {
    				vec <- vec / 1e9
    				ext <- " bln"
    			} else if (mag > 8 || (mag > 6 && all(vec - floor(vec/1e6)*1e6 < 1))) {
    				vec <- vec / 1e6
    				ext <- " mln"
    			} else {
    				ext <- ""
    			}
    			
    			# set default values
    			if (!("big.mark" %in% names(args))) args$big.mark <- ","
    			if (!("format" %in% names(args))) args$format <- "f"
    			if (!("preserve.width" %in% names(args))) args$preserve.width <- "none"
    			x <- paste(do.call("formatC", c(list(x=vec, digits=digits), args)), ext, sep="")
    		} else {
    			if (!("format" %in% names(args))) args$format <- "g"
    			x <- do.call("formatC", c(list(x=vec, digits=digits), args))
    		}
    	}
    }

    if (intervals) {
        if (scientific) {
            if (interval.closure=="left") {
                lbls <- paste("[", x[-n], ", ", x[-1], ")", sep="")
                lbls[n-1] <- paste(substr(lbls[n-1], 1, nchar(lbls[n-1])-1), "]", sep="")
            } else {
                lbls <- paste("(", x[-n], ", ", x[-1], "]", sep="")
                lbls[1] <- paste("[", substr(lbls[1], 2, nchar(lbls[1])), sep="")
            }
        } else {
            x[vec==-Inf] <- ""
            
            lbls <- paste(x[-n], x[-1], sep = paste0(" ", text.separator, " "))
            if (vec[1]==-Inf) lbls[1] <- paste(paste(text.less.than, collapse = " "), x[2], sep = " ")
            if (vec[n]==Inf) lbls[n-1] <- paste(x[n-1], paste(text.or.more, collapse = " "), sep = " ")
            
            if (text.to.columns) {
            	#xtra <- as.numeric(!is.na(text.align) && text.align=="right")
            	
            	
            	nc1 <- nchar(paste(x[-n], " ", sep = "")) + 1
            	nc2 <- rep(nchar(paste(text.separator, " ", sep = "")), n-1)

            	lbls_breaks <- matrix(c(nc1, nc1+nc2), ncol=2)
            		
            	if (vec[1]==-Inf) {
            		if (length(text.less.than)==1) {
            			lbls_breaks[1,] <- rep(nchar(paste(text.less.than[1], " ", sep = "")) + 1, 2)
            		} else {
            			lbls_breaks[1,] <- cumsum(c(nchar(paste(text.less.than[1], " ", sep = "")) + 1, nchar(text.less.than[2])+1))
            		}
            	}
            	if (vec[n]==Inf) { 
            		if (length(text.or.more)==1) {
            			lbls_breaks[n-1,] <- rep(nchar(paste(x[n-1], " ", sep = "")) + 1, 2)	
            		} else {
            			lbls_breaks[n-1,] <- cumsum(c(nchar(paste(x[n-1], " ", sep = "")) + 1, nchar(text.or.more[1])+1))
            		}
            		
            	}

            	attr(lbls, "brks") <- lbls_breaks
            }
        }
    }

    y <- if (intervals) lbls else x
    attr(y, "align") <- text.align
    y
}


num2breaks <- function(x, n, style, breaks, approx=FALSE, interval.closure="left", var = NULL) {
	nobs <- sum(!is.na(x))
	# create intervals and assign colors
	if (style=="fixed") {
		q <- list(var=x,
				  brks=breaks)
		attr(q, "style") <- "fixed"
		attr(q, "nobs") <- nobs
		attr(q, "intervalClosure") <- interval.closure
		class(q) <- "classIntervals"
	} else {
		if (nobs==0) {
			if (!is.null(var)) {
				stop("Numerical variable \"", var, "\" only contains missing values.", call.=FALSE)
			} else {
				stop("Numerical variable only contains missing values.", call.=FALSE)
			}
		}

		nunique <- length(na.omit(unique(x)))

		
		if (nunique == 1 && style!="pretty") {
			if (!is.null(var)) {
				warning("Single unique value found for the variable \"", var, "\", so style set to \"pretty\"", call. = FALSE)
			} else {
				warning("Single unique value found, so style set to \"pretty\"", call. = FALSE)
			}
		}

		tempx <- nunique <= n
		
		if (tempx) {
			x_orig <- x
			if (length(na.omit(unique(x))) == 1) x <- pretty(x)
			x <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1)
		}
		
		q <- suppressWarnings(classIntervals(x, n, style= style, intervalClosure=interval.closure))
		
		if (tempx) q$var <- x_orig

	}

    if (approx && style != "fixed") {
        if (n >= length(unique(x)) && style=="equal") {
            # to prevent classIntervals to set style to "unique"
            q <- list(var=x, brks=seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out=n))
            attr(q, "intervalClosure") <- interval.closure
            class(q) <- "classIntervals"
        } else {
            brks <- q$brks

            # to prevent ugly rounded breaks such as -.5, .5, ..., 100.5 for n=101
            qm1 <- suppressWarnings(classIntervals(x, n-1, style= style, intervalClosure=interval.closure))
            brksm1 <- qm1$brks
            qp1 <- suppressWarnings(classIntervals(x, n+1, style= style, intervalClosure=interval.closure))
            brksp1 <- qp1$brks
            if (min(brksm1) > min(brks) && max(brksm1) < max(brks)) {
                q <- qm1
            } else if (min(brksp1) > min(brks) && max(brksp1) < max(brks)) {
                q <- qp1
            }
        }
    }
    q
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
