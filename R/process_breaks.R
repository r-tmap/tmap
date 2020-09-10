fancy_breaks <- function(vec, as.count = FALSE, intervals=FALSE, interval.closure="left", fun=NULL, scientific=FALSE, big.num.abbr = c("mln" = 6, "bln" = 9), prefix = "", suffix = "", text.separator="to", text.less.than=c("less", "than"), text.or.more=c("or", "more"), text.align="left", text.to.columns=FALSE, digits=NA, ...) {
	args <- list(...)
	n <- length(vec)
	
	if (!is.null(fun)) {
		x <- do.call(fun, list(vec))
	} else if (all(is.infinite(vec))) {
		x <- as.character(vec)
	} else {
		# calculate magnitude, needed to determine digits and big number abbreviations
		vec_fin <- unique(vec[!is.infinite(vec)])
		frm <- gsub(" ", "", sprintf("%20.10f", abs(vec_fin)))
		mag <- max(nchar(frm)-11)
		
		if (as.count) {
			steps <- (vec[-1] - vec[-n])
			vec <- c(vec, vec - 1L, vec + 1L) # needed for: {1, 2, ... 9}
			digits <- 0
		} else {
			# get number of decimals (which is number of decimals in vec, which is reduced when mag is large)
			ndec <- max(10 - nchar(frm) + nchar(sub("0+$","",frm)))
			if (is.na(digits)) {
				digits <- max(min(ndec, 4-mag), 0)
				
				# add sign to frm
				frm_sign <- unique(paste0(ifelse(vec_fin<0, "-", "+"), frm))
				
				# test if number of digits is sufficient for unique labels
				if (!scientific) {
					while (anyDuplicated(substr(frm_sign, 1, nchar(frm_sign)-10 + digits)) && (digits < 10)) {
						digits <- digits + 1
					}
				}
				
			}
		}
		
		if (!scientific || as.count) {
			
			# check whether big number abbrevations should be used
			ext <- ""
			if (!is.na(big.num.abbr[1])) {
				big.num.abbr <- sort(big.num.abbr, decreasing = TRUE)
				for (i in 1:length(big.num.abbr)) {
					o <- unname(big.num.abbr[i])
					if (mag>(o+2) || (mag > o && all(vec - floor(vec/(10^o))*(10^o) < 1))) {
						vec <- vec / (10^o)
						ext <- paste0(" ", names(big.num.abbr)[i])
						break
					}
				}
			}
			
			# set default values
			if (!("big.mark" %in% names(args))) args$big.mark <- ","
			if (!("format" %in% names(args))) args$format <- "f"
			if (!("preserve.width" %in% names(args))) args$preserve.width <- "none"
			x <- paste(do.call("formatC", c(list(x=vec, digits=digits), args)), ext, sep="")
			x <- paste0(prefix, x, suffix)
			
			
		} else {
			if (!("format" %in% names(args))) args$format <- "g"
			x <- do.call("formatC", c(list(x=vec, digits=digits), args))
		}
		
		if (as.count) {
			x1 <- x[1:(n-1)]
			x2 <- x[(n+2):(2*n)]
			x1p1 <- x[(2*n+1):(3*n-1)]
		}
		# x <- formatC(vec, format = "f", digits = 0)
		# x1 <- x[-n]
		# x2 <- formatC(vec[-1] - 1L, format = "f", digits = 0)
		# xs <- (vec[-1] - vec[-n])
		# x1p1 <- formatC(vec[-n] + 1L, format = "f", digits = 0)
	}
	
	if (intervals) {
		if (scientific) {
			if (as.count) {
				# discrete
				lbls <- paste("{", x1, "}", sep = "")
				lbls[steps == 2] <- paste("{", x1[steps == 2], ", ", x2[steps == 2], "}", sep="")
				lbls[steps > 2] <- paste("{", x1[steps > 2], ", ", x1p1[steps > 2], ", ..., ", x2[steps > 2], "}", sep="")
			} else {
				# continuous
				if (interval.closure=="left") {
					lbls <- paste("[", x[-n], ", ", x[-1], ")", sep="")
					lbls[n-1] <- paste(substr(lbls[n-1], 1, nchar(lbls[n-1])-1), "]", sep="")
				} else {
					lbls <- paste("(", x[-n], ", ", x[-1], "]", sep="")
					lbls[1] <- paste("[", substr(lbls[1], 2, nchar(lbls[1])), sep="")
				}
			}
			
			
		} else {
			if (as.count) {
				lbls <- x1
				lbls[steps>1] <- paste(x1[steps>1], x2[steps>1], sep = paste0(" ", text.separator, " "))
				if (vec[n]==Inf) lbls[n-1] <- paste(x1[n-1], paste(text.or.more, collapse = " "), sep = " ")
			} else {
				x[vec==-Inf] <- ""
				
				lbls <- paste(x[-n], x[-1], sep = paste0(" ", text.separator, " "))
				if (vec[1]==-Inf) lbls[1] <- paste(paste(text.less.than, collapse = " "), x[2], sep = " ")
				if (vec[n]==Inf) lbls[n-1] <- paste(x[n-1], paste(text.or.more, collapse = " "), sep = " ")
			}
			
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


num2breaks <- function(x, n, style, breaks, approx=FALSE, interval.closure="left", var = NULL, as.count = FALSE, args = list()) {
	
	tmapOptions = get("tmapOptions", envir = .TMAP_CACHE)
	show.warnings <- tmapOptions$show.warnings
	
	nobs <- sum(!is.na(x))
	# create intervals and assign colors
	if (style=="fixed") {
		q <- list(var=x,
				  brks=breaks)
		if (any(na.omit(x) < min(breaks)) && show.warnings) warning("Values have found that are less than the lowest break", call. = FALSE)
		if (any(na.omit(x) > max(breaks)) && show.warnings) warning("Values have found that are higher than the highest break", call. = FALSE)
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
		
		
		if (nunique == 1 && style!="pretty" && show.warnings) {
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
		
		q <- suppressWarnings(do.call(classInt::classIntervals, c(list(x, n, style= style, intervalClosure=interval.closure), args)))
		
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
			qm1 <- suppressWarnings(do.call(classInt::classIntervals, c(list(x, n-1, style= style, intervalClosure=interval.closure), args)))
			brksm1 <- qm1$brks
			qp1 <- suppressWarnings(do.call(classInt::classIntervals, c(list(x, n+1, style= style, intervalClosure=interval.closure), args)))
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
