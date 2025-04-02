fancy_breaks <- function(vec, as.count = FALSE, intervals=FALSE, interval.closure="left", fun=NULL, scientific=FALSE, big.num.abbr = c("mln" = 6, "bln" = 9), prefix = "", suffix = "", text.separator="to", text.less.than=c("less", "than"), text.or.more=c("or", "more"), text.align="left", text.to.columns=FALSE, digits=NA, html.escape = TRUE, ...) {
	args <- list(...)
	n <- length(vec)

	if (inherits(vec, c("POSIXct", "POSIXlt", "Date"))) {
		x = format(vec)
	} else if (!is.null(fun)) {
		if (as.count) {
			steps <- (vec[-1] - vec[-n])
			vec <- c(vec, vec - 1L, vec + 1L) # needed for: {1, 2, ... 9}
			digits <- 0
		}
		x <- do.call(fun, list(vec))
		if (as.count) {
			x1 <- x[1:(n-1)]
			x2 <- x[(n+2):(2*n)]
			x1p1 <- x[(2*n+1):(3*n-1)]
		}
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

	tmapOptions = tmap_options_mode(mode.specific = FALSE)
	show.warnings <- tmapOptions$show.warnings

	nobs <- sum(!is.na(x))
	# create intervals and assign colors
	if (style=="fixed") {
		x2 = na.omit(x)
		tooLow = (x2 < min(breaks))
		tooHigh = (x2 > max(breaks))
		if (any(tooLow) && show.warnings) warning("Values have found that are less than the lowest break. They are assigned to the lowest interval", call. = FALSE)
		if (any(tooHigh) && show.warnings) warning("Values have found that are higher than the highest break. They are assigned to the highest interval", call. = FALSE)

		q <- list(var=x,
				  brks=breaks)
		attr(q, "style") <- "fixed"
		attr(q, "nobs") <- nobs
		attr(q, "intervalClosure") <- interval.closure
		class(q) <- "classIntervals"
	} else {
		if (nobs == 0) {
			if (!is.null(var)) {
				cli::cli_abort("Numerical variable {.var {var}} only contains missing values.")
			} else {
				cli::cli_abort("Numerical variable only contains missing values.")
			}
		}

		nunique <- length(na.omit(unique(x)))


		if (nunique == 1 && style != "pretty" && show.warnings) {
			if (is.null(var)) {
				bullet <- "Single value found."
			} else {
				bullet <- "Single unique value found for variable {.var {var}}"
			}
			cli::cli_warn(c(
				"!" = bullet,
				"i" = "Setting {.code style = {.str pretty}}"
			))
		}

		tempx <- nunique <= n

		if (tempx) {
			x_orig <- x
			if (length(na.omit(unique(x))) == 1) x <- pretty(x)
			x <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1)
		}

		q <- tryCatch({
			suppressWarnings(do.call(classInt::classIntervals, c(list(x, n, style = style, intervalClosure = interval.closure), args)))
		}, error = function(e) {
			cli::cli_abort(c(
				"Calculating interval classes failed for the variable {.var {var}} with {.code style = {.val {style}}}."
			    ),
			    parent = e
			)
		})



		if (tempx) q$var <- x_orig

	}

	if (approx && style != "fixed") {
		if (n >= length(unique(x)) && style == "equal") {
			# to prevent classIntervals to set style to "unique"
			q <- list(var = x, brks = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n))
			attr(q, "intervalClosure") <- interval.closure
			class(q) <- "classIntervals"
		} else {
			brks <- q$brks

			# to prevent ugly rounded breaks such as -.5, .5, ..., 100.5 for n=101
			qm1 <- suppressWarnings(do.call(classInt::classIntervals, c(list(x, n - 1, style = style, intervalClosure = interval.closure), args)))
			brksm1 <- qm1$brks
			qp1 <- suppressWarnings(do.call(classInt::classIntervals, c(list(x, n + 1, style = style, intervalClosure = interval.closure), args)))
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


are_breaks_diverging = function(brks) {
	# if !divx then c-Inf, 2, 5, 10) is considered sequential
	negb <- any(brks[brks != -Inf] < 0) || (brks[1] == -Inf && brks[2]<=0)
	nb <- length(brks)
	posb <- any(brks[brks != Inf] > 0) || (brks[nb] == Inf && brks[nb-1]>=0)
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
use_div = function(brks, midpoint = NULL) {
	if (!is.null(midpoint) && !is.na(midpoint)) return(TRUE)

	if (is.null(brks)) {
		return(NA)
	} else {
		are_breaks_diverging(brks)
	}
}



# https://stackoverflow.com/questions/12688717/round-up-from-5
#rnd <- function(x) trunc(x + sign(x) * 0.5)
round2 = function(x, n = 0) {
	posneg = sign(x)
	z = abs(x)*10^n
	z = z + 0.5 + sqrt(.Machine$double.eps)
	z = trunc(z)
	z = z/10^n
	z*posneg
}
# a = seq(0.5, 10.5, by = 0.5)
# names(a) = a
# round(a)
# round2(a, 0)
# rnd(a)


# round x to nearest multiple of y
round_num = function(x, y) round2(x / y) * y


# x = c(12.777156207494, 20.7613848999599, 28.7456135924257, 36.7298422848916, 44.7140709773574)
prettyTicks = function(x, dev = 0.1) {
	is_equi = local({
		dff = max(x) - min(x)
		if (dff == 0) return(TRUE)
		steps = (x[-1] - head(x,-1)) / dff
		all(abs(steps-steps[1]) < 1e-3)
	})

	if (is_equi) {
		pretty(x, n = length(x))
	} else {
		s = x[-1] - head(x,-1)
		s = c(s[1], s, tail(s,1))
		s = pmin(head(s,-1), tail(s,-1))

		mapply(function(xi, si) {
			for (r in rev(.TMAP$round_to)) {
				xir = round_num(xi, r)
				if (abs(xi - xir) < (si * dev)) break
			}
			xir
		}, x, s, SIMPLIFY = TRUE)	}
}
# x = c(3.654, 4.65, 5.1, 7.5)
# x = sort(c(0.004324, 0.00324, 0.00227, 0.00745435))
# x = c(3, 4,6,7,9)
# x = c(3.1,3.354,3.6)
# x = c(3.1,4.354,5.6)
# x = c(1043,2045,4005, 6765)
# x = c(265, 280, 520, 1000)
# x = c(265, 520, 1000, 1280)
# prettyTicks(x)
