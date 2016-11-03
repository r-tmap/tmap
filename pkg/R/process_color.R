process_color <- function(col, alpha=NA, sepia.intensity=0, saturation=1) {
	#if (length(col)>100) browser()
	
	isFactor <- is.factor(col)
	
	if (isFactor) {
		x <- as.integer(col)
		col <- levels(col)
	}
	
	res <- t(col2rgb(col, alpha=TRUE))
	
	# set alpha values
	if (!is.na(alpha)) res[res[,4] != 0, 4] <- alpha * 255

	# convert to sepia
	if (sepia.intensity!=0) {
		conv_matrix <- matrix(c(.393, .769, .189,
								.349, .686, .168,
								.272, .534, .131), ncol=3, byrow=FALSE)
		res[,1:3] <-  (res[,1:3] %*% conv_matrix) * sepia.intensity + res[,1:3] * (1-sepia.intensity)
		res[res>255] <- 255
		res[res<0] <- 0
	}	
	
	# convert to black&white
	if (saturation!=1) {
		res[,1:3] <- (res[,1:3] %*% matrix(c(.299, .587, .114), nrow=3, ncol=3))  * (1-saturation) + res[,1:3] * saturation
		res[res>255] <- 255
		res[res<0] <- 0
	}
	if (all(res[,4]==255)) res <- res[,-4, drop=FALSE]

	new_cols <- do.call("rgb", c(unname(as.data.frame(res)), list(maxColorValue=255)))
	
	if (isFactor) {
		new_cols[x]
	} else {
		new_cols
	}
}

is_light <- function(col) {
	colrgb <- col2rgb(col)
	apply(colrgb * c(.299, .587, .114), MARGIN=2, sum) >= 128
}

get_light <- function(col) {
	colrgb <- col2rgb(col)
	apply(colrgb * c(.299, .587, .114), MARGIN=2, sum) / 255
}


darker <- function(colour, rate, alpha=NA) {
	col <- col2rgb(colour, TRUE)/255
	col[1:3] <- col[1:3] * (1-rate)
	if (is.na(alpha)) alpha <- col[4,] 
	rgb(col[1, ], col[2, ], col[3, ], alpha)
}

lighter <- function(colour, rate, alpha=NA) {
	col <- col2rgb(colour, TRUE)/255
	col[1:3] <- col[1:3] + (1-col[1:3]) * rate
	if (is.na(alpha)) alpha <- col[4,] 
	rgb(col[1, ], col[2, ], col[3, ], alpha)
}

col2hex <- function(x) {
	y <- apply(col2rgb(x), MARGIN=2, FUN=function(y)do.call(rgb, c(as.list(y), list(maxColorValue=255))))
	y[is.na(x)] <- NA
	y
}


# determine palette type
# palettes of length 1,2 or 3 are cat
# palette of length 4+ are seq if luminance is increasing or decreaing
# palette of length 5+ are div if luminance is increasing in first half and decreasing in second half, or the other way round.
palette_type <- function(palette) {
	k <- length(palette)
	if (k<4) return("cat")
	
	m1 <- ceiling(k/2) - 1
	m2 <- floor(k/2) + 1
	
	colpal_light <- get_light(palette)
	
	s <- sign(colpal_light[-1] - colpal_light[-k])
	
	if (all(s==1) || all(s==-1)) {
		return("seq")
	} else if (k>4 && ((all(s[1:m1]==1) && all(s[m2:(k-1)]==-1)) ||
		(all(s[1:m1]==-1) && all(s[m2:(k-1)]==1)))) {
		return("div")
	} else {
		return("cat")
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


get_brewer_pal <- function(palette, n, contrast, stretch=TRUE) {
	nmax <- brewer.pal.info[palette, "maxcolors"]
	if (brewer.pal.info[palette, "category"]=="qual") {
		brewerpal <- brewer.pal(min(nmax, max(n, 3)), name=palette)
		if (stretch && n > length(brewerpal)) {
			p <- colorRampPalette(brewerpal)(n)
		} else {
			p <- rep(brewerpal, length.out=n)
		}
	} else if (brewer.pal.info[palette, "category"]=="seq") {
		if (is.na(contrast[1])) contrast <- default_contrast_seq(n)
		if (length(contrast)==1) contrast <- c(0, contrast)
		brewerpal <- brewer.pal(nmax, name=palette)
		contrastIDs <- round(seq(contrast[1]*100, contrast[2]*100, length.out=n))+1
		p <- colorRampPalette(brewerpal)(101)[contrastIDs]
	} else {
		if (is.na(contrast[1])) contrast <- default_contrast_div(n)
		if (length(contrast)==1) contrast <- c(0, contrast)
		brewerpal <- brewer.pal(nmax, name=palette)
		contrastIDs <- map2divscaleID(breaks=seq(-10,10, length.out=n+1), contrast=contrast)
		p <- colorRampPalette(brewerpal)(101)[contrastIDs]
	}
	p
}


valid_colors <- function(x) {
	is.na(x) | (x %in% colors()) |	(sapply(gregexpr("^#(([[:xdigit:]]){6}|([[:xdigit:]]){8})$", x), "[[", 1) == 1L)
}



# get_alpha_col <- function(colour, alpha=NA) {
# 	col <- col2rgb(colour, TRUE)/255
# 	if (is.na(alpha)) alpha <- col[4,] 
# 	new_col <- rgb(col[1, ], col[2, ], col[3, ], alpha)
# 	new_col
# }
# 
# get_sepia_col <- function(col, intensity=1) {
# 	conv_matrix <- matrix(c(.393, .769, .189,
# 							.349, .686, .168,
# 							.272, .534, .131), ncol=3, byrow=FALSE)
# 	colM <- t(col2rgb(col))
# 	res <-  (colM %*% conv_matrix) * intensity + colM * (1-intensity)
# 	res[res>255] <- 255
# 	do.call("rgb", c(unname(as.data.frame(res)), list(maxColorValue=255)))
# }
# 