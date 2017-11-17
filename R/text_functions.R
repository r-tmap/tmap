get_text_i <- function(txt, i) {
	if (is.character(txt[[1]])) {
		sapply(txt, "[[", i)
	} else if (is.expression(txt[[1]])) {
		sapply(txt, "[", i)
	}
}

number_text_lines <- function(txt) {
	if (is.character(txt)) {
		length(strsplit(txt, "\n")[[1]])
	} else 1
}

nonna_text <- function(txt) {
	if (is.expression(txt)) {
		txt
	} else if (all(is.na(txt))) {
		""
	} else txt
}

is.ena <- function(x) {
	if (is.expression(x)) {
		rep(FALSE, length(x))
	} else is.na(x)
}

replace_na_text <- function(txt, repl) {
	if (is.expression(txt)) {
		txt
	} else if(is.na(txt[1])) {
		repl
	} else txt
}

nonempty_text <- function(txt) {
	if (is.character(txt)) {
		txt!=""
	} else rep(TRUE, length(txt))
}

expr_to_char <- function(txt) {
	if (is.character(txt)) {
		txt
	} else {
		as.character(txt)
	}
}

text_height_npc <- function(txt, to_width = FALSE) {
	if (to_width) {
		convertWidth(convertHeight(stringHeight(txt), "inch"), "npc", TRUE)
	} else {
		convertHeight(stringHeight(txt), "npc", TRUE)
	}
}

text_width_npc <- function(txt, space=TRUE, to_height = FALSE) {
	brks <- attr(txt, "brks")
	
	if (is.null(brks)) {
		if (space) txt <- paste(txt, " ", sep = "")
		
		if (to_height) {
			convertHeight(convertWidth(stringWidth(txt), "inch"), "npc", TRUE)
		} else {
			convertWidth(stringWidth(txt), "npc", TRUE)
		}
		
	} else {
		txt_splits <- split_legend_labels(txt, brks)
		spx <- if (space) convertWidth(stringWidth(" "), "npc", TRUE) * 1.5 else 0
		res <- lapply(txt_splits, function(tx) convertWidth(stringWidth(tx), "npc", TRUE) + spx)

		max1 <- max(sapply(res, "[", 1))
		max2 <- max(sapply(res, "[", 2))
		max3 <- max(sapply(res, "[", 3))
		#r3 <- sapply(res, "[", 3)
		widths <- max1 + max2 + max3 #r3
		
		attr(widths, "cw") <-  do.call(rbind, res)
		widths
	}
}

split_legend_labels <- function(txt, brks) {
	lapply(1L:length(txt), function(i) {
		c(substr(txt[i], 1, brks[i,1]-2),
		  substr(txt[i], brks[i,1], brks[i,2]-2),
		  substr(txt[i], brks[i,2], nchar(txt[i])))
	})
}


# same as lapply, but expression are subsetted by [ rather than [[
elapply <- function(X, FUN, ...) {
	ise <- is.expression(X)
	do.call(lapply, c(list(X=X, FUN=function(x, ...) {
		x <- if(ise) as.expression(x) else x
		do.call(FUN, c(list(x), list(...)))	
	}), list(...)))
}
# e2 <- expression(paste("Gsd ", sqrt(3)), paste("Gdf ", pi))
# lapply(letters[1:4], function(x)x)
# lapply(e2, function(x)x)
# 
# elapply(letters[1:4], function(x)x)
# elapply(e2, function(x)x)
# 
# lapply(list(c(1,6,3, NA, 3), c(5,43)), mean)
# lapply(list(c(1,6,3, NA, 3), c(5,43)), mean, na.rm=TRUE)
# 
# elapply(list(c(1,6,3, NA, 3), c(5,43)), mean)
# elapply(list(c(1,6,3, NA, 3), c(5,43)), mean, na.rm=TRUE)



# same as mapply, but expression are subsetted by [ rather than [[
emapply <- function(FUN, ..., MoreArgs=NULL, SIMPLIFY=TRUE, USE.NAMES=TRUE) {
	args <- list(...)
	ise <- vapply(args, is.expression, FUN.VALUE = logical(1))
	m <- length(args)
	do.call(mapply, c(list(FUN=function(...) {
		args <- list(...)[1:m]
		args <- mapply(function(a, i) {
			if (i) as.expression(a) else a 
		}, args, ise, SIMPLIFY=FALSE)
		do.call(FUN, c(args, MoreArgs))	
	}), args, list(MoreArgs=MoreArgs, SIMPLIFY=SIMPLIFY, USE.NAMES=USE.NAMES)))
}
# e2 <- expression(paste("Gsd ", sqrt(3)), paste("Gdf ", pi))
# e3 <- expression(paste("Fhdgas ", 3+10), paste("Gags ", cos(pi)))
# mapply(pmax, 1:5, 5:1)
# emapply(pmax, 1:5, 5:1)
# 
# mapply(paste, letters[1:5], letters[6:10])
# mapply(paste, letters[1:5], letters[6:10], MoreArgs = list(sep="_^_"))
# 
# emapply(paste, letters[1:5], letters[6:10])
# emapply(paste, letters[1:5], letters[6:10], MoreArgs = list(sep="_^_"))
# 
# mapply(paste, e2, e3, MoreArgs = list(sep="_^_"), SIMPLIFY = FALSE)
# 
# emapply(paste, e2, e3, MoreArgs = list(sep="_^_"), SIMPLIFY = FALSE)
