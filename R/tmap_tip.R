#' Get a tip about tmap
#'
#' Generates a tip with an example. The tip and example code are printed, and the example itself is executed.
#' 
#' @param from.version version number. Only tips regarding features from this version are shown.
#' @export
#' @example ./examples/tmap_tip.R
tmap_tip <- function(from.version = NULL) {
	tip <- get_tip(from.version)

	if (is.null(tip)) return(invisible(NULL))
	
	def_opts <- .defaultTmapOptions
	attr(def_opts, "style") <- NULL # to prevent overwriting the style "white"
	def_opts$show.messages <- FALSE
	opts <- tmap_options(def_opts)
	mode <- tmap_mode(tip$mode)
	
	cat(tip$text, "\n")
	cat("New since tmap", as.character(tip$version), "\n\n")
	
	
	cat(paste(tip$code, collapse="\n"), "\n")
	
	print(eval(parse(text = tip$code)))
	cat("\n")
	
	# restore options and mode
	tmap_mode(mode)
	tmap_options(opts)
	invisible(NULL)
}



read_tmap_tips <- function() {
	f <- readLines(system.file("tips.txt", package="tmap"))
	fs <- split(f, cumsum(f==""))
	cks <- lapply(fs, function(ck) {
		list(version = as.numeric_version(ck[2]),
			 text = ck[3],
			 mode = ck[4],
			 code = ck[5:length(ck)])
	})
}


determine_tips_order <- function(new_tips_weight = 3) {
	## randomize order with higher weights for newer versions
	cks <- read_tmap_tips()
	n <- length(cks)
	vrs <- unname(as.numeric_version(sapply(cks, "[[", 1)))
	#vlev <- as.character(sort(unique(vrs)))
	#weights <- seq(1, new_tips_weight, length.out = length(vlev))
	#vint <- as.integer(factor(as.character(vrs), levels = vlev))
	#w <- weights[vint]
	s <- sample(1L:n) #, prob = w
	#structure(s, new = (vint == max(vint))[s])
	structure(s, version = vrs[s])
}



get_tip <- function(from.version = NULL) {
	cks = read_tmap_tips()
	ids = get("tmapTipsIds", envir = .TMAP_CACHE)
	id = get("tmapTipsId", envir = .TMAP_CACHE)
	
	v = attr(ids, "version")
	
	vth = if (!is.null(from.version)) {
		numeric_version(from.version)
	} else {
		min(v)
	}
	sel = which(v >= vth)
	if (!length(sel)) {
		message("No tips found from version ", as.character(vth))
		return(NULL)
	} 
	if (id >= tail(sel, 1)) {
		id = sel[1]
	} else {
		id = sel[which(sel - id > 0)[1]]
	}

	assign("tmapTipsId", ifelse(id == length(ids), 1, id + 1), envir = .TMAP_CACHE)
	cks[[ids[id]]]
}
