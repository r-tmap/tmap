#' Get a tip about tmap
#'
#' Generates a tip with an example. The tip and example code are printed, and the example itself is executed.
#' 
#' @param latest.version should only tips be generated from the latest version of tmap? False by default.
#' @export
#' @examples tmap_tip() 
tmap_tip <- function(latest.version = FALSE) {
	tip <- get_tip(latest.version)

	def_opts <- .defaultTmapOptions
	attr(def_opts, "style") <- NULL # to prevent overwriting the style "white"
	def_opts$show.messages <- FALSE
	opts <- tmap_options(def_opts)
	mode <- tmap_mode(tip$mode)
	
	cat(tip$text, "\n\n")
	cat(paste(tip$code, collapse="\n"))
	
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
	vrs <- as.numeric_version(sapply(cks, "[[", 1))
	vlev <- as.character(sort(unique(vrs)))
	weights <- seq(1, new_tips_weight, length.out = length(vlev))
	vint <- as.integer(factor(as.character(vrs), levels = vlev))
	w <- weights[vint]
	s <- sample(1L:n, prob = w)
	structure(s, new = (vint == max(vint))[s])
}

get_new_tip_id <- function(ids, id) {
	ids_new <- which(attr(ids, "new"))
	if (id %in% ids_new) {
		id
	} else if (all(ids_new < id)) {
		ids_new[1]
	} else {
		ids_new[which((ids_new-id) > 0)[1]]
	}
}

get_tip <- function(latest.version) {
	cks <- read_tmap_tips()
	ids <- get("tmapTipsIds", envir = .TMAP_CACHE)
	id <- get("tmapTipsId", envir = .TMAP_CACHE)
	
	if (latest.version) id <- get_new_tip_id(ids, id)
	
	assign("tmapTipsId", ifelse(id == length(ids), 1, id + 1), envir = .TMAP_CACHE)
	cks[[ids[id]]]
}
