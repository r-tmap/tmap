process_text_just <- function(just, interactive) {
	show.messages <- get(".tmapOptions", envir = .TMAP_CACHE)$show.messages
	
	n <- length(just)
	isnum <- is_num_string(just)
	
	if (!all(isnum | (just %in% c("left", "right", "top", "bottom", "center", "centre")))) {
		warning("wrong specification of argument just", call. = FALSE)
	}
	
	just[just == "centre"] <- "center"

	if (interactive) {
		just <- just[1]
		if (n > 1 && show.messages) message("In interactive mode, the just argument should be one element")	
		
		if (isnum[1]) {
			justnum <- as.numeric(just)
			just <- ifelse(justnum < .25, "left",
					ifelse(justnum > .75, "right", "center"))
			if (show.messages) message("In interactive mode, just cannot be a numeric value. Therefore, ", justnum, " has been cenverted to \"", just, "\".")
		}
	} else {
		if (n > 2) warning("The just argument should be a single value or a vector of 2 values.", call. = FALSE)
		if (n == 1) {
			if (just %in% c("top", "bottom")) {
				just <- c("center", just)
				isnum <- c(FALSE, isnum)
			} else {
				just <- c(just, "center")
				isnum <- c(isnum, FALSE)
			}
		}
		
		x <- ifelse(isnum[1], as.numeric(just[1]),
			 ifelse(just[1] == "left", 0,
			 ifelse(just[1] == "right", 1,
			 ifelse(just[1] == "center", .5, NA))))
		if (is.na(x)) {
			warning("wrong specification of argument just", call. = FALSE)
			x <- .5
		}
		
		y <- ifelse(isnum[2], as.numeric(just[2]),
			 ifelse(just[2] == "bottom", 0,
			 ifelse(just[2] == "top", 1,
			 ifelse(just[2] == "center", .5, NA))))
		if (is.na(y)) {
			warning("wrong specification of argument just", call. = FALSE)
			y <- .5
		}
		just <- c(x, y)
	}
	just
}
