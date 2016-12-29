is_num_string <- function(x) {
	suppressWarnings(!is.na(as.numeric(x)))
}
