#' Print a rnadom tip to the console
#' 
#' @returns A message
#' @export
tmap_tip <- function() {
	tips <- c(
		"tmap v4 works with visual variables."
	)
	tip <- sample(tips, size = 1)
	cli::cli_inform(tip)
}
