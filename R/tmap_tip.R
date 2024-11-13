#' Print a random tip to the console
#'
#' @returns A message
#' @export
tmap_tip <- function() {
	tips <- c(
		"tmap v4 works with {.href [visual variables](https://r-tmap.github.io/tmap/articles/03_tmap_vv.html)}.",
		""
	)
	tip <- sample(tips, size = 1)
	cli::cli_inform(tip)
}
