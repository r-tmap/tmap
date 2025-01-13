#' Print a random tip to the console
#'
#' @returns A message
#' @export
tmap_tip <- function() {
	tips <- c(
		"tmap v4 works with {.href [visual variables](https://r-tmap.github.io/tmap/articles/basics_vv)}.",
		"tmap v4 can be {.href [extended](https://r-tmap.github.io/tmap/articles/adv_extensions)} in several ways."
	)
	tip <- sample(tips, size = 1)
	cli::cli_inform(tip, .frequency_id = "tmap_tip", .frequency = "always")
}
