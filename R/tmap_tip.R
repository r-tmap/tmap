#' Print a random tip to the console
#'
#' @returns A message
#' @export
tmap_tip <- function() {
	nr = .TMAP$tip_nr
	tip = .TMAP$tips[nr]

	nr2 = if (nr == length(.TMAP$tips)) 1L else nr + 1L

	.TMAP$tip_nr = nr2

	cli::cli_inform(tip, .frequency_id = "tmap_tip", .frequency = "always")
}
