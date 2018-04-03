process_tiles <- function(g) {
	provider <- if (is.na(g$provider)) "OpenStreetMap" else g$provider
	list(tiles.provider = provider, tiles.alpha = g$alpha)
}
