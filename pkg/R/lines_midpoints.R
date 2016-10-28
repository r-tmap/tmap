lines_midpoints <- function (sldf) 
{
	#stopifnot(is.projected(sldf))
	Lns <- slot(sldf, "lines")
	hash_lns <- sapply(Lns, function(x) length(slot(x, "Lines")))
	N <- sum(hash_lns)
	midpoints <- matrix(NA, ncol = 2, nrow = N)
	Ind <- integer(length = N)
	ii <- 1
	for (i in 1:length(Lns)) {
		Lnsi <- slot(Lns[[i]], "Lines")
		for (j in 1:hash_lns[i]) {
			Ind[ii] <- i
			midpoints[ii, ] <- get_midpoint(slot(Lnsi[[j]], "coords"))
			ii <- ii + 1
		}
	}
	if (is(sldf, "SpatialLinesDataFrame")) {
		df0 <- slot(sldf, "data")[Ind, ]
		df <- as.data.frame(cbind(df0, Ind))
	}
	else df <- data.frame(Ind = Ind)
	spdf <- SpatialPointsDataFrame(midpoints, data = df, proj4string = get_projection(sldf, as.CRS = TRUE))
	return(spdf)
}

get_midpoint <- function (coords) {
	dist <- sqrt((diff(coords[, 1])^2 + (diff(coords[, 2]))^2))
	dist_mid <- sum(dist)/2
	dist_cum <- c(0, cumsum(dist))
	end_index <- which(dist_cum > dist_mid)[1]
	start_index <- end_index - 1
	start <- coords[start_index, ]
	end <- coords[end_index, ]
	dist_remaining <- dist_mid - dist_cum[start_index]
	start + (end - start) * (dist_remaining/dist[start_index])
}