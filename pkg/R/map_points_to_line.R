map_points_to_line <- function(shp.points, shp.lines, key.points=NULL, key.lines=NULL, by.key=FALSE) {
	
	if (by.key) {
		if (!is.factor(shp.points[[key.points]])) shp.points[[key.points]] <- factor(shp.points[[key.points]])
		
		shp.lines[[key.lines]] <- factor(as.character(shp.lines[[key.lines]]), levels=levels(shp.points[[key.points]]))
	}
	
	co <- coordinates(shp.lines)
	cp <- coordinates(shp.points)
	
	res3 <- lapply(1:length(shp.points), function(i) {
		
		coi <- cp[i,]
		
		if (by.key) {
			lev <- shp.points[[key.points]][i]
			sel <- which(shp.lines[[key.lines]]==lev)
		} else sel <- 1:length(shp.lines)
		col <- co[sel]
		
		res2 <- lapply(col, function(x) {
			res <- lapply(x, function(y) {
				y <- as.data.frame(cbind(y, x1=coi[1], x2=coi[2]))
				y$d <- sqrt((y[,3]-y[,1])^2 + (y[,4]-y[,2])^2)
				wm <- which.min(y$d)[1]
				data.frame(id3=wm, d=y$d[wm])
			})
			res <- do.call("rbind", res)
			res$id2 <- 1:length(x)
			res[which.min(res$d)[1], ]
		})
		pdat <- do.call("rbind", res2)
		
		pdat$id1 <- sel
		pdat[which.min(pdat$d)[1], c("id1", "id2", "id3", "d")]
	})
	res3 <- do.call("rbind", res3)
	row.names(res3) <- NULL
	res3
}
