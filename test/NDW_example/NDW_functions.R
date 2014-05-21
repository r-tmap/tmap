simplifyRoad <- function(shp, id) {
	require(vegan)
	require(igraph)
	require(fields)
	
	cat <- shp@data[,id]
	
	if (!is.factor(cat)) cat <- as.factor(cat)
	
	cat.levels <- names(which(table(cat)>0))
	
	lines <- lapply(cat.levels, function(level) {
		cat("proces", level, "\n")
		shp2 <- shp[cat==level, ]
		
		coor <- lapply(shp2@lines, function(x) {
			coor <- lapply(x@Lines, function(y)y@coords)
			do.call("rbind", coor)
		})
		coor <- as.data.frame(do.call("rbind", coor))
		
		n <- nrow(coor)
		d <- rdist(coor)
		
		d <- fields.rdist.near(x1=coor, delta=10, max.points=1e8)
		sel <- d$ind[,1] > d$ind[,2]
		ind <- d$ind[sel, ]
		ra <- d$ra[sel]
		
		v.red <- ind[, 1]
		
		coor2 <- coor[-v.red, ]
		n2 <- nrow(coor2)
		
		## close connected points
		d2 <- fields.rdist.near(x1=coor2, delta=250, max.points=1e8)
		sel2 <- d2$ind[,1] > d2$ind[,2]
		edgelistA <- d2$ind[sel2, ]
		distancesA <- d2$ra[sel2]
		
		## mst
		d3 <- dist(coor2)
		mst <- spantree(d3)
		edgelistB <- cbind(from=2:nrow(coor2), to=mst$kid)
		distancesB <- mst$dist
		for (i in 1:nrow(edgelistB)) {
			if (edgelistB[i, 1] < edgelistB[i, 2]) edgelistB[i, 1:2] <- edgelistB[i, 2:1]
		}
		
		edgelist <- rbind(edgelistA, edgelistB)
		distances <- c(distancesA, distancesB)
		
		dupl <- duplicated(edgelist, MARGIN=1)
		edgelist <- edgelist[!dupl, ]
		distances <- distances[!dupl]
		
		g <- graph.edgelist(edgelist, directed=FALSE)
		E(g)$weight <- distances
		
		is.connected(g)
		no.clusters(g, mode=c("weak"))
		
		xtable <- shortest.paths(g)
		
		cat("total length:", max(xtable), "\n")
		maxid <- which.max(xtable)
		maxcol <- floor((maxid-1) / n2) + 1
		maxrow <- maxid - (maxcol-1) * n2
		
		
		
		sp <- get.shortest.paths(g, maxrow, maxcol)
		
		coor3 <- coor2[sp$vpath[[1]],]
		
		line <- Line(coor3)
		Lines(list(line), ID=level)
	})
	shp3 <- SpatialLines(lines, proj4string=shp@proj4string)
	data <- data.frame(ID=cat.levels, row.names=cat.levels)
	shp3 <- SpatialLinesDataFrame(shp3, data=data, match.ID=FALSE)
	shp3
}
