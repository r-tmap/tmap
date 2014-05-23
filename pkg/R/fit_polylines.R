#' Fit polylines through a set of spatial points
#' 
#' This function fits one or more smooth polylines through a set of spatial points.
#' 
#' @param shp Shape object that contains the spatial points. Preferably a \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}} or \code{\link[sp:SpatialPoints]{SpatialPoints}} object. If spatial lines are given, the coordinates are considered as points, and if spatial polygons are given, the centroids are considerend as points.
#' @param id Name of the data variable that determines the classes of the points. For each class a polyline is fit. Is omitted, a polyline is fit through all points.
#' @param min.dist Minimum distance. Poins that are closer than \code{min.dist} from any other point are omitted in the fitting method (see details below)
#' @param max.opt.dist Maximal optimized distance. Between any two points that lie closer than \code{max.opt.dist} to each other, an edge is created in the fitting method (see details below)
#' @param sep.dist Seperation distance. If the distance between two groups of points is larger than \code{sep.dist}, two seperate polylines are created.
#' @param verbose Print logging text
#' @param na.rm If \code{id} is specified, should NA values be removed? If \code{FALSE}, they are considered as a seperate category
#' @return SpatialLinesDataframe
#' @import fields igraph vegan
#' @export
fit_polylines <- function(shp, id=NULL, min.dist=10, max.opt.dist=250, sep.dist=5000, verbose=TRUE, na.rm=TRUE) {

	n <- length(shp)
	addNA <- FALSE
	if (!is.null(id)) {
		cat <- shp@data[,id]
		if (!is.factor(cat)) cat <- as.factor(cat)
		cat.levels <- names(which(table(cat)>0))
		if (any(is.na(cat))) {
			if (na.rm) {
				shp <- shp[!is.na(cat), ]
				n <- length(shp)
			} else {
				levels(cat) <- c(levels(cat), "NA")
				cat.levels <- c(cat.levels, "NA")
				cat[is.na(cat)] <- "NA"
				addNA <- TRUE
			}
		}
	} else {
		cat <- factor(rep(1, n))
		cat.levels <- levels(cat)
	}
	
	if (inherits(shp, "SpatialLines")) {
		coorlist <- lapply(shp@lines, function(x) {
			co <- lapply(x@Lines, function(y)y@coords)
			co2 <- do.call("rbind", co)
		})
		idnr <- unlist(mapply(FUN=rep, 1:n, sapply(coorlist, nrow)))
		coor <- as.data.frame(do.call("rbind", coorlist))
		
	} else {
		coor <- as.data.frame(coordinates(shp))
		idnr <- 1:n
	}
	
	grp <- cat[idnr]

	lines <- lapply(cat.levels, function(level) {
		if (verbose) cat("PROCESS", level, "\n")

		cs <- coor[grp==level,]
				   
		n <- nrow(cs)
		
		if (verbose) cat("number of coordinates:", n, "\n")
		
		## omit closely neighboring points
		d <- fields.rdist.near(x1=cs, delta=min.dist, max.points=1e8)
		sel <- d$ind[,1] > d$ind[,2]
		ind <- d$ind[sel, ]
		ra <- d$ra[sel]
		
		v.red <- ind[, 1]
		
		cs2 <- cs[-v.red, ]
		n2 <- nrow(cs2)

		if (verbose) cat(n-n2, "coordinates omitted\n")
		
		
		## use mst to find clusters
		d2 <- dist(cs2)
		mst <- spantree(d2)

		edgelist <- cbind(from=2:n2, to=mst$kid)
		toofar <- mst$dist > sep.dist
		edgelist2 <- edgelist[!toofar,]
		distances <- mst$dist[!toofar]
		
		g <- graph.edgelist(edgelist2, directed=FALSE)
		
		memid <- clusters(g)$membership
		
		csg <- split(cs2, memid)
		if (verbose) cat("Number of polylines:", length(csg), "\n")
		
		linesc <- lapply(csg, function(csc) {
			nc <- nrow(csc)
			## full network of short edges
			d2 <- fields.rdist.near(x1=csc, delta=max.opt.dist, max.points=1e8)
			sel2 <- d2$ind[,1] > d2$ind[,2]
			edgelistA <- d2$ind[sel2, ]
			distancesA <- d2$ra[sel2]
			
			## full mst
			d3 <- dist(csc)
			mst <- spantree(d3)
			
			edgelistB <- cbind(from=2:nc, to=mst$kid)
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

			xtable <- shortest.paths(g)
			maxid <- which.max(xtable)
			maxcol <- floor((maxid-1) / nc) + 1
			maxrow <- maxid - (maxcol-1) * nc
			sp <- get.shortest.paths(g, maxrow, maxcol)
		
			csc2 <- csc[sp$vpath[[1]],]
			
			Line(csc2)
		})
		lng <- sapply(linesc, LineLength)
		if (verbose) {
			if (length(csg)>1) {
				cat("Line lengths:", lng, "\n")
			}
			cat("Total line length:", sum(lng), "\n")
		} 
		Lines(linesc, ID=level)
	})
	shp3 <- SpatialLines(lines, proj4string=shp@proj4string)
	cat.levels2 <- if (addNA) setdiff(cat.levels, "NA") else cat.levels
	data <- data.frame(ID=cat.levels2, row.names=cat.levels)
	shp3 <- SpatialLinesDataFrame(shp3, data=data, match.ID=FALSE)
	shp3
}
