#' Fit polylines through a set of spatial points
#' 
#' This function fits one or more smooth polylines through a set of spatial points. (Experimental)
#' 
#' @param ... Shape objects that contains the spatial points.
#' @param id Name of the data variable that determines the classes of the points. For each class a polyline is fit. Is omitted, a polyline is fit through all points.
#' @param min.dist Minimum distance. Poins that are closer than \code{min.dist} from any other point are omitted in the fitting method (see details below)
#' @param max.opt.dist Maximal optimized distance. Between any two points that lie closer than 
#' \code{max.opt.dist} to each other, an edge is created in the fitting method (see details below)
#' @param sep.dist Seperation distance. If the distance between two groups of points is larger than \code{sep.dist}, two seperate polylines are created.
#' @param verbose Print logging text
#' @return SpatialLinesDataframe
#' @import fields igraph vegan
#' @export
fit_polylines <- function(..., id=NULL, min.dist=10, max.opt.dist=250, sep.dist=5000, verbose=TRUE) {
	shps <- list(...)
	
	if (is.null(id)) {
		lvls <- ""
		shps <- lapply(shps, function(shp) {
			shp$IDs <- 1
			shp
		})
	} else {
		lvls <- levels(shps[[1]][[id]])
		for (i in 1:length(shps)) {
			if (!inherits(shps[[i]], "Spatial")) stop(paste("object", i, "is not Spatial"))
			if (!identical(levels(shps[[i]][[id]]), lvls)) stop("Levels id not consistent across shapes")
		}
		
		shps <- lapply(shps, function(shp) {
			shp <- shp[!is.na(shp[[id]]), ]
			shp$IDs <- as.integer(shp[[id]])
			shp
		})
	}
	
	co_s <- lapply(shps, FUN=get_coordinates, id="IDs")
	co_s <- lapply(co_s, function(x){
		names(x) <- c("V1", "V2", "ID")
		x
	})
	co <- do.call(rbind, co_s)
	
	IDs <- unique(co$ID)
	
	lines <- lapply(IDs, function(i) {
		if (verbose) cat("PROCESS", lvls[i], "\n")

		cs <- co[co$ID==i,]
				   
		n <- nrow(cs)
		
		if (verbose) cat("number of coordinates:", n, "\n")

		## omit closely neighboring points
		d <- fields.rdist.near(x1=cs, delta=min.dist, max.points=1e8)
		sel <- d$ind[,1] > d$ind[,2]
		ind <- d$ind[sel, , drop=FALSE]
		ra <- d$ra[sel]
		
		if (length(ind)) {
			ind[,] <- as.character(ind[,])
			
			g_close <- graph.edgelist(ind, directed=FALSE)
			cl <- clusters(g_close, mode="weak")
			
			Vids <- as.integer(V(g_close)$name)
			close <- data.frame(ID=V(g_close)$name, cl=cl$membership, 
								x=cs[Vids, 1],
								y=cs[Vids, 2])
			
			newx <- as.vector(tapply(close$x, INDEX=list(close$cl), FUN=mean))
			newy <- as.vector(tapply(close$y, INDEX=list(close$cl), FUN=mean))
			cs2 <- cs[-Vids, ]
			cs2 <- rbind(cs2, data.frame(V1=newx, V2=newy, ID=i))
		
		} else cs2 <- cs
		n2 <- nrow(cs2)
		
		cs2 <- cs2[, 1:2]

		if (verbose) cat("number of coordinates after clustering:", n2, "\n")
		
		## use mst to find clusters
		d2 <- dist(cs2)
		mst <- spantree(d2)

		edgelist <- cbind(from=2:n2, to=mst$kid)
		toofar <- mst$dist > sep.dist
		if (all(toofar)) return(NULL)
		edgelist2 <- edgelist[!toofar,,drop=FALSE]
		distances <- mst$dist[!toofar]
		
		g <- graph.edgelist(edgelist2, directed=FALSE)
		
		memid <- clusters(g)$membership
		
		csg <- split(cs2, memid)
		if (verbose) cat("Number of polylines:", length(csg), "\n")
		
		linesc <- lapply(csg, function(csc) {
			nc <- nrow(csc)
			if (nc==1) return(NULL)
			## full network of short edges
			d2 <- fields.rdist.near(x1=csc, delta=max.opt.dist, max.points=1e8)
			sel2 <- d2$ind[,1] > d2$ind[,2]
			edgelistA <- d2$ind[sel2, , drop=FALSE]
			distancesA <- d2$ra[sel2]
			
			## full mst
			d3 <- dist(csc)
			mst <- spantree(d3)
			
			edgelistB <- cbind(from=2:nc, to=mst$kid)
			distancesB <- mst$dist
			for (i in 1:nrow(edgelistB)) {
				if (edgelistB[i, 1] < edgelistB[i, 2]) edgelistB[i, 1:2] <- edgelistB[i, 2:1]
			}
			
			if (nrow(edgelistA)==0) {
				edgelist <- edgelistB
			} else edgelist <- rbind(edgelistA, edgelistB)
			distances <- c(distancesA, distancesB)
			
			dupl <- duplicated(edgelist, MARGIN=1)
			edgelist <- edgelist[!dupl, ,drop=FALSE ]
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
		linesc <- linesc[!sapply(linesc, is.null)]
		
		lng <- sapply(linesc, LineLength)
		if (verbose) {
			if (length(csg)>1) {
				cat("Line lengths:", lng, "\n")
			}
			cat("Total line length:", sum(lng), "\n")
		} 
		Lines(linesc, ID=lvls[i])
	})
	isnull <- sapply(lines, is.null)
	
	lines <- lines[!isnull]
	IDs <- IDs[!isnull]
	
	if (!length(lines)) stop("No lines could be fitted.")
	shp3 <- SpatialLines(lines, proj4string=shps[[1]]@proj4string)
	data <- data.frame(ID=factor(lvls[IDs], levels=lvls), row.names=lvls[IDs])
	shp3 <- SpatialLinesDataFrame(shp3, data=data, match.ID=FALSE)
	shp3
}

get_coordinates <- function(shp, id) {
	n <- length(shp)
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
	coor$ID <- shp[[id]][idnr]
	coor
}
