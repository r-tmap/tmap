#' Sample dots from spatial polygons
#' 
#' Sample dots from spatial polygons according to a spatial distribution of a population. The population may consist of classes. The output, a SpatialPointsDataFrame, can be used to create a dot map (see \code{\link{tm_dots}}), where the dots are colored according to the classes.
#' 
#' The sampling algoritm is the following: TO DO
#' 
#' @param shp A shape object, more specifically, a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}.
#' @param vars Names of one or more variables that are contained in \code{shp}. If \code{vars} is not provided, the dots are sampled uniformly. If \code{vars} consists of one variable name, the dots are sampled according to the distribution of the corresponding variable. If \code{vars} consist of more than one variable names, then the dots are sampled according to the distributions of those variables. A categorical variable is added that contains the distrubtion classes (see \code{var.name}).
#' @param convert2density Should the variables be converted to density values? Density values are used for the sampling algorithm, so use \code{TRUE} when the values are absolute counts.
#' @param nrow Number of grid rows
#' @param ncol Number of grid colums
#' @param N Number of grid points
#' @param npop Population total. If \code{NA}, it is recontructed from the data. If density values are specified, the population total is approximated using the polygon areas (see also \code{unit} and \code{unit.size}).
#' @param n Number of sampled dots
#' @param w Number of population units per dot. It is the population total divided by \code{n}. If specified, \code{n} is calculated accordingly.
#' @param shp.id Name of the variable of \code{shp} that contains the polygon identifying numbers or names.
#' @param var.name Name of the variable that will be created to store the classes. The classes are defined by \code{vars}, and the labels can be configured with \code{var.labels}. 
#' @param var.labels Labels of the classes (see \code{var.name}).
#' @param unit Unit, see \code{\link{calc_densities}}. Needed to relate \code{npop} to \code{w}, if they are not both specified.
#' @param unit.size Unit size, see \code{\link{calc_densities}}. Needed to relate \code{npop} to \code{w}, if they are not both specified.
#' @param randomize should the order of sampled dots be randomized? The dots are sampled class-wise (specified by \code{vars}). If this order is not randomized (so if \code{randomize=FALSE}), then the dots from the last class will be drawn on top, which may introduce a perception bias. By default \code{randomize=TRUE}, so the sampled dots are randomized to prevent this bias.
#' @param ... other arguments passed on to \code{\link{calc_densities}} and \code{\link{approx_areas}}
#' @export
#' @example ../examples/sample_dots.R
#' @importFrom raster raster extent rasterize couldBeLonLat crop
sample_dots <- function(shp, vars=NULL, convert2density=FALSE, nrow=NA, ncol=NA, N=250000, npop=NA, n=10000, w=NA, shp.id=NULL, var.name="class", var.labels=vars, unit="km", unit.size=1000, randomize=TRUE, ...) {
	args <- list(...)
	
	bbx <- shp@bbox
	asp <- get_asp_ratio(shp)
	np <- length(shp)
	prj <- shp@proj4string
	
	projected <- is_projected(shp)
	
	k <- length(vars)
	
	## process shape data
	message("Please wait...")	
	data <- shp@data[, vars, drop=FALSE]
	data <- as.data.frame(lapply(data, function(d) {
		if (any(is.na(d))) {
			message("Shape data contains missing values. Replaced with 0.")
			d[is.na(d)] <- 0
		}
		if (any(is.nan(d)) || any(is.infinite(d)) || any(d<0)) stop("Shape data contains NaN, infinite, or negative values")
		d
	}))
	

	## find total population number, and convert data to density values
	if (convert2density) {
		if (!projected) warning("shp should be projected when convert2density=TRUE, for otherwise the sampling method is unreliable.")
		
		if (is.na(npop)) npop <- sum(data)
		
		# calculate densities
		dens_args <- args[names(args) %in% c("total.area", "suffix")]
		data <- do.call("calc_densities", args = c(list(shp=shp, var=vars, unit=unit, unit.size=unit.size, drop=FALSE), dens_args))
		data[is.na(data)] <- 0
	} else {
		# calculate absolute values
		if (!("total.area" %in% names(args)) && !projected) warning("unable to determine population total, unless total.area is specified.")
		if (is.na(npop)) {
			area_approx_args <- args[names(args) == "total.area"]
			areas <- do.call("approx_areas", args = c(list(shp=shp, unit=unit, unit.size=unit.size), area_approx_args))
			
			npop <- sum(rowSums(data) * areas)
		}
	}
	
	# append aux variables
	data$TOTAL__VARS <- rowSums(data)
	data$ID__POLY <- 1:np
	
	## align npop, n, and w
	if (is.na(w)) {
		w <- round(npop / n)
	} else {
		n <- round(npop / w)
	}

	## determine grid size
	if (is.na(nrow) || is.na(ncol)) {
		nrow <- round(sqrt(N/asp))
		ncol <- round(N / nrow)
	}
	N <- nrow * ncol
	
	## rasterize (SLOW...)
	r <- raster(extent(bbx), nrows=nrow, ncols=ncol, crs=prj)
	r2 <- rasterize(shp, r, field=data$ID__POLY) #raster with poly id's
	r2@data@names <- "ID__POLY"
	g <- as(r2, "SpatialGridDataFrame")
	

	## append data to raster
	suppressMessages({
		g <- append_data(g, data=data, key.shp = "ID__POLY", key.data="ID__POLY", ignore.na=TRUE)
	})
	
	## calculate data distributions
	sm <- sum(g$TOTAL__VARS, na.rm=TRUE)
	
	# find number of grid points (N2) in region
	notNA <- which(!is.na(g$ID__POLY))
	N2 <- length(notNA)
	
	## calculate distributions (which sum up to 1)
	distr <- g@data[notNA, vars, drop=FALSE] / sm
	
	## for each grid cell with overlapping data distributions, sample a data point, and sum the probabilities
	distr2 <- assign_rowsum_to_sampled_value(distr)
	
	# calculate number of points per class
	sizes <- round(colSums(distr2) * n)
	
	# correct possible difference due to rounding
	if (sum(sizes)!=n) sizes[1] <- n - sum(sizes[-1])
	
	# check if n is not too large
	mx_per_class <- apply(distr2, 2, function(i)sum(i!=0))
	if (any(sizes>mx_per_class)) {
		rescale <- min(mx_per_class/sizes)
		sizes <- floor(sizes * rescale)
		n <- sum(sizes)
		w <- round(npop / n)
		warning("Too many dots. Number of dots downsized.")
	}
	
	# do the sampling (could be slow...)
	samples <- mapply(function(x, size) {
		sample.int(n=N2, size= size, replace=FALSE, prob=x)
	}, as.data.frame(distr2), sizes, SIMPLIFY=FALSE)
	sam <- unlist(samples)
	stopifnot(n == length(sam))
	
	message(paste("Grid size: ", nrow, " by ", ncol, " ("), N, ")", sep="")
	message(paste("Number of dots =", n))

	
	if (projected || (!convert2density && ("total.area" %in% names(args)))) {
		message(paste("Population size =", npop))
		message(paste("One dot represents", w, "population units"))
	}
	
	# convert to SPointsDF
	p <- as(g, "SpatialPointsDataFrame")
	p2 <- p[sam, ]
	
	if (k > 1) {
		p2[[var.name]] <- factor(unlist(mapply(rep, vars, sapply(samples, length))), levels=vars, labels=var.labels)
	}
	
	# append ID variable
	if (!missing(shp.id)) p2[[shp.id]] <- shp[[shp.id]][p2$ID__POLY] 

	# clean data
	if (k<= 1) {
		p2 <- as(p2, "SpatialPoints")
	} else {
		p2@data[, c("ID__POLY", "ID__POLY.data", "TOTAL__VARS", vars)] <- list()
	}

	# shuffle points to prevent overplotting bias
	if (randomize) {
		p2[sample.int(n), ]
	} else p2
}

#	For each row in matrix m, a value is sampled based on the values of that row.
#	The corresponding row in the new matrix m2 will have the rowsum on that position, and zeros elsewhere
#	Why used? Say a grid point has positive population density values for two out of three classes, say .01 and .03.
#	The row in m is [0	.01	.03]. However, the point draw at the corresponding grid cell can have only one color.
#	So, one value per row is sampled with the row values taken as probabilites. So 3/4 probability that the 
#	corresponding row in m2 is [0	0	.04] and 1/4 probability that it is [0	.04	0].
#	Only if the positive value in the m2 row is sampled later on, a dot will be colored according to the 
#	corresponding class.
assign_rowsum_to_sampled_value <- function(m) {
	nr <- nrow(m)
	nc <- ncol(m)
	
	rnd <- matrix(runif(nr*nc), nrow=nr, ncol=nc)
	rnd2 <- m * rnd
	
	rnd_max <- apply(rnd2, 1, which.max)
	ind <- nr*(rnd_max-1) + (1:nr)
	
	m2 <- matrix(0, nrow=nr, ncol=nc)
	m2[ind] <- rowSums(m)
	m2
}