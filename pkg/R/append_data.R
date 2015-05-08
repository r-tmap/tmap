#' Append data to a shape object
#' 
#' Data, in the format of a data.frame, is appended to a shape object. This is either done by a right join where keys are specified for both data and shape, or by fixed order.
#'
#' @param shp shape object, which is one of
#' \enumerate{
#'  \item{\code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialPointsDataFrame]{SpatialPoints(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialLinesDataFrame]{SpatialLines(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialGridDataFrame]{SpatialPoints(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialPixelsDataFrame]{SpatialPixels(DataFrame)}}}
#' }
#' @param data data.frame
#' @param key.shp variable name of \code{shp} map data to be matched with \code{key.data}. If not specified, and \code{fixed.order} is \code{FALSE}, the ID's of the polygons/lines/points are taken.
#' @param key.data variable name of \code{data} to be matched with \code{key.shp}. If not specified, and \code{fixed.order} is \code{FALSE}, the row names of \code{data} are taken.
#' @param ignore.duplicates should duplicated keys in \code{data} be ignored? (\code{FALSE} by default)
#' @param ignore.na should NA values in \code{key.data} and \code{key.shp} be ignored? (\code{FALSE} by default)
#' @param fixed.order should the data be append in the same order as the shapes in \code{shp}?
#' @return Shape object with appended data.
#' @examples
#' \dontrun{
#' data(Europe)
#' 
#' f <- tempfile()
#' download.file("http://kejser.org/wp-content/uploads/2014/06/Country.csv", destfile = f)
#' domain_codes <- read.table(f, header=TRUE, sep="|")
#' unlink(f)
#' 
#' domain_codes <- subset(domain_codes, select = c("Alpha3Code", "TopLevelDomain"))
#' domain_codes$Alpha3Code <- toupper(domain_codes$Alpha3Code)
#' 
#' Europe <- append_data(Europe, domain_codes, key.shp = "iso_a3", key.data = "Alpha3Code",
#'     ignore.na = TRUE)
#' 
#' qtm(Europe, text="TopLevelDomain")
#' }
#' @export
append_data <- function(shp, data, key.shp = NULL, key.data = NULL, ignore.duplicates=FALSE, ignore.na=FALSE, fixed.order=is.null(key.data) && is.null(key.shp)) {
	spatialDF <- inherits(shp, c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame", "SpatialLinesDataFrame", "SpatialGridDataFrame", "SpatialPixelsDataFrame"))

	if (fixed.order) {
		if (length(shp)!=nrow(data)) 
			stop("Number of shapes not equal to number of data rows")
	} else {
		# key.data specification
		if (missing(key.data)) {
			cat("No key.data specified. Therefore, rownames are taken as keys.\n")
			ids.data <- rownames(data)
		} else ids.data <- as.character(data[[key.data]])
	
		# key.data remove duplicates
		if (any(duplicated(ids.data))) {
			duplicated_data <- unique(ids.data[duplicated(ids.data)])
			if (length(duplicated_data) >= 20) {
				duplicated_data <- paste0(paste(head(duplicated_data, 10), collapse=", "), ", ...")
			} else {
				duplicated_data <- paste(duplicated_data, collapse=", ")
			}
			if (ignore.duplicates) {
				message(paste("data contains duplicated keys:", duplicated_data))
				data <- data[!duplicated(ids.data), ]
				ids.data <- ids.data[!duplicated(ids.data)]
			} else stop(paste("data contains duplicated keys:", duplicated_data, 
							  "Set ignore.duplicates=TRUE to ignore duplicates in data."))
		}
		
		# key.data any NA?
		if (any(is.na(ids.data))) {
			if (ignore.na) {
				message("data key contains NA's, which are ignored")
				ids.data[is.na(ids.data)] <- "data_key_NA"
			} else {
				stop("data key contains NA's. Set ignore.na = TRUE to ignore them.")
			}
		}
		
		# key.shp specification
		if (missing(key.shp)) {
			# if key.shp is NULL, then take the polygon id's
			ids.shp <- get_IDs(shp)
		} else {
			# use the key.shp variable of shp@data
			if (!spatialDF) 
				stop("shp is not a Spatial*DataFrame, while key.shp is specified")
			if (!key.shp %in% names(shp@data))
				stop("key.shp is not available in shp@data")
			ids.shp <- as.character(shp@data[[key.shp]])
		}
		
		# key.shp check duplicates
		if (any(duplicated(ids.shp))) {
			duplicated_shp <- unique(ids.shp[duplicated(ids.shp)])
			if (length(duplicated_shp) >= 20) {
				duplicated_shp <- c(paste(head(duplicated_shp, 10), collapse=", "), ", ...")
			} else {
				duplicated_shp <- paste(duplicated_shp, collapse=", ")
			}
			cat("shp contains duplicated keys:", duplicated_shp, "\n")
		}
	
		# key.shp any NA?
		if (any(is.na(ids.shp))) {
			if (ignore.na) {
				message("shp key contains NA's, which are ignored")
				ids.shp[is.na(ids.shp)] <- "shp_key_NA"
			} else {
				stop("shp key contains NA's. Set ignore.na = TRUE to ignore them.")
			}
		}
				
		# prepare data
		data <- data[match(ids.shp, ids.data),]

		# check coverage
		ids.data <- setdiff(ids.data, "data_key_NA")
		ids.shp <- setdiff(ids.shp, "shp_key_NA")
		if (setequal(ids.data, ids.shp)) {
			cat("Keys match perfectly.\n")
		} else {
			if (!all(ids.shp %in% ids.data)) {
				notMatched.shp <- setdiff(ids.shp, ids.data)
				nnm <- length(notMatched.shp)
				nshp <- length(ids.shp)
				if (nnm==nshp) stop("No match found")
				message(paste("Under coverage. No data for", nnm, "out of", 
							  nshp, "polygons:", 
							  paste(head(notMatched.shp, 5), collapse=", "),
							  ifelse(length(notMatched.shp)>5, ", ...", "")))
			}				
			if (!all(ids.data %in% ids.shp)) {
				notMatched.data <- setdiff(ids.data, ids.shp)
				nnm <- length(notMatched.data)
				ndata <- length(ids.data)
				message(paste("Over coverage.", nnm, "out of", ndata, 
							  "unmatched data records:", 
							  paste(head(notMatched.data, 5), collapse=", "),
							  ifelse(length(notMatched.data)>5, ", ...", "")))
			}				
		}
		
		
	}
	
	# attach data to shp
	if (spatialDF) {
		doubleNames <- names(data) %in% names(shp@data)
		names(data)[doubleNames] <- paste(names(data)[doubleNames], ".data", sep="")
		shp@data <- cbind(shp@data, data)
	} else if (inherits(shp, "SpatialPolygons")) {
		shp <- SpatialPolygonsDataFrame(shp, data, match.ID = FALSE)
	} else if (inherits(shp, "SpatialPoints")) {
		shp <- SpatialPointsDataFrame(shp, data, match.ID = FALSE)
	} else if (inherits(shp, "SpatialLines")) {
		shp <- SpatialLinesDataFrame(shp, data, match.ID = FALSE)
	} else if (inherits(shp, "SpatialGrid")) {
		shp <- SpatialGridDataFrame(shp, data)
	} else if (inherits(shp, "SpatialPixels")) {
		shp <- SpatialPixelsDataFrame(shp, data)
	} else {
		stop("shp is not a shape file")
	}
	shp
}