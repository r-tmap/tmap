#' Append data to a shape object
#' 
#' Data, in the format of a data.frame, is appended to a shape object. This is either done by a left join where keys are specified for both shape and data, or by fixed order. Under coverage (shape items that do not correspond to data records), over coverage (data records that do not correspond to shape items respectively) as well as the existence of duplicated key values are automatically checked and reported via console messages. With \code{under_coverage} and \code{over_coverage} the under and over coverage key values from the last \code{append_data} call can be retrieved. Tip: run \code{append_data} without assigning the result to check the coverage.
#'
#' @param shp shape object, which is one of
#' \enumerate{
#'  \item{\code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialPointsDataFrame]{SpatialPoints(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialLinesDataFrame]{SpatialLines(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialPixelsDataFrame]{SpatialPixels(DataFrame)}}}
#' }
#' @param data data.frame
#' @param key.shp variable name of \code{shp} map data to be matched with \code{key.data}. If not specified, and \code{fixed.order} is \code{FALSE}, the ID's of the polygons/lines/points are taken.
#' @param key.data variable name of \code{data} to be matched with \code{key.shp}. If not specified, and \code{fixed.order} is \code{FALSE}, the row names of \code{data} are taken.
#' @param ignore.duplicates should duplicated keys in \code{data} be ignored? (\code{FALSE} by default)
#' @param ignore.na should NA values in \code{key.data} and \code{key.shp} be ignored? (\code{FALSE} by default)
#' @param fixed.order should the data be append in the same order as the shapes in \code{shp}?
#' @return Shape object with appended data. Tip: run \code{append_data} without assigning the result to check the coverage.
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
#' @rdname append_data
#' @export
append_data <- function(shp, data, key.shp = NULL, key.data = NULL, ignore.duplicates=FALSE, ignore.na=FALSE, fixed.order=is.null(key.data) && is.null(key.shp)) {
	spatialDF <- inherits(shp, c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame", "SpatialLinesDataFrame", "SpatialGridDataFrame", "SpatialPixelsDataFrame"))

	callAD <- deparse(match.call())
	
	dataName <- deparse(substitute(data))
	shpName <- deparse(substitute(shp))
	
	if (fixed.order) {
		if (length(shp)!=nrow(data)) 
			stop("Number of shapes not equal to number of data rows")
		data2 <- data
	} else {
		# key.data specification
		if (missing(key.data)) {
			message("No key.data specified. Therefore, rownames are taken as keys.\n")
			ids.data <- rownames(data)
		} else {
			if (!key.data %in% names(data)) stop("Variable \"", key.data, "\" not found in ", dataName, ".")
			ids.data <- as.character(data[[key.data]])
		} 
	
		# key.data remove duplicates
		if (any(duplicated(ids.data))) {
			duplicated_data <- unique(ids.data[duplicated(ids.data)])
			if (length(duplicated_data) >= 20) {
				duplicated_data <- paste0(paste(head(duplicated_data, 10), collapse=", "), ", ...")
			} else {
				duplicated_data <- paste(duplicated_data, collapse=", ")
			}
			if (ignore.duplicates) {
				message("data contains duplicated keys: ", duplicated_data)
			  data_ID <- which(!duplicated(ids.data))
				data <- data[data_ID, ]
				ids.data <- ids.data[data_ID]
			} else stop("data contains duplicated keys: ", duplicated_data, 
							  ". Set ignore.duplicates=TRUE to ignore duplicates in data.")
		} else {
		  data_ID <- 1L:nrow(data)
		}
		
		# key.data any NA?
		if (any(is.na(ids.data))) {
			if (ignore.na) {
				message(dataName, " key variable \"", key.data, "\" contains NA's, which are ignored")
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
				stop(shpName, " is not a Spatial*DataFrame, while key.shp is specified")
			if (!key.shp %in% names(shp@data))
				stop("Variable \"", key.shp, "\" not found in ", shpName, "@data")
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
			message(shpName, " key variable \"", key.shp, "\" contains duplicated keys:", duplicated_shp, "\n")
		}
	
		# key.shp any NA?
		if (any(is.na(ids.shp))) {
			if (ignore.na) {
				message(shpName, " key variable \"", key.shp ,"\" contains NA's, which are ignored")
				ids.shp[is.na(ids.shp)] <- "shp_key_NA"
			} else {
				stop(shpName, " key variable \"", key.shp ,"\" contains NA's. Set ignore.na = TRUE to ignore them.")
			}
		}

		# prepare data
		data2 <- data[match(ids.shp, ids.data),]
		
		data2[[key.data]] <- NULL

		# check coverage
		uc_id <- which(!(ids.shp %in% ids.data))
		oc_id <- which(!(ids.data %in% ids.shp))

		ndata <- nrow(data)
		nshp <- length(shp)
				
		#ids.data <- setdiff(ids.data, "data_key_NA")
		#ids.shp <- setdiff(ids.shp, "shp_key_NA")
		if (length(uc_id)==0 && length(oc_id)==0) {
			message("Keys match perfectly.\n")
			uc_res <- "No under coverage: each shape feature has appended data."
			oc_res <- "No over coverage: each data record is appended to a shape feature."
		} else {
			if (length(uc_id)) {
				nnm <- length(uc_id)
				if (nnm==nshp) stop("No match found")
				uc_res <- paste("Under coverage: ", nnm, " out of ", nshp, " shape features did not get appended data." , sep="")
				message(uc_res, " Run under_coverage() to get the corresponding feature id numbers and key values.")
			} else {
				uc_res <- "No under coverage: each shape feature has appended data."
			}
			if (length(oc_id)) {
				nnm <- length(oc_id)
				oc_res <- paste("Over coverage: ", nnm, " out of ", ndata, " data records were not appended.", sep="")
				message(oc_res, " Run over_coverage() to get the corresponding data row numbers and key values.")
			} else {
				oc_res <- "No over coverage: each data record is appended to a shape feature."
			}
		}
		assign(".underCoverage", list(result=uc_res, call=callAD, id=uc_id, value=shp@data[uc_id, key.shp]), envir = .TMAP_CACHE)
		assign(".overCoverage", list(result=oc_res, call=callAD, id=data_ID[oc_id], value=data[oc_id, key.data]), envir = .TMAP_CACHE)
		
	}
	
	# attach data to shp
	if (spatialDF) {
		doubleNames <- names(data2) %in% names(shp@data)
		names(data2)[doubleNames] <- paste(names(data2)[doubleNames], ".data", sep="")
		shp@data <- cbind(shp@data, data2)
	} else if (inherits(shp, "SpatialPolygons")) {
		shp <- SpatialPolygonsDataFrame(shp, data2, match.ID = FALSE)
	} else if (inherits(shp, "SpatialPoints")) {
		shp <- SpatialPointsDataFrame(shp, data2, match.ID = FALSE)
	} else if (inherits(shp, "SpatialLines")) {
		shp <- SpatialLinesDataFrame(shp, data2, match.ID = FALSE)
	} else if (inherits(shp, "SpatialGrid")) {
		shp <- SpatialGridDataFrame(shp, data2)
	} else if (inherits(shp, "SpatialPixels")) {
		shp <- SpatialPixelsDataFrame(shp, data2)
	} else {
		stop("shp is not a shape file")
	}
	invisible(shp)
}

#' @rdname append_data
#' @export
under_coverage <- function() {
	res <- get(".underCoverage", envir = .TMAP_CACHE)
	if (is.null(res)) {
		message("Function append_data not called yet.")
		invisible()
	} else {
		#if (length(res$id)==0) message("No under coverage: each shape feature has appended data.")
		res
	}
}

#' @export
#' @rdname append_data
over_coverage <- function() {
	res <- get(".overCoverage", envir = .TMAP_CACHE)
	if (is.null(res)) {
		message("Function append_data not called yet.")
		invisible()
	} else {
		#if (length(res$id)==0) message("No over coverage: each data record is appended to a shape feature.")
		res
	}
}
