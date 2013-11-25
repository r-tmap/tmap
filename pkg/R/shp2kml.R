#' Transform a shape object to kml file
#' 
#' This function creates a kml file from a shape object. kml files can be read with Google Earth.
#'
#' @param shp shape object
#' @param file filename of the kml file
#'
#'
#' @export
#' @example ../examples/shp2kml.R
shp2kmlPolygons <- function(shp, cols, file="./temp.kml") {
    shpname <- deparse(substitute(shp))
    
    #
    
    # transform to wgs84 coordinates
    if (any(shp@bbox > 200000)) shp <- rd2wgs84(shp)
    
    writeOGR(shp, "test.kml", shp@data, driver="KML")
    
    out <- mapply(function(x, cl) { kmlPolygon(x,
    			  						 name=as(shp, "data.frame")[slot(x, "ID"), "GM_NAAM"], 
    			  						 col=cl, lwd=1.5, border='black',
    			  						 description=paste("ISO3:", slot(x, "ID"))) },
    			  slot(shp, "polygons"), cols, SIMPLIFY=TRUE)
    kmlFile <- file(file, "w")
    
    
    cat(kmlPolygon(kmlname=shpname, kmldescription="<i>Fancy map</i>")$header, 
        file=kmlFile, sep="\n")
    
    
    cat(unlist(out["style",]), file=kmlFile, sep="\n")
    cat(unlist(out["content",]), file=kmlFile, sep="\n")
    cat(kmlPolygon()$footer, file=kmlFile, sep="\n")
    close(kmlFile)
}

shp2kmlOverlay <- function(shp, colsList, file="./temp.kml", time, ...) {
	
	shp <- rd2wgs84(shp)
	
	ge <- GE_SpatialGrid(shp, maxPixels=1000)
	
	timeStrings <- format(time, "%Y-%m-%d_%Hh%m")
	
	for (i in 1:length(colsList)) {
		png(file=paste0(timeStrings[i],".png"), width=ge$width, height=ge$height,
			bg="transparent")
		par(mar=c(0,0,0,0), xaxs="i", yaxs="i")
		cl <- colsList[[i]]
		cartoMap(shp, shp.col=cl, plot.bg=NA, ...)
		dev.off()
	}
	
	
	
	# time example 2010-05-28T02:02:09Z
	timeStrings2 <- format(time, "%Y-%m-%dT%H:%M:%S+02")
		
	x <- kmlOverlay(ge, file, paste0(timeStrings[1],".png"))
	kmlX <- c(x[1:2], "<Folder>", paste0("<name>", basename(file), "</name>"))
	for (i in 1:length(colsList)) {
		x <- kmlOverlay(ge, file, paste0(timeStrings[i],".png"),  name=timeStrings[i])
		y <- c(x[3], 
			   "<TimeSpan>",
			   paste0("<begin>", timeStrings2[i], "</begin>"),
			   paste0("<end>", timeStrings2[i+1], "</end>"),
			   	"</TimeSpan>",
			   x[4:6],
			   "</GroundOverlay>")
		kmlX <- c(kmlX, y)
	}
	kmlX <- c(kmlX, "</Folder>" , "</kml>")
	
	f <- file(file, "w")
	writeLines(kmlX, f)
	close(f)
}

