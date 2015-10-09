get_proj4_code <- function(x) {
	if (is.null(x)) return(NULL)
	if (is.na(x)) return(NA)
	y <- switch(x,
		   longlat="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
			latlong="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
			WGS84="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
		   NAD83="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs",
			NAD83="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs",
			NAD27="+proj=longlat +ellps=clrk66 +datum=NAD27 +no_defs",
			wintri="+proj=wintri +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
		   robin="+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
		   eck4="+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
		   hd="+proj=cea +lat_ts=37.5",
		   gall="+proj=cea +lon_0=0 +lat_ts=45 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
		   merc="+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
		   mill="+proj=mill +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +R_A +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
		   eqc0="+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
		   eqc30="+proj=eqc +lat_ts=30 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
		   eqc45="+proj=eqc +lat_ts=45 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
		   rd="+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +towgs84=565.2369,50.0087,465.658,-0.406857330322398,0.350732676542563,-1.8703473836068,4.0812 +no_defs",
		   x)
	if (substr(y, 1, 3)=="utm") {
		y <- paste("+proj=utm +zone=", substr(y, 4, 5), ifelse(substr(y, 6, 6)=="s", " +south", ""), " +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep="")
	}
	check <- CRSargs(CRS(y))
	y
}

