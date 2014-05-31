get_proj4_code <- function(x) {
	if (is.null(x)) return(NULL)
	y <- switch(x,
		   longlat="+proj=longlat +datum=WGS84",
		   wintri="+proj=wintri",
		   robin="+proj=robin",
		   eck4="+proj=eck4",
		   hd="+proj=cea +lat_ts=37.5",
		   gall="+proj=cea +lon_0=0 +x_0=0 +y_0=0 +lat_ts=45",
		   merc="+proj=merc",
		   mill="+proj=mill",
		   eqc0="+proj=eqc",
		   eqc30="+proj=cea +lat_ts=30",
		   eqc45="+proj=cea +lat_ts=45",
		   rd="+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +towgs84=565.2369,50.0087,465.658,-0.406857330322398,0.350732676542563,-1.8703473836068,4.0812 +no_defs",
		   x)
	if (substr(y, 1, 3)=="utm") {
		y <- paste("+proj=utm +zone=", substr(y, 4, 5), ifelse(substr(y, 6, 6)=="s", " +south", ""), " +ellps=WGS84", sep="")
	}
	check <- CRSargs(CRS(y))
	y
}