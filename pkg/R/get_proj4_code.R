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
		   rd="+init=epsg:28992 +towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812",
		   x)
	
	check <- CRSargs(CRS(y))
	y
}