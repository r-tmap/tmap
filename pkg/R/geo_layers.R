geo.borders <- function(shp, col="black", lwd=1, lty="solid") {
	shp_name <- deparse(substitute(shp))
	g <- list(geo_borders=list(shp=shp_name, col=col, lwd=lwd, lty=lty))
	class(g) <- "geo"
	g
}

geo.fill <- function(shp, col="lightgray") {
	shp_name <- deparse(substitute(shp))
	g <- list(geo_fill=list(shp=shp_name, col=col))
	class(g) <- "geo"
	g
}

geo.bubbles <- function(coor, size=1, col="red", border=NA, scale=1) {
	coor_name <- deparse(substitute(coor))
	g <- list(geo_bubbles=list(coor=coor_name, bubble.size=size, bubble.col=col, bubble.border=border, bubble.scale=scale))
	class(g) <- "geo"
	g
}

geo.text <-  function(shp, text=names(shp)[1], cex=1) {
	shp_name <- deparse(substitute(shp))
	g <- list(geo_text=list(shp=shp_name, text=text, cex=cex))
	class(g) <- "geo"
	g
}

	
geo.choropleth <- function(shp, col, n = 5,
							style = "pretty",
							breaks = NULL,
							palette = "RdBu",
						    labels = NULL,
							auto.palette.mapping = TRUE,
							contrast = 1,
							colorNA = "#FF1414") {
	
	shp_name <- deparse(substitute(shp))
	g <- list(geo_choropleth=list(shp=shp_name, col=col, n=n, style=style, breaks=breaks, palette=palette, labels=labels, 
							auto.palette.mapping=auto.palette.mapping, contrast=contrast, colorNA=colorNA))
	class(g) <- "geo"
	g
}	

geo.bubblemap <- function(coor, size = NULL, col = NULL,
						  border=NA,
						  scale=1,
						  n = 5, style = "pretty",
						  breaks = NULL,
						  palette = NULL,
						  labels = NULL,
						  auto.palette.mapping = TRUE,
						  contrast = 1,
						  colorNA = "#FF1414") {
	coor_name <- deparse(substitute(coor))
	g <- list(geo_bubblemap=list(coor=coor_name, bubble.size=size, bubble.col=col, bubble.border=border,
								 bubble.scale=scale,
								 n=n, style=style, breaks=breaks, palette=palette, labels=labels,
								 auto.palette.mapping=auto.palette.mapping, contrast=contrast))
	class(g) <- "geo"
	g
}



geo.grid <- function(ncol=NULL, nrow=NULL, 
					 free.scales=FALSE)	{
	g <- list(geo_grid=list(ncol=ncol, nrow=nrow, free.scales=free.scales))
	class(g) <- "geo"
	g
}

geo.zoom <- function(xlim = c(0, 1),
					ylim = c(0, 1),
					units = "rel") {
	g <- list(geo_zoom=list(xlim=xlim, ylim=ylim, units=units))
	class(g) <- "geo"
	g
}

geo.theme <- function(title=NULL,
					  title.cex=1.5,
					  bg.color="yellow",
					  show.legend.text=TRUE,
					  type.legend.plot = "hist",
					  legend.position = c("left", "top"),
					  legend.plot.size = NA,
					  legend.cex = 0.8,
					  legend.digits = 2,
					  title.position = c("left", "top"),
					  margins = NA,
					  draw.frame=NA,
					  frame.lwd=1,
					  legend.only=FALSE) {
	g <- list(geo_theme=list(title=title, title.cex=title.cex, 
							 bg.color=bg.color,
							 show.legend.text=show.legend.text,
							 type.legend.plot=type.legend.plot, 
							 legend.position=legend.position,
							 legend.plot.size=legend.plot.size, 
							 legend.cex=legend.cex,
							 legend.digits=legend.digits, 
							 title.position=title.position,
							 margins=margins, draw.frame=draw.frame, 
							 frame.lwd=frame.lwd, legend.only=legend.only))
	class(g) <- "geo"
	g
}	

# geo.legend
# 	
# 
"+.geo" <- function(e1, e2) {
	g <- c(e1,e2)
	class(g) <- "geo"
	g
}

