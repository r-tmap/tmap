process_layers <- function(g, free.scales.choro, free.scales.bubble.size, 
						   free.scales.bubble.col, legend.digits, legend.NA.text) {
	shp <- g$geo_shape$shp
	
	# determine plotting order 
	bubbleID <- which(names(g) %in% c("geo_bubbles", "geo_bubblemap"))
	textID <- which(names(g) %in% c("geo_text"))
# 	plotorder <- ifelse(!length(bubbleID) && !length(textID), NA,
# 				 ifelse(!length(bubbleID), "text",
# 				 ifelse(!length(textID), "bubble",
# 				 ifelse(textID[1] <	bubbleID[1], "text_bubble", "bubble_text"))))
	plotorder <- 	"text_bubble"
	
	# border info
	gborders <- if (is.null(g$geo_borders)) {
		list(col=NA, lwd=1, lty="blank")
	} else g$geo_borders
	
	# fill info
	gfill <- if (is.null(g$geo_choropleth)) {
		fill <- if (is.null(g$geo_fil)) NA else g$geo_fill$col
		list(fill=fill, choro.values=NA,
					  choro.legend.labels=NA,
					  choro.legend.palette=NA,
					  choro.breaks=NA,
					  xfill=NA)
	} else process_choro(shp, g$geo_choropleth, free.scales.choro, legend.digits, legend.NA.text)
	
	# bubble info
	gbubble <- if (is.null(g$geo_bubblemap)) {
		gbub <- if (is.null(g$geo_bubbles)) {
			list(bubble.size=NULL)
		} else g$geo_bubbles
		
		c(gbub, list(
			bubble.legend.labels=NA,
			bubble.legend.palette=NA,
			bubble.legend.sizes=NA,
			bubble.legend.size_labels=NA,
			xsize=NA,
			xcol=NA))
	} else process_bubblemap(shp, g$geo_bubblemap, free.scales.bubble.size, free.scales.bubble.col, legend.digits, legend.NA.text)

	# text info
	gtext <- if (is.null(g$geo_text)) {
		list(text=NULL)
	} else g$geo_text
	
	c(list(shp=shp, varnames=list(choro.fill=gfill$xfill, bubble.size=gbubble$xsize, bubble.col=gbubble$xcol), plotorder=plotorder), gborders, gfill, gbubble, gtext)
}