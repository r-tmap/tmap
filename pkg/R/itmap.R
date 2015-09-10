#' Interactive thematic map
#' 
#' Convert the tmap output to an interactive SVG, that can be plot in RStudio. In development. Requires the development version of svgPanZoom, see \url{devtools::install_github("timelyportfolio/svgPanZoom")}.
#' 
#' @param tm tmap object. A tmap object is created with \code{\link{qtm}} or by stacking \code{\link{tmap-element}}s.
#' @param file file name. If specified, the SVG image is saved to this file.
#' @param width width
#' @param height height
#' @importFrom gridSVG grid.garnish grid.export
#' @import svgPanZoom
#' @import htmlwidgets
#' @import XML
#' @example ../examples/itmap.R
#' @export
itmap <- function(tm, file=NULL, width = NULL, height = NULL ) {
	tmp <- tempfile()

	# Can I get the grid object, without actually plotting it?
	# A workaround could be to plot it in another device, e.g:
	png(tmp, 
		width = convertWidth(unit(1,"npc"), "points", valueOnly = TRUE),
		height = convertHeight(unit(1,"npc"), "points", valueOnly = TRUE)
	)
	res <- print(tm)
	

	## garnish polygons and lines (they are contained in groups)
	## TO DO: thick border on mouse over not working
	mapply(function(v, nm) {
		type  <- substr(nm, 4, 4) #p, b, or l, for resp polygons, bubbles and lines
		
		if (type %in% c("p", "l")) {
			m <- mapply(function(vec, vecn) {
				if (vecn=="ID") as.character(vec) else paste(vecn, vec, sep="=")
			}, v, names(v))
			hover_text <- apply(m, MARGIN = 1, paste, collapse=" ")
			lapply(
				seq.int(1,length(hover_text))
				,function(n){
					lwd <- grid.get(paste(nm, n, sep="_"))$gp$lwd # why is this different from SVG file?
					grid.garnish(
						paste(nm, n, sep="_")
						, title=hover_text[n]
						, onmouseover=sprintf("Array.prototype.map.call(this.querySelectorAll('path'),function(d){d.setAttribute('stroke-width', '%.2f')})", lwd + 1, "');")
						, onmouseout=sprintf("Array.prototype.map.call(this.querySelectorAll('path'),function(d){d.setAttribute('stroke-width', '%.2f')})", lwd, "');")
						, group = TRUE
					)
					grid.get(paste(nm, n, sep="_"))
				}
			)
		} else NULL
	}, res, names(res))
	
	tmap_svg <- grid.export(name = NULL)$svg
	dev.off()
	unlink(tmp)
	
	# set nested title for polygons
	# TODO same for tm_lines
	xpathApply(
		tmap_svg
		,"//*[local-name() = 'g' and starts-with(@id, 'tm_polygon')]"
		,function(g_el){
			if("title" %in% names(xmlAttrs(g_el))){
				addChildren(
					g_el
					, newXMLNode("title",xmlAttrs(g_el)[["title"]])
				)
			}
		}
	)
	
	mapply(function(v, nm) {
		type  <- substr(nm, 4, 4) #p, b, or l, for resp polygons, bubbles and lines
		if (type == "b") {
			m <- mapply(function(vec, vecn) {
				if (vecn=="ID") as.character(vec) else paste(vecn, vec, sep="=")
			}, v, names(v))
			hover_text <- apply(m, MARGIN = 1, paste, collapse=" ")
			mapply(
				function(el,title){
					xmlAttrs(el) <-  c("title" = title)
					copy_attrs <- xmlAttrs(el)
					copy_circle <- newXMLNode("circle")
					xmlAttrs(copy_circle) <- copy_attrs
					title_node <- newXMLNode("title",copy_attrs[["title"]])
					newg <- replaceNodes(
						el
						,newXMLNode("g",.children = list(copy_circle,title_node))
					)
					newg
				}
				, getNodeSet( tmap_svg, paste0("//*[local-name() = 'circle' and starts-with(@id, '", nm, "')]"))
				, hover_text
			)
		}
	}, res, names(res))
	
	if (!missing(file)) {
		cat(saveXML(tmap_svg), file = file)
	}
	
	# add pan zoom with svgPanZoom htmlwidget
#	svgPanZoom(
#		tmap_svg  #grid.export(name = NULL)$svg #works but no interactivity from above
#		, controlIconsEnabled = TRUE
#	)
	
	# restrict zoom to just the mapElements
	mapel <- getNodeSet( tmap_svg,"//*[contains(@id,'mapElements.1')]")
	xmlAttrs(mapel[[1]]) <- c(xmlAttrs(mapel[[1]]),"transform"="scale(1,-1)")
	
	mapel_container <- newXMLNode(
		"g"
		, attrs = c( "transform"="scale(1,-1)")
	)
	mapel_g <- newXMLNode(
		"g"
		, attrs = c("class"="map_viewport")
	)
	replaceNodes( mapel[[1]], mapel_container )
	addChildren( mapel_container, mapel_g )
	addChildren( mapel_g, mapel[[1]] )
	
	# try to append random number to
	#  clip-path attribute and id to avoid id conflict
	lapply(
		getNodeSet(tmap_svg,"//*[contains(@clip-path,'url')]")
		,function(g_clip){
			# ugly way to get a pseudo random id to append
			random_id <- round(runif(1,1,10),4)
			x_attrs = xmlAttrs(g_clip)
			clip_id <- gsub(x=x_attrs[["clip-path"]],pattern="(url\\(#)(.*)(\\))",replacement="\\2",perl=T)
			clip <- getNodeSet(
				tmap_svg
				, sprintf("//*[@id = '%s']", clip_id)
		  	)[[1]]
			xmlAttrs(clip) <- c(id = sprintf("%s_%s",clip_id,random_id))
			x_attrs[["clip-path"]] <- gsub(
				x = x_attrs[["clip-path"]]
				,pattern = "(clipPath)"
				,replacement = paste0("\\1_", random_id)
			)
			xmlAttrs(g_clip) <- x_attrs
		}
	)
	# also remove the map frame rect, since will no longer fit
	#invisible(xpathApply(tmap_svg,"//*[local-name()='g'][contains(@id,'mapFrame')]",removeNodes))
	
	#getNodeSet(svg,"//*[local-name()='g' and contains(@id,'mapFrame')]//*[local-name()='rect']")

	# force mapBG to fill svg
	mapBG <- getNodeSet(tmap_svg,"//*[contains(@id,'mapBG')]//*[local-name()='rect']")[[1]]
	mapBG_attrs <- xmlAttrs(mapBG)
	svg_viewbox <- strsplit(xmlAttrs(xmlRoot(tmap_svg))[["viewBox"]]," ")[[1]]
	mapBG_attrs[["x"]] <- svg_viewbox[1]
	mapBG_attrs[["y"]] <- svg_viewbox[2]
	mapBG_attrs[["width"]] <- svg_viewbox[3]
	mapBG_attrs[["height"]] <- svg_viewbox[4]
	xmlAttrs(mapBG) <- mapBG_attrs
	
	
	# originally removed the mapBG but force fill above works better
	#   for things like the github teaser
	# removeNodes(getNodeSet(tmap_svg,"//*[contains(@id,'mapBG')]")[[1]])

	if(is.null(width) || is.null(height)){
		width <- xmlAttrs(xmlRoot(tmap_svg))[["width"]]
		height <- xmlAttrs(xmlRoot(tmap_svg))[["height"]]
	}
	
	# ids in SVG likely to conflict if multiple maps on a page
	#   remove all ids from the XML
	xpathApply(
		tmap_svg
		,"//*[local-name() != 'clipPath']"
		,function(nod){
			removeAttributes(nod, "id")
		}
	)
	
 	svgPanZoom(
 		tmap_svg 
 		, viewportSelector = ".map_viewport"
 		, controlIconsEnabled = TRUE
 		, center = FALSE
 		, width = width
		, height = height
 	)
	
}


tmap2svg <- function(...) {
	warning("'tmap2svg' is renamed to the catchier 'itmap'")
	do.call("tmap2svg", list(...))
}
