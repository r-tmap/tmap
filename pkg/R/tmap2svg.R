tmap2svg <- function(tm, fill=NULL) {
	tmp <- tempfile()
	png(tmp, 
		width = convertWidth(unit(1,"npc"), "points", valueOnly = TRUE),
		height = convertHeight(unit(1,"npc"), "points", valueOnly = TRUE)
	)
	res <- print(tm)
	
	
	
	
	hover_text <- paste(World$name, "\nPopulation=", World$pop_est)
	
	lapply(
		seq.int(1,length(World@data$name))
		,function(n){
			grid.garnish(
				paste0("tm_polygons_1_",n)
				, title=hover_text[n]
				, onmouseover="this.setAttribute('opacity', '0.5');"
				, onmouseout="this.setAttribute('opacity', '1');"
				, group = TRUE
			)
			grid.get(paste0("tm_polygons_",n))
		}
	)
	
	grid.garnish("tm_bubbles_2_1", title=c("test123", "test321"), group=FALSE)
	
	grid.export("../test/test2.svg")
	
	
	
	dev.off()
}