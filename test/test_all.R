library(sf)
library(grid)

data(NLD_prov)

NLD_prov <- st_sf(name = as.character(NLD_prov$name), geometry=NLD_prov$geometry, stringsAsFactors = FALSE)
NLD_prov$by <- factor(rep(c(NA,2,3, 4), each = 3), levels=1:5, labels = letters[1:5])
NLD_prov$v1 <- c(9, 8, 3, 7, 8, NA, NA, NA, NA, 3, 5, NA)
NLD_prov$v2 <- c("x", "y", NA, NA, NA, NA, "x", "y", "x", "y", NA, NA)
NLD_prov$name[c(5, 7, 8, 9)] <- NA


tests <- list(
	list(layer = "polygons", args = list(col = "green")),
	list(layer = "polygons", args = list(col = "v1")),
	list(layer = "polygons", args = list(col = "v2")),
	list(layer = "lines", args = list(col = "green")),
	list(layer = "lines", args = list(col = "v1")),
	list(layer = "lines", args = list(col = "v2"))
)

test <- tests[[5]]

for (test in tests) {
	if (test$layer == "lines") {
		NLD_prov$geometry <- sf::st_cast(NLD_prov$geometry, "MULTILINESTRING", group_or_split = FALSE)
	}
	
	filename <- paste0("test_", test$layer, "_", unname(unlist(test$args))[1], ".pdf")
	fun <- paste0("tm_", test$layer)
	
	pdf(filename, width = 7, height = 7)
	
	settings <- list(drop.units = c(TRUE, FALSE),
					 free.coords = c(TRUE, FALSE),
					 free.scales = c(TRUE, FALSE),
					 drop.empty.facets = c(TRUE, FALSE),
					 showNA = c(TRUE, FALSE),
					 drop.NA.facets = c(TRUE, FALSE))
	shortcuts <- c("du", "fc", "fs", "de", "dn", "sn")
	comb <- do.call(expand.grid, c(settings, list(KEEP.OUT.ATTRS = FALSE)))
	
	
	# comb[c(44,60), ]
	
	pb <- txtProgressBar(min = 1, max = nrow(comb), initial = 1)
	
	for (i in 1:nrow(comb)) {
		setTxtProgressBar(pb, i)
		cb <- as.list(comb[i,])
		name <- paste(mapply(paste0, shortcuts, c("F", "T")[as.numeric(unlist(cb)) + 1]), collapse = "_")
		
		tryCatch({
			print(tm_shape(NLD_prov) +
				  	do.call(fun, test$args) +
				  	do.call(tm_facets, c(list(by="by"), cb)) +
				  	tm_layout(title=name)
			)
			
		}, error=function(e) {
			#		grid.newpage()
			grid::upViewport(0)
			grid.text(y = .8, label = name)
			grid.text(y = .2, label = e)
		})
	}
	dev.off()
}





# 
# 
# tm_shape(NLD_prov) +
# 	tm_polygons("green") +
# 	tm_facets(by = "by", drop.units = T, free.coords = T, drop.empty.facets = T, drop.NA.facets = T, showNA = T, free.scales = T)
# 
# 
# tm_shape(NLD_prov) +
# tm_polygons("v1") +
# tm_facets(by = "by", drop.units = T, free.coords = T, free.scales = F, drop.empty.facets = F, drop.NA.facets = T, showNA = T)

tm_shape(NLD_prov) +
	tm_lines("v1") +
	tm_facets(by = "by", drop.units = F, free.coords = F, free.scales = T, drop.empty.facets = F,  showNA = T, drop.NA.facets = F)

