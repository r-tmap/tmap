library(sf)
library(grid)

data(NLD_prov)

NLD_prov <- st_sf(name = as.character(NLD_prov$name), geometry=NLD_prov$geometry, stringsAsFactors = FALSE)
NLD_prov$by <- factor(rep(c(NA,2,3, 4), each = 3), levels=1:4, labels = letters[1:4])
NLD_prov$v1 <- c(9, 8, 3, 7, 8, NA, NA, NA, NA, 3, 5, NA)
NLD_prov$v2 <- c("x", "y", NA, NA, NA, NA, "x", "y", "x", "y", NA, NA)
NLD_prov$name2 <- NLD_prov$name
NLD_prov$name[c(5, 7, 8, 9)] <- NA



tests <- list(
	list(layer = "polygons", args = list(col = "green")),
	list(layer = "polygons", args = list(col = "v1")),
	list(layer = "polygons", args = list(col = "v2")),
	list(layer = "lines", args = list(col = "green", lwd = 10)),
	list(layer = "lines", args = list(col = "v1", lwd = 10)),
	list(layer = "lines", args = list(col = "v2", lwd = 10)),
	list(layer = "lines", args = list(col = "blue", lwd = "v1", scale = 5)),
	list(layer = "symbols", args = list(col = "green")),
	list(layer = "symbols", args = list(col = "v1")),
	list(layer = "symbols", args = list(col = "v2")),
	list(layer = "symbols", args = list(col = "blue", size = "v1")),
	list(layer = "symbols", args = list(col = "red", shape = "v2"))
)

test <- tests[[7]]

filter <- c(TRUE, TRUE) # extra option: if c(T, F) units 1, 3, ... are selected

errsL <- list()

for (test in tests[1:7]) {
	shp <- NLD_prov
	if (test$layer == "lines") {
		shp$geometry <- sf::st_cast(shp$geometry, "MULTILINESTRING", group_or_split = FALSE)
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
	errs <- data.frame(i = 1:nrow(comb),
					   err = character(nrow(comb)),
					   stringsAsFactors = FALSE)
	for (i in 1:nrow(comb)) {
		setTxtProgressBar(pb, i)
		cb <- as.list(comb[i,])
		name <- paste(mapply(paste0, shortcuts, c("F", "T")[as.numeric(unlist(cb)) + 1]), collapse = "_")
		
		errs[i, 2] <- tryCatch({
			print(tm_shape(shp, filter = filter) +
				  	do.call(fun, test$args) +
				  	do.call(tm_facets, c(list(by="by"), cb)) +
				  	tm_layout(title=name)
			)
			""
		}, error=function(e) {
			#		grid.newpage()
			grid::upViewport(0)
			grid.text(y = .8, label = name)
			grid.text(y = .2, label = e)
			as.character(e)
		}, warning = function(w) {
			as.character(w)
		})
	}
	dev.off()
	
	errs <- errs[errs$err != "", ]
	
	errs2 <- list(errs)
	names(errs2) <- filename
	errsL <- c(errsL, errs2)
}


edf <- do.call(rbind, errsL)
View(edf)



shp <- NLD_prov
shp$geometry <- sf::st_cast(shp$geometry, "MULTILINESTRING", group_or_split = FALSE)
tm_shape(shp) +
	tm_lines("blue", lwd = "v2", scale = 5, lwd.legend = 1:10) +
	tm_facets(by = "by", drop.units = T, free.coords = T, free.scales = F, drop.empty.facets = F,  showNA = F, drop.NA.facets = T)


shp <- NLD_prov
tm_shape(shp) +
	tm_polygons(col="v1") +
	tm_facets(by = "by", drop.units = T, free.coords = F, free.scales = T, drop.empty.facets = F,  showNA = F, drop.NA.facets = F)


shp <- NLD_prov
shp$v3 <- letters[1:12]
shp$v2b <- "green"
shp$v2b[is.na(shp$v2)] <- NA
tm_shape(shp, filter = c(TRUE, F)) +
	tm_polygons(col="v2") +
	tm_facets(by = "by", drop.units = T, free.coords = F, free.scales = F, drop.empty.facets = T,  showNA = F, drop.NA.facets = T)

shp$by2 <- factor("a", levels=letters[1:3])
shp$v4 <- 1:12

tm_shape(shp, filter = c(TRUE, F)) +
	tm_polygons(col="v4") +
	tm_facets(by = "by2", drop.units = T, free.coords = F, free.scales = F, drop.empty.facets = F,  showNA = F, drop.NA.facets = F)



shp <- NLD_prov
shp$v3 <- letters[1:12]
shp$v2b <- "green"
shp$v2b[is.na(shp$v2)] <- NA
tm_shape(shp, filter = c(TRUE, T)) +
	tm_symbols(col="v2b") +
	tm_facets(by = "by", drop.units = T, free.coords = F, free.scales = F, drop.empty.facets = F,  showNA = F, drop.NA.facets = F)

