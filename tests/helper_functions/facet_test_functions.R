testname <- function(test) paste0(test$layer, "_", unname(unlist(test$args))[1])

run_facet_test <- function(test) {
	library(sf)
	library(grid)
	library(raster)
	
	data(NLD_prov)
	
	NLD_prov <- st_sf(name = as.character(NLD_prov$name), geometry=NLD_prov$geometry, stringsAsFactors = FALSE)
	NLD_prov$by <- factor(rep(c(NA,2,3, 4), each = 3), levels=1:4, labels = letters[1:4])
	NLD_prov$v1 <- c(9, 8, 3, 7, 8, NA, NA, NA, NA, 3, 5, NA)
	NLD_prov$v2 <- c("x", "y", NA, NA, NA, NA, "x", "y", "x", "y", NA, NA)
	NLD_prov$name2 <- NLD_prov$name
	NLD_prov$name[c(5, 7, 8, 9)] <- NA
	
	filter <- c(TRUE, TRUE) # extra option: if c(T, F) units 1, 3, ... are selected
	
	#test <- tests[[16]]
	

	shp <- NLD_prov
	if (test$layer == "lines") {
		shp$geometry <- sf::st_cast(shp$geometry, "MULTILINESTRING", group_or_split = FALSE)
	}
	
	dir.create("output", showWarnings = FALSE)
	filename <- paste0("output/test_", test$layer, "_", unname(unlist(test$args))[1], ".pdf")
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
	
	cat("\ntest:", testname(test), "\n")
	pb <- txtProgressBar(min = 1, max = nrow(comb), initial = 1)
	errs <- data.frame(i = 1:nrow(comb),
					   err = character(nrow(comb)),
					   stringsAsFactors = FALSE)
	for (i in 1:nrow(comb)) {
		setTxtProgressBar(pb, i)
		cb <- as.list(comb[i,])
		name <- paste(mapply(paste0, shortcuts, c("F", "T")[as.numeric(unlist(cb)) + 1]), collapse = "_")
		
		errs[i, 2] <- tryCatch({
			tm <- tm_shape(shp, filter = filter)
			if (fun == "tm_symbols") tm <- tm + tm_borders()	
			print(tm +
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
	
	name <- testname(test)
	
	filename <- paste0("output/results_", name, ".rds")
	saveRDS(errs, file = filename)

	nr <- nrow(errs)
	
	if (nr!=0) {
		cat(name, paste(errs$i, collapse =", "), "\n")
	}
	
	nr
}
