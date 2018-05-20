context("facets")

run_facet_test <- function() {
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
	
	# test <- tmaptools::poly_to_raster(NLD_prov)
	# d <- st_set_geometry(NLD_prov, NULL)[test[], ]
	# d <- lapply(d, function(di) {
	# 	if (is.character(di)) as.factor(di) else di
	# })
	# test2 <- do.call(brick, lapply(1:5, function(i) {
	# 	tmp <- setValues(test, d[[i]])
	# 	if (is.factor(d[[i]])) as.factor(tmp) else tmp
	# }))
	
	
	
	tests <- list(
		list(layer = "polygons", args = list(col = "green")),
		list(layer = "polygons", args = list(col = "v1")),
		list(layer = "polygons", args = list(col = "v2")),
		list(layer = "lines", args = list(col = "green", lwd = 10)),
		list(layer = "lines", args = list(col = "v1", lwd = 10)),
		list(layer = "lines", args = list(col = "v2", lwd = 10)),
		list(layer = "lines", args = list(col = "blue", lwd = "v1", scale = 5)),
		list(layer = "lines", args = list(palette = "Set1", col = "v2", lwd = "v1", scale = 5)),
		list(layer = "symbols", args = list(col = "green")),
		list(layer = "symbols", args = list(col = "v1")),
		list(layer = "symbols", args = list(col = "v2")),
		list(layer = "symbols", args = list(col = "blue", size = "v1")),
		list(layer = "symbols", args = list(col = "red", shape = "v2")),
		list(layer = "text", args = list(col = "green", text = "name2")),
		list(layer = "text", args = list(col = "red", text = "name2",  size = "v1")),
		list(layer = "text", args = list(col = "v2", text = "name2"))
	)
	
	filter <- c(TRUE, TRUE) # extra option: if c(T, F) units 1, 3, ... are selected
	
	#test <- tests[[16]]
	testname <- function(test) paste0(test$layer, "_", unname(unlist(test$args))[1])
	
	errsL <- lapply(tests, function(test) {
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
		
		errs[errs$err != "", ]
	})
	
	
	
	names(errsL) <- sapply(tests, testname)
	
	edf <- do.call(rbind, errsL)
	
	saveRDS(edf, file = paste0("edf_", tmap_mode(), ".rds"))
	
	get_test <- function(tnr, i) {
		test <- tests[[tnr]]
		fun <- paste0("tm_", test$layer)
		
		settings <- list(drop.units = c(TRUE, FALSE),
						 free.coords = c(TRUE, FALSE),
						 free.scales = c(TRUE, FALSE),
						 drop.empty.facets = c(TRUE, FALSE),
						 showNA = c(TRUE, FALSE),
						 drop.NA.facets = c(TRUE, FALSE))
		shortcuts <- c("du", "fc", "fs", "de", "dn", "sn")
		comb <- do.call(expand.grid, c(settings, list(KEEP.OUT.ATTRS = FALSE)))
		
		cb <- as.list(comb[i,])
		name <- paste(mapply(paste0, shortcuts, c("F", "T")[as.numeric(unlist(cb)) + 1]), collapse = "_")
		
		cat("shp <- NLD_prov\n")
		if (test$layer == "lines") {
			cat("shp$geometry <- sf::st_cast(shp$geometry, \"MULTILINESTRING\", group_or_split = FALSE)\n")
		}
		if (fun == "tm_symbols") {
			cat("tm_shape(shp) + tm_borders() +\n")
		} else {
			cat("tm_shape(shp) + \n")	
		}
		
		anames <- names(test$args)
		aclass <- sapply(test$args, class)
		avalues <- unname(unlist(test$args))
		
		if (any(aclass=="character")) {
			avalues[aclass == "character"] <- paste0("\"", avalues[aclass == "character"], "\"")	
		}
		
		cat(paste0(fun, "(", paste(unlist(mapply(paste, anames, avalues, MoreArgs = list(sep = " = "), SIMPLIFY = FALSE)), collapse=", "), ")"), "+\n")
		cbnames <- names(cb)
		cbvalues <- unname(unlist(cb))
		cat(paste0("tm_facets(by = \"by\", ", paste(unlist(mapply(paste, cbnames, cbvalues, MoreArgs = list(sep = " = "), SIMPLIFY = FALSE)), collapse=", "), ")"), "\n")
	}
	
	trace_test <- function(ids, rev = FALSE) {
		settings <- list(drop.units = c(TRUE, FALSE),
						 free.coords = c(TRUE, FALSE),
						 free.scales = c(TRUE, FALSE),
						 drop.empty.facets = c(TRUE, FALSE),
						 showNA = c(TRUE, FALSE),
						 drop.NA.facets = c(TRUE, FALSE))
		shortcuts <- c("du", "fc", "fs", "de", "dn", "sn")
		comb <- do.call(expand.grid, c(settings, list(KEEP.OUT.ATTRS = FALSE)))
		
		if (rev) ids <- setdiff(1L:nrow(comb), ids)
		comb[ids, ]
	} 
	
	nrow(edf)
}

test_that("facets (all combinations, plot)", {
	tmap_mode("plot")

	nr <- run_facet_test()
	expect_equal(nr, expected = 0)
})

test_that("facets (all combinations, view)", {
	tmap_mode("view")
	tmap_options(limits = c(facets.view = 10, facets.plot = 64))
	
	nr <- run_facet_test()
	expect_equal(nr, expected = 0)
})

