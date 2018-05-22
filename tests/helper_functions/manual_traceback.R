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
