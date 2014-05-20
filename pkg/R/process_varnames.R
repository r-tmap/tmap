process_varnames <- function(gp, nx) {
	varnamesList <- lapply(gp, function(x) x$varnames)
	fillVar <- lapply(varnamesList, function(x) x$choro.fill)
	sizeVar <- lapply(varnamesList, function(x) x$bubble.size)
	colVar <- lapply(varnamesList, function(x) x$bubble.col)
	lineVar <- lapply(varnamesList, function(x) x$line.col)
	
	fillId <- which(sapply(fillVar, function(x)!is.na(x[1])))
	sizeId <- which(sapply(sizeVar, function(x)!is.na(x[1])))
	colId <- which(sapply(colVar, function(x)!is.na(x[1])))
	lineId <- which(sapply(lineVar, function(x)!is.na(x[1])))
	
	list(choro.fill={if (length(fillId)) rep(fillVar[[fillId[1]]], 
											 length.out=nx) else NA},
		 bubble.size={if (length(sizeId)) rep(sizeVar[[sizeId[1]]], 
		 									 length.out=nx) else NA},
		 bubble.col={if (length(colId)) rep(colVar[[colId[1]]], 
		 								   length.out=nx) else NA},
		 line.col={if (length(lineId)) rep(lineVar[[lineId[1]]], 
		 								   length.out=nx) else NA}
	)
}
