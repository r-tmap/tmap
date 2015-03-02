fns <- c("tm_shape", "tm_fill", "tm_borders", "tm_bubbles", "tm_lines", "tm_text", "tm_layout", "tm_grid", "tm_facets")
fns_prefix <- c("shape", "fill", "borders", "bubble", "line", "text", "layout", "grid", "facets")

#args <- lapply(fns, function(x)names(do.call(x, args=list(""))[[1]]))
args <- lapply(fns, function(x)names(formals(x)))

lst <- unlist(args)
lst_dup <- names(which(table(lst)>1))



args2 <- mapply(function(x, nm){
	dupl <- x %in% lst_dup
	x[dupl] <- paste(nm, x[dupl], sep=".")
	x
}, args, fns_prefix, SIMPLIFY=FALSE)


deparse(substitute(expression(tm_shape))



data(Europe)
data(rivers)
data(cities)

qtm(shp=Europe, fill = "pop_est", bubble.size = "pop_est", bubble.col = "green", projection="longlat", borders = "red", text="iso_a3", cex=3, title="Test qtm")
