## v3 functions and arguments
library(remotes)
install_github("r-tmap/tmap@v3")

library(tmap)

get_tmap_funs = function() {
	nms = getNamespaceExports("tmap")
	funs = lapply(nms, function(f) {
		g = get(f)
		if (is.function(g)) {
			x = setdiff(names(do.call(formals, list(g))), "...")
			if (is.null(x)) x = character()
			x
		} else {
			NULL
		}
	})
	names(funs) = nms
	funs[!sapply(funs, is.null)]
}

funs_v3 = get_tmap_funs()

funs_v3$tm_polygons = unique(c(funs_v3$tm_fill, funs_v3$tm_polygons, funs_v3$tm_borders))


## v4 functions and arguments
devtools::load_all()

funs_v4 = get_tmap_funs()
usethis::use_data(funs_v3, funs_v4, internal = TRUE, overwrite = TRUE)









