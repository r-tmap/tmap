#' Overview of tmap layers
#'
#' Overview of tmap layers, organized by layer type.
#'
#' @export
#' @return A list of three layer types are returned: data layers, aux layers, and components.
tmap_overview = function() {

	modes = tmap:::get_modes()
	tmap_graphics_name()

	om = get("tmapOptions", envir = .TMAP)$modes

	modes = names(om)

	gs = vapply(modes, function(m) om[[m]]$name, FUN.VALUE = character(1))

	library(data.table)

	funs = c("CompPrepare", "AuxPrepare")
	nms = c("components", "aux_layers")
	rem = c("", "aux_")

	res = mapply(function(funi, remi) {
		dt = rbindlist(mapply(function(g, nm) {
			ms = utils::methods(paste0("tmap", g, funi))
			df = attr(ms, "info")


			x = data.table(layer = sub(paste0("^[^.]*\\.(tm_)?(", remi, ")?"), "\\1", rownames(df)),
						   package = sub("^[^.]*\\.", "\\.", df$from),
						   mode = nm)
			x[x$layer != "default", ]
		}, gs, names(gs), SIMPLIFY = FALSE))

		wide_dt <- dcast(
			dt,
			layer ~ mode,
			value.var = "package",fill = ""
		)
		setcolorder(
			wide_dt,
			c("layer", "plot", "view", sort(setdiff(names(wide_dt), c("layer", "plot", "view"))))
		)
		res = as.data.frame(wide_dt)
	}, funs, rem, SIMPLIFY = FALSE)
	names(res) = nms
	res
}
