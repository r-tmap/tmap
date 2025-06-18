#' Overview of tmap layers
#'
#' Overview of tmap layers, organized by layer type.
#'
#' @export
#' @return A list of three layer types are returned: data layers, aux layers, and components.
tmap_overview = function() {

	modes = get_modes()
	tmap_graphics_name()

	om = get("tmapOptions", envir = .TMAP)$modes

	modes = names(om)

	gs = vapply(modes, function(m) om[[m]]$name, FUN.VALUE = character(1))

	funs = c("DataPlot", "AuxPrepare", "CompPrepare")
	nms = c("data_layer", "aux_layer", "component")
	rem = c("data_", "aux_", "")

	res = rbindlist(mapply(function(funi, remi, ni) {
		dt = data.table::rbindlist(mapply(function(g, nm) {
			ms = utils::methods(paste0("tmap", g, funi))
			df = attr(ms, "info")

			x = data.table::data.table(element = sub(paste0("^[^.]*\\.(tm_)?(", remi, ")?"), "\\1", rownames(df)),
									   type = ni,
						   package = sub("^[^.]*\\.", "\\.", df$from),
						   mode = nm)
			x[x$element != "default", ]
		}, gs, names(gs), SIMPLIFY = FALSE))

		wide_dt <- data.table::dcast(
			dt,
			element + type ~ mode,
			value.var = "package",fill = ""
		)
		data.table::setcolorder(
			wide_dt,
			c("element", "type", "plot", "view", sort(setdiff(names(wide_dt), c("element", "type", "plot", "view"))))
		)
	}, funs, rem, nms, SIMPLIFY = FALSE))
	as.data.frame(res)
}
