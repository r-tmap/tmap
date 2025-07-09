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

	# Helper to get package
	find_env <- function(generic, class) {
		# Get the actual method
		fun <- tryCatch(
			getS3method(generic, class),
			error = function(e) NULL
		)
		if (is.null(fun)) return(NA)
		# Get its environment
		env <- environment(fun)
		if (isNamespace(env)) {
			return(unname(getNamespaceName(env)))
		} else {
			return(unname(as.character(env)))
		}
	}

	res = rbindlist(mapply(function(funi, remi, ni) {
		dt = data.table::rbindlist(mapply(function(g, nm) {
			guni = paste0("tmap", g, funi)
			ms = utils::methods(guni)
			df = attr(ms, "info")

			cls = sub("^[^.]*\\.", "", rownames(df))
			x = data.table::data.table(element = sub(paste0("^[^.]*\\.(tm_)?(", remi, ")?"), "\\1", rownames(df)),
									   type = ni)
			x$package = vapply(cls, function(x) {
				find_env(guni, x)
			}, FUN.VALUE = character(1))

			x$mode = nm
			x[x$element != "default", ]
		}, gs, names(gs), SIMPLIFY = FALSE))

		wide_dt <- data.table::dcast(
			dt,
			element + type ~ mode,
			value.var = "package",fill = "----"
		)
		data.table::setcolorder(
			wide_dt,
			c("element", "type", "plot", "view", sort(setdiff(names(wide_dt), c("element", "type", "plot", "view"))))
		)
	}, funs, rem, nms, SIMPLIFY = FALSE))
	as.data.frame(res)
}
