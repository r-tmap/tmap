grob_mod <- function(grb, x.a=0, x.b=1, y.a=0, y.b=1) {
	if (inherits(grb, "gTree")) {
		grb$children <-  do.call(gList, lapply(grb$children, grob_mod, x.a=x.a, x.b=x.b, y.a=y.a, y.b=y.b))
	} else {
		nms <- names(grb)
		if ("x" %in% nms) grb$x <- unit_mod(grb$x, a=x.a, b=x.b)
		if ("y" %in% nms) grb$y <- unit_mod(grb$y, a=y.a, b=y.b)
		
		if ("width" %in% nms && !is.null(grb$width)) grb$width <- unit_mod(grb$width, b=x.b)
		if ("height" %in% nms && !is.null(grb$height)) grb$height <- unit_mod(grb$height, b=y.b)
	}
	grb
}

unit_mod <- function(unt, a=0, b=1) {
    if (getRversion() >= "4.0.0") {
        unitType <- get("unitType", envir=asNamespace("grid"))
        units <- unitType(unt)
        unit(as.numeric(unt)*b + a, units)
    } else {
        cls <- attr(unt, "unit")
        unit(as.numeric(unt)*b + a, cls)
    }
}
