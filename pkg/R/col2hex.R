col2hex <- function(x) apply(col2rgb(x), MARGIN=2, FUN=function(y)do.call(rgb, c(as.list(y), list(maxColorValue=255))))
