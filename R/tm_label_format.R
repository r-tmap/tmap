#' tmap function to specify labels
#'
#' tmap function to specify labels used in the scale functions, e.g. via the argument `label.format` in [tm_scale_intervals()].
#'
#' @param fun Function to specify the labels. It should take a numeric vector, and should return a character vector of the same size. By default it is not specified. If specified, the list items \code{scientific}, \code{format}, and \code{digits} (see below) are not used.
#' @param scientific Should the labels be formatted scientifically? If so, square brackets are used, and the \code{format} of the numbers is \code{"g"}. Otherwise, \code{format="f"}, and \code{text.separator}, \code{text.less.than}, and \code{text.or.more} are used. Also, the numbers are automatically  rounded to millions or billions if applicable. By default, \code{FALSE}
#' @param format By default, \code{"f"}, i.e. the standard notation \code{xxx.xxx}, is used. If \code{scientific = TRUE} then \code{"g"}, which means that numbers are formatted scientifically, i.e. \code{n.dddE+nn} if needed to save space.
#' @param digits Number of digits after the decimal point if \code{format="f"}, and the number of significant digits otherwise. By default \code{NA}, meaning as many as needed to have distinct numbers
#' @param interval.disjoint In case of intervals (see [tm_scale_intervals()]), should the intervals appear disjoint, e.g. \code{0 to 999}, \code{1000 - 1999}, \code{2000 - 2999} (\code{TRUE}, default), or not: \code{0 - 1000}, \code{1000 - 2000}, \code{2000- 3000}.
#' @param big.num.abbr Vector that defines whether and which abbrevations are used for large numbers. It is a named numeric vector, where the name indicated the abbreviation, and the number the magnitude (in terms on numbers of zero). Numbers are only abbrevation when they are large enough. Set it to \code{NA} to disable abbrevations.  The default is \code{c("mln" = 6, "bln" = 9)}. For layers where \code{style} is set to \code{log10} or \code{log10_pretty}, the default is \code{NA}.
#' @param prefix Prefix of each number
#' @param suffix Suffix of each number
#' @param text.separator Character string to use to separate numbers in an interval legend (default: \code{"to"}).
#' @param text.less.than Character value(s) to use for 'less than'. Default \code{"Less than"}. When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}
#' @param text.less.than_as.prefix Should \code{text.less.than} be used as prefix?
#' @param text.or.more Character value(s) to use to  'or more'. Default is \code{"or more"}. When a character vector of length 2 is specified, one for each word, these words are aligned when \code{text.to.columns = TRUE}
#' @param text.or.more_as.prefix Should \code{text.or.more} be used as prefix?
#' @param text.align Not implemented in v4 (yet). Value that determines how the numbers are aligned, \code{"left"}, \code{"center"} or \code{"right"}. By default \code{"left"}.
#' @param text.to.columns Not implemented in v4 (yet). Logical that determines whether the text is aligned to three columns (from, text.separator, to). By default \code{FALSE}.
#' @param html.escape Logical that determins whther HTML code is escaped in the popups in view mode. By default \code{TRUE}. If set to \code{FALSE} HTML code can be added, e.g. to added white space via \code{&nbsp;}.
#' @param ... arguments passed on to \code{\link[base:formatC]{formatC}}
#' @return list with formatting options
#' @export
tm_label_format = function(fun,
						   scientific,
						   format,
						   digits,
						   interval.disjoint,
						   big.num.abbr,
						   prefix,
						   suffix,
						   text.separator,
						   text.less.than,
						   text.less.than_as.prefix,
						   text.or.more,
						   text.or.more_as.prefix,
						   text.align,
						   text.to.columns,
						   html.escape,
						   ...) {

	args = lapply(as.list(rlang::call_match(dots_expand = TRUE)[-1]), eval, envir = parent.frame())
	args$called = names(args)

	structure(args, class = c("tmapLabelFormat", "list"))
}
