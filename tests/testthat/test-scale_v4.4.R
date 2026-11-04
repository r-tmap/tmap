# The numeric scales were made consistent in tmap 4.5. The style "v4.4" reproduces the numeric scales of tmap 4.0
# to 4.4. The reference values below were computed with tmap 4.4.

scale_cases = function() {
	x = c(0, 100, 400, 2500, 10000, 7000, 300)
	xi = c(1L, 2L, 3L, 3L, 4L, 2L, 1L)
	xf = factor(c("a", "b", "c", "a", "b", "c", "a"))
	list(
		list("lwd", "lines", x, tm_scale_continuous()),
		list("lwd", "lines", x, tm_scale_intervals(n = 4, style = "equal")),
		list("lwd", "lines", x, tm_scale_intervals(n = 4, style = "equal", values = c(0.5, 4))),
		list("lwd", "lines", xi, tm_scale_discrete()),
		list("lwd", "lines", xf, tm_scale_categorical()),
		list("size", "symbols", x, tm_scale_continuous()),
		list("size", "bubbles", x, tm_scale_continuous()),
		list("size", "symbols", x, tm_scale_intervals(n = 4, style = "equal")),
		list("size", "symbols", x + 1, tm_scale_continuous_log10()),
		list("fill_alpha", "polygons", x, tm_scale_intervals(n = 4, style = "equal")),
		list("fill_alpha", "polygons", x, tm_scale_continuous()),
		list("col_alpha", "lines", x, tm_scale_continuous())
	)
}

scale_values = function(style = NULL) {
	o = tmap_options_mode("plot", style = style, mode.specific = FALSE)
	leg = modifyList(tm_legend(), list(reverse = FALSE, na.show = NA))
	lapply(scale_cases(), function(cs) {
		fun = switch(class(cs[[4]])[1],
					 tm_scale_intervals = tmapScaleIntervals,
					 tm_scale_discrete = tmapScaleDiscrete,
					 tm_scale_categorical = tmapScaleCategorical,
					 tmapScaleContinuous)
		r = suppressMessages(fun(cs[[3]], cs[[4]], leg, tm_chart_none(), o, cs[[1]], cs[[2]], list(), sortRev = NULL,
								 bypass_ord = FALSE, submit_legend = FALSE))
		list(vals = round(r$vals, 4), legend = round(unlist(r$legend$vvalues), 4))
	})
}

ref_v4.4 = list(
	"lwd lines tm_scale_continuous" = list(vals = c(0, 0.01, 0.04, 0.25, 1, 0.7, 0.03),
		legend = c(0.2, 0.4, 0.6, 0.8, 1)),
	"lwd lines tm_scale_intervals" = list(vals = c(0, 0, 0, 1, 3, 2, 0),
		legend = c(0, 1, 2, 3)),
	"lwd lines tm_scale_intervals values" = list(vals = c(0.5, 0.5, 0.5, 1.6667, 4, 2.8333, 0.5),
		legend = c(0.5, 1.6667, 2.8333, 4)),
	"lwd lines tm_scale_discrete" = list(vals = c(0, 1, 2, 2, 3, 1, 0),
		legend = c(0, 1, 2, 3)),
	"lwd lines tm_scale_categorical" = list(vals = c(0, 1.5, 3, 0, 1.5, 3, 0),
		legend = c(0, 1.5, 3)),
	"size symbols tm_scale_continuous" = list(vals = c(0, 0.1, 0.2, 0.5, 1, 0.8367, 0.1732),
		legend = c(0.4472, 0.6325, 0.7746, 0.8944, 1)),
	"size bubbles tm_scale_continuous" = list(vals = c(0, 0.1333, 0.2667, 0.6666, 1.3333, 1.1155, 0.2309),
		legend = c(0.5963, 0.8433, 1.0328, 1.1925, 1.3333)),
	"size symbols tm_scale_intervals" = list(vals = c(0, 0, 0, 0.5774, 1, 0.8165, 0),
		legend = c(0, 0.5774, 0.8165, 1)),
	"size symbols tm_scale_continuous_log10" = list(vals = c(0, 0.7079, 0.8067, 0.9217, 1, 0.9804, 0.7872),
		legend = c(0, 0.5, 0.7071, 0.866, 1)),
	"fill_alpha polygons tm_scale_intervals" = list(vals = c(0.25, 0.25, 0.25, 0.5, 1, 0.75, 0.25),
		legend = c(0.25, 0.5, 0.75, 1)),
	"fill_alpha polygons tm_scale_continuous" = list(vals = c(0, 0.01, 0.04, 0.25, 1, 0.7, 0.03),
		legend = c(0.2, 0.4, 0.6, 0.8, 1)),
	"col_alpha lines tm_scale_continuous" = list(vals = c(0, 0.01, 0.04, 0.25, 1, 0.7, 0.03),
		legend = c(0.2, 0.4, 0.6, 0.8, 1))
)

test_that("style v4.4 reproduces the numeric scales of tmap 4.4", {
	expect_equal(unname(scale_values("v4.4")), unname(ref_v4.4))
	# combined with another style
	expect_equal(unname(scale_values(c("cobalt", "v4.4"))), unname(ref_v4.4))
	expect_equal(unname(scale_values("cobalt+v4.4")), unname(ref_v4.4))
	# the v3 styles also include it
	expect_equal(unname(scale_values("v3")), unname(ref_v4.4))
})

test_that("numeric scales are consistent", {
	res = setNames(scale_values(), names(ref_v4.4))
	# the same range for continuous and classed scales
	expect_equal(max(res[["lwd lines tm_scale_continuous"]]$vals), 3)
	expect_equal(res[["lwd lines tm_scale_intervals"]]$legend, c(0.75, 1.5, 2.25, 3))
	expect_equal(res[["lwd lines tm_scale_discrete"]]$legend, c(0.75, 1.5, 2.25, 3))
	expect_equal(res[["lwd lines tm_scale_categorical"]]$legend, c(1, 2, 3))
	# classes of size: k/n of the area
	expect_equal(res[["size symbols tm_scale_intervals"]]$legend, round(sqrt((1:4) / 4), 4))
	# sequences that do not start at 0: evenly spaced from the first to the last value
	expect_equal(res[["lwd lines tm_scale_intervals values"]]$legend, c(0.5, 1.6667, 2.8333, 4))
	expect_equal(res[["fill_alpha polygons tm_scale_continuous"]]$vals[c(1, 5)], c(0.25, 1))
})

test_that("tm_seq gives the first and last visual values", {
	expect_equal(tmapSeq(tm_seq(0.5, 1, "sqrt"), 3), sqrt(c(0.25, 0.625, 1)))
	expect_equal(tmapSeq(tm_seq(0, 3), 4), 0:3)
	# values.range is relative: the part of the curve that is used
	expect_equal(tmapSeq(tm_seq(0, 3), 4, range = c(0.5, 1)), seq(1.5, 3, length.out = 4))
})

test_that("styles can be combined", {
	expect_error(tm_style(c("cobalt", "nonexisting")), "nonexisting")
	o = tmap_options_mode("plot", style = c("cobalt", "v4.4"), mode.specific = FALSE)
	o_cobalt = tmap_options_mode("plot", style = "cobalt", mode.specific = FALSE)
	expect_equal(o$bg.color, o_cobalt$bg.color)
	expect_equal(o$values.range$lwd, c(0, 1))
})
