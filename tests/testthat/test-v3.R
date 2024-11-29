test_that("v3 syntax works", {
	skip_on_cran()


	expect_message(tm_shape(World) + tm_fill("darkolivegreen3") + tm_format("World", title = "A green World"))
	expect_message(tm_shape(World) + tm_polygons(fill = "darkolivegreen3", col = NA) + tm_format("World", title = "A green World"))


	# Data variable containing color values
	World$isNLD <-
		ifelse(World$name == "Netherlands",
			   "darkorange",
			   "darkolivegreen3")

	tm_shape(World, projection = "+proj=eck4") +
		tm_polygons("economy", title = "Economy", id = "name") +
		tm_text("iso_a3", size = "AREA", scale = 1.5) +
		tm_format("World")

	# Numeric data variable
	tm_shape(World, projection = "+proj=eck4") +
		tm_polygons(
			"HPI",
			palette = "RdYlGn",
			style = "cont",
			n = 8,
			title = "Happy Planet Index",
			id = "name"
		) +
		tm_text("iso_a3", size = "AREA", scale = 1.5) +
		tm_style("grey") +
		tm_format("World")

	data(NLD_prov, NLD_muni)
	# Map coloring algorithm
	tm_shape(NLD_prov) +
		tm_fill("name", legend.show = FALSE) +
		tm_shape(NLD_muni) +
		tm_polygons("MAP_COLORS", palette = "Grays", alpha = .25) +
		tm_shape(NLD_prov) +
		tm_borders(lwd = 2) +
		tm_text("name", shadow = TRUE) +
		tm_format("NLD", title = "Dutch provinces and\nmunicipalities", bg.color =
				  	"white")

})

test_that("Possible to revert to v3 styling.", {
	skip_on_cran()
	World$HPI[1:10] = NA
	tm_shape(World) + tm_polygons("economy", style = "cat")
	current_style <- tmap_style("v3")
	tm_shape(World) + tm_polygons("economy", style = "cat") %>% expect_no_error()
	tm_shape(World) + tm_polygons("HPI", style = "cont") %>% expect_no_error()
	expect_message(tm_shape(World) + tm_polygons("HPI",
									  style = "fixed",
									  breaks = c(0, 20, 35, 42, 50)))
	expect_message(tm_shape(World) + tm_polygons("HPI", style = "sd"))
	expect_message(tm_shape(World) + tm_polygons("HPI", style = "equal"))
	expect_message(tm_shape(World) + tm_polygons("HPI", style = "pretty"))
	expect_message(tm_shape(World) + tm_polygons("HPI", style = "quantile"))
	expect_message(tm_shape(World) + tm_polygons("HPI", style = "kmeans"))
	expect_message(tm_shape(World) + tm_polygons("HPI", style = "hclust"))
	expect_message(tm_shape(World) + tm_polygons("HPI", style = "bclust"))
	expect_message(tm_shape(World) + tm_polygons("HPI", style = "fisher"))
	expect_message(tm_shape(World) + tm_polygons("HPI", style = "jenks"))
	expect_message(tm_shape(World) + tm_polygons("HPI", style = "dpih"))
	expect_message(tm_shape(World) + tm_polygons("HPI", style = "headtails"))
	expect_message(tm_shape(World) + tm_polygons("HPI", style = "log10"))
	tmap_style(current_style)
})

test_that("log10_pretty and order styles work", {
	skip_on_cran()
	expect_no_error(expect_message(tm_shape(World) + tm_polygons("HPI", style = "log10_pretty")))
	expect_no_error(expect_message(tm_shape(World) + tm_polygons("HPI", style = "order")))

})

test_that("v3 works", {
	skip_on_cran()
	World$isNLD <- ifelse(World$name=="Netherlands", "darkorange", "darkolivegreen3")

	expect_message({tm_shape(World) +
		tm_fill("isNLD") +
		tm_layout("Find the Netherlands!")})
})

test_that("v3 legends work", {
	skip_on_cran()
	expect_message(
		tm_shape(World) +
			tm_fill("income_grp") +
			tm_add_legend(type = "fill", title = "hello", labels = c("1", "2", "3"), col = "blue", border.col = "black")
	)

	expect_message(
		tm_shape(World) +
			tm_fill("income_grp", border.alpha = 0.5)
	)
})

test_that("title size works with many titles.", {
	skip_on_cran()
	# Example to illustrate the type of titles
	# Brought over to make examples work.
	# The failing test can be resolved later.
	# the problem is still there for many titles.
	# Many titles now work
	expect_no_error(tm_shape(World) +
		tm_polygons(c("income_grp", "economy"), title = c("Legend Title 1", "Legend Title 2")) +
		tm_layout(main.title = "Main Title",
				  main.title.position = "center",
				  main.title.color = "blue",
				  title = c("Title 1", "Title 2"),
				  title.color = "red",
				  panel.labels = c("Panel Label 1", "Panel Label 2"),
				  panel.label.color = "purple",
				  legend.text.color = "brown"))
})

test_that("convert2density is deprecated", {
	skip_on_cran()
	expect_message(
		tm_shape(NLD_muni) + tm_polygons("population", convert2density = TRUE),
		"convert2density"
	)
})
