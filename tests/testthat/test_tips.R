context("tmap tips")

test_that("all tmap tips work", {
	f <- readLines(system.file("tips.txt", package="tmap"))
	fs <- split(f, cumsum(f==""))
	ntips <- length(fs)
	
	expect_output({
		for (i in 1:ntips) {
			suppressWarnings(rm(World, land, metro, rivers, NLD_muni, NLD_prov))
			tmap_tip()
		}
	})
})
