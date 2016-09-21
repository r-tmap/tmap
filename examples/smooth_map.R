####################################
## Already smoothed raster
####################################
vol <- raster::raster(t(volcano[, ncol(volcano):1]), xmn=0, xmx=870, ymn=0, ymx=610)
vol_smooth <- smooth_map(vol, smooth.raster = FALSE, nlevels = 10)

tm_shape(vol_smooth$polygons) +
	tm_fill(palette=terrain.colors(11), title="Elevation") +
	tm_shape(vol_smooth$iso) +
	tm_iso(col = "black", size = .7, fontcolor="black") +
	tm_layout("Maunga Whau volcano (Auckland)", title.position=c("left", "bottom"), 
	    inner.margins=0) +
	tm_legend(width=.13, position=c("right", "top"), bg.color="gray80", frame = TRUE)
	
	
\dontrun{
####################################
## Smooth polygons
####################################
data(NLD_muni)

NLD_muni$population_dens <- calc_densities(NLD_muni, "population")

qtm(NLD_muni, fill="population_dens")

NLD_smooth <- smooth_map(NLD_muni, var = "population_dens")

qtm(NLD_smooth$raster, style="grey")
qtm(NLD_smooth$polygons, format="NLD")
	
####################################
## Smooth points
####################################

# Approximate world population density as spatial points, one for each 1 million people, 
# in the following way. Each metropolitan area of x million people will be represented 
# by x dots. The remaining population per country will be represented by dots that are 
# sampled across the country.
create_dot_per_1mln_people <- function() {
	data(World, metro)
	metro_eck <- set_projection(metro, projection = "eck4")
	
	# aggregate metropolitan population per country
	metro_per_country <- tapply(metro_eck$pop2010, INDEX = list(metro_eck$iso_a3), FUN=sum)
	metro_per_country_in_World <- metro_per_country[names(metro_per_country) %in% World$iso_a3]
	
	# assign to World shape
	World$pop_metro <- 0
	World$pop_metro[match(names(metro_per_country_in_World), World$iso_a3)] <- 
		metro_per_country_in_World
	
	# define population density other than metropolitan areas
	World$pop_est_dens_non_metro <- (World$pop_est - World$pop_metro) / World$area
	
	# generate dots for metropolitan areas (1 dot = 1mln people)
	metro_dots <- do.call("sbind", lapply(1:length(metro_eck), function(i) {
		m <- metro_eck[i,]
		m[rep(1, max(1, m$pop2010 %/% 1e6)),]
	}))
	
	# sample population dots from non-metropolitan areas (1 dot = 1mln people)
	World_pop <- sample_dots(World, vars="pop_est_dens_non_metro", w = 1e6, 
							 npop = 7.3e9 - length(metro_dots)*1e6)
	
	# combine 
	sbind(as(World_pop, "SpatialPoints"), as(metro_dots, "SpatialPoints"))
}

World_1mln_dots <- create_dot_per_1mln_people()


# dot map
tm_shape(World_1mln_dots) + tm_dots()

# create smooth map
World_list <- smooth_map(World_1mln_dots, cover = World, weight=1e6)

# plot smooth raster map
qtm(World_list$raster, style="grey")

# plot smooth raster map
qtm(World, bbox="India") + qtm(World_list$iso)

# plot kernel density map
qtm(World_list$polygons, style="grey", format="World")

####################################
## Smooth raster
####################################
data(land)

land_smooth <- smooth_map(land, var="trees", cover.type = "smooth")

qtm(land, raster="trees")
qtm(land_smooth$raster)
qtm(land_smooth$polygons, format="World", style="grey")
}
