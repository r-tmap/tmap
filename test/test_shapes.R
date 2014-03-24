#### Test Europe

data(Europe)
geo_choropleth(Europe, "gdp_cap_est", style="kmeans") + geo_borders(Europe) + geo_text(Europe, "iso_a3")

# Europe regions map

Europe$region <- ""
Europe$region[Europe$iso_a3 %in% c("CZE", "HUN", "LIE", "POL", "SVK", "BGR", "ROU", "RUS", "UKR", "BLR", "MDA")] <-
	"Eastern Europe"

Europe$region[Europe$iso_a3 %in% c("DNK", "EST", "FIN", "ISL", "LVA", "LTU", "NOR", "SWE", "IRL", "GBR", "ALA", "FRO", "IMN")] <-
	"Northern Europe"

Europe$region[Europe$iso_a3 %in% c("NLD", "AUT", "CHE", "DEU", "BEL", "LUX", "FRA", "GGY", "JEY", "MCO")] <-
	"Western Europe"

Europe$region[Europe$iso_a3 %in% c("PRT", "ESP", "ITA", "SVN", "ALB", "AND", "BIH", "GRC", "HRV", "Kosovo", "SMR", "SRB", "TUR", "VAT", "MKD", "MLT", "MNE")] <-
	"Southern Europe"

Europe$region <- factor(Europe$region)

geo_shape(Europe) +
	geo_borders() +
	geo_choropleth("region") +
	geo_text("iso_a3")

devtools::load_all("../../treemap/pkg")
TCdf <- treepalette(Europe@data, index= c("region", "iso_a3"), palette.HCL.options=list(hue_fraction=1))
Europe$col <- TCdf$HCL.color[match(Europe$iso_a3, TCdf$iso_a3)]

geo_shape(Europe) +
	geo_borders() +
	geo_fill(Europe$col) +
	geo_text("iso_a3")



#### Test world

geo_borders(world)
geo_borders(world) + geo_theme(draw.frame=TRUE, frame.lwd=NA)

geo_borders(world) +
	geo_choropleth(world, col="region_un") + geo_text(world, "iso_a3")

