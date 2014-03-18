#### Test europe


geo.choropleth(europe, "gdp_cap_est", style="kmeans") + geo.borders(europe) + geo.text(europe, "iso_a3")

# europe regions map

europe$region <- ""
europe$region[europe$iso_a3 %in% c("CZE", "HUN", "LIE", "POL", "SVK", "BGR", "ROU", "RUS", "UKR", "BLR", "MDA")] <-
	"Eastern europe"

europe$region[europe$iso_a3 %in% c("DNK", "EST", "FIN", "ISL", "LVA", "LTU", "NOR", "SWE", "IRL", "GBR", "ALA", "FRO", "IMN")] <-
	"Northern europe"

europe$region[europe$iso_a3 %in% c("NLD", "AUT", "CHE", "DEU", "BEL", "LUX", "FRA", "GGY", "JEY", "MCO")] <-
	"Western europe"

europe$region[europe$iso_a3 %in% c("PRT", "ESP", "ITA", "SVN", "ALB", "AND", "BIH", "GRC", "HRV", "Kosovo", "SMR", "SRB", "TUR", "VAT", "MKD", "MLT", "MNE")] <-
	"Southern europe"

europe$region <- factor(europe$region)

geo.borders(europe) +
	geo.choropleth(europe, "region") +
	geo.text(europe, "iso_a3")

devtools::load_all("../../treemap/pkg")
TCdf <- treepalette(europe@data, index= c("region", "iso_a3"))
europe$col <- TCdf$HCL.color[match(europe$iso_a3, TCdf$iso_a3)]

geo.borders(europe) +
	geo.fill(europe, europe$col) +
	geo.text(europe, "iso_a3")


#### Test world

geo.borders(world)
geo.borders(world) + geo.theme(draw.frame=TRUE, frame.lwd=NA)

geo.borders(world) +
	geo.choropleth(world, col="region_un") + geo.text(world, "iso_a3")

