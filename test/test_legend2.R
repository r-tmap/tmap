data(Europe)


geo_shape(Europe) +
	geo_borders() +
	geo_fill("gdp_cap_est") +
	geo_bubbles("gdp_md_est", palette="Set2", col="part", scale=2) + 
	geo_theme_Europe()

geo_shape(Europe) +
	geo_borders() +
	geo_fill("gdp_cap_est") +
	geo_theme_Europe("", legend.titles="")



geo_shape(Europe) +
	geo_borders() +
	geo_fill("gdp_cap_est") +
	geo_bubbles("gdp_md_est", palette="Set2", col="part", scale=2) + 
	geo_theme_Europe(legend.titles=c(fill="GDP per capita"))


geo_shape(Europe) +
	geo_borders() +
	geo_fill("gdp_cap_est") +
	geo_bubbles("gdp_md_est", palette="Set2", col="part", scale=2) + 
	geo_theme_Europe("GDP per capita")


geo_shape(Europe) +
	geo_borders() +
	geo_fill("gdp_cap_est") +
	geo_bubbles("gdp_md_est", palette="Set2", col="part", scale=2) + 
	geo_theme_Europe(legend.titles=c(fill="GDP per capita", bubble.col="Part of Europe"))

geo_shape(Europe) +
	geo_borders() +
	geo_fill("gdp_cap_est") +
	geo_bubbles("gdp_md_est", palette="Set2", col="part", scale=2) + 
	geo_theme_Europe(legend.titles=c(fill="GDP per capita", bubble.size="GPD per country", bubble.col="Part of Europe"))


geo_shape(Europe) +
	geo_borders() +
	geo_fill("gdp_cap_est") +
	geo_bubbles("gdp_md_est", palette="Set2", col="part", scale=2) + 
	geo_theme_Europe(legend.titles=c(fill="GDP per capita", bubble.size="GPD per country"), legend.config=c("bubble.col", "bubble.size", "fill"))
					 
geo_shape(Europe) +
	geo_fill() +
	geo_theme("test")
					 

					 
geo_shape(Europe) +
	geo_borders() +
	geo_fill("gdp_cap_est") +
	geo_bubbles("gdp_md_est", palette="Set2", col=c("part", "economy"), scale=2) + 
	geo_theme_Europe(legend.titles=list(fill=c("GDP per capita", "xx"), bubble.col="Part of Europe"))

geo_shape(Europe) +
	geo_borders() +
	geo_fill("gdp_cap_est") +
	geo_bubbles("gdp_md_est", palette="Set2", col=c("part", "economy"), scale=2) + 
	geo_theme_Europe(legend.titles=list(fill=c("GDP per capita", "xx"), bubble.size="Part of Europe"))

geo_shape(Europe) +
	geo_borders() +
	geo_fill("gdp_cap_est") +
	geo_bubbles("gdp_md_est", palette="Set2", col=c("part", "economy"), scale=2) + 
	geo_theme_Europe(c("A", "B"), legend.titles="bla")


geo_shape(Europe) +
	geo_borders() +
	geo_fill("gdp_cap_est") +
	geo_bubbles("gdp_md_est", palette="Set2", col=c("part", "economy"), scale=2) + 
	geo_theme_Europe(c("A", "B"), legend.titles=c(fill="bla"))


geo_shape(Europe) +
	geo_borders() +
	geo_fill("gdp_cap_est") +
	geo_bubbles("gdp_md_est", palette="Set2", col=c("part", "economy"), scale=2) + 
	geo_theme_Europe("", legend.titles="bla")


data(Europe)
geo_shape(Europe) +
	geo_borders() +
	geo_fill("gdp_cap_est") +
	geo_bubbles("gdp_md_est", palette="Set2", col=c("part", "economy"), scale=2) + 
	geo_facets("part") +
	geo_theme_Europe(scale=2)

