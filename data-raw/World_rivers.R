library(rnaturalearth)
library(sf)
library(dplyr)

rivers <- rnaturalearth::ne_download(scale = 50, type = "rivers_lake_centerlines_scale_rank", category = "physical", returnclass = "sf")
rivers$strokeweig <- rivers$strokeweig * 10

rivers <- rivers %>% select(name, type=featurecla, scalerank, strokelwd = strokeweig)

#rivers$name <- factor(rivers$name)
rivers$type <- factor(rivers$type)
rivers$scalerank <- as.integer(rivers$scalerank)
rivers$strokelwd <- as.numeric(rivers$strokelwd)

# the names are UTF-8: transliterate to ASCII (e.g. "Sénégal" -> "Senegal"), as in NLD.R
rivers$name <- stringi::stri_trans_general(rivers$name, "Latin-ASCII")
stopifnot(all(is.na(rivers$name) | grepl("^[ -~]*$", rivers$name)))

rivers = sf::st_set_crs(rivers, 4326)

rivers = rivers %>%
	dplyr::filter(!is.na(type))


rivers$geometry = rivers |> st_geometry() |> st_sfc(precision = 1000) %>% st_as_binary %>% st_as_sfc

World_rivers = rivers

World_rivers = st_set_crs(World_rivers, 4326)

save(World_rivers, file="data/World_rivers.rda", compress="xz")
