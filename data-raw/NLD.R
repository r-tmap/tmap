library(cbsodataR)
library(sf)
library(tidyverse)


## Demographic data on municipality (gemeente), district, (wijk) and neighbourhood (buurt) level
##
## Source CBS
## https://www.cbs.nl/nl-nl/maatwerk/2024/11/kerncijfers-wijken-en-buurten-2022
## https://www.cbs.nl/nl-nl/cijfers/detail/85318NED
##
## Note: numbers from 2022 taken (rather than 2023 or 2024) because of availability: as of sept-2024 these are final figures

wb22 = cbs_get_data(id = "85318NED")

# number of missings per variable:
if (FALSE) {
	sort(sapply(wb22, function(x)sum(is.na(x))))
}


## Download geospatial data from PDOK (a platform for Dutch geospatial data)
gm22 = st_read("https://service.pdok.nl/cbs/gebiedsindelingen/2022/wfs/v1_0?request=GetFeature&service=WFS&version=2.0.0&typeName=gemeente_gegeneraliseerd&outputFormat=json")
pv22 = st_read("https://service.pdok.nl/cbs/gebiedsindelingen/2022/wfs/v1_0?request=GetFeature&service=WFS&version=2.0.0&typeName=provincie_gegeneraliseerd&outputFormat=json")
wk22 = local({
	# PDOK has a restriction of 1000 features per download
	# number of districts in 2022: sum(substr(wb22$WijkenEnBuurten, 1, 2) == "WK")
	wks = list()
	for (start in seq(0, 3000, by = 1000)) {
		wks = c(wks, list(st_read(paste0("https://service.pdok.nl/cbs/gebiedsindelingen/2022/wfs/v1_0?request=GetFeature&service=WFS&version=2.0.0&typeName=wijk_gegeneraliseerd&outputFormat=json&startindex=", start))))
	}
    do.call(rbind, wks)
})


# selection of variables
sel = c(
	WijkenEnBuurten = "code",
	OppervlakteTotaal_111 = "area",
	MateVanStedelijkheid_116 = "urbanity",
	AantalInwoners_5 = "population",
	k_0Tot15Jaar_8 = "population_0_14",
	k_15Tot25Jaar_9 = "population_15_24",
	k_25Tot45Jaar_10= "population_25_44",
	k_45Tot65Jaar_11 = "population_45_64",
	k_65JaarOfOuder_12 = "population_65_plus",
	Woningvoorraad_34 = "dwelling_total",
	GemiddeldeWOZWaardeVanWoningen_35 = "dwelling_value",
	Koopwoningen_40 = "dwelling_ownership",
	Nettoarbeidsparticipatie_67 = "employment_rate",
	OpleidingsniveauLaag_64 = "edu_low",
	OpleidingsniveauMiddelbaar_65 = "edu_middle",
	OpleidingsniveauHoog_66 = "edu_high",
	k_40PersonenMetLaagsteInkomen_73 = "income_low",
	k_20PersonenMetHoogsteInkomen_74 = "income_high")

# some data manipulation
wb22b = wb22 |>
	select(all_of(names(sel))) |>
	rename_at(vars(names(sel)), ~sel) |>
	mutate(code = str_replace_all(code, " ", ""),
		   pop_0_14 = ifelse(population == 0, NA, round(population_0_14 / population * 100)),
		   pop_15_24 = ifelse(population == 0, NA, round(population_15_24 / population * 100)),
		   pop_25_44 = ifelse(population == 0, NA, round(population_25_44 / population * 100)),
		   pop_45_64 = ifelse(population == 0, NA, round(population_45_64 / population * 100)),
		   pop_65plus = ifelse(population == 0, NA, round(population_65_plus / population * 100)),
		   edu_appl_sci = round(edu_high / (edu_low + edu_middle + edu_high) * 100),
		   area = units::as_units(area / 100, "km2"),
		   edu_high = NULL,
		   edu_middle = NULL,
		   edu_low = NULL,
		   population_0_14 = NULL,
		   population_15_24 = NULL,
		   population_25_44 = NULL,
		   population_45_64 = NULL,
		   population_65_plus = NULL,
		   urbanity = factor(urbanity, levels = 1:5, labels = c("extremely urbanised", "strongly urbanised", "moderately urbanised", "hardly urbanised", "not urbanised"))) |>
	relocate(starts_with("pop_"), .after = population)

# Create datasets
NLD_prov = pv22 |>
	rename(code = statcode,
		   name = statnaam) |>
	select(code, name) |>
	mutate(name = stringi::stri_trans_general(name, "Latin-ASCII"))

NLD_dist = wk22 |>
	rename(code = statcode,
		   name = statnaam) |>
	select(code, name) |>
	mutate(name = stringi::stri_trans_general(name, "Latin-ASCII")) |>
	left_join(wb22b, by = "code") |>
	mutate(province = NLD_prov$name[unlist(st_intersects(st_point_on_surface(geometry), NLD_prov))]) |>
	relocate(province, .after = name)

NLD_muni = gm22 |>
	rename(code = statcode,
		   name = statnaam) |>
	select(code, name) |>
	mutate(name = stringi::stri_trans_general(name, "Latin-ASCII")) |>
	left_join(wb22b, by = "code") |>
	mutate(province = NLD_prov$name[unlist(st_intersects(st_point_on_surface(geometry), NLD_prov))]) |>
	relocate(province, .after = name)


# recalculate total area per muni, because it includes water (other than rivers):
if (FALSE) {
	sum(NLD_muni$area)
	sum(NLD_dist$area)
	sum(st_area(NLD_dist))
	sum(st_area(NLD_muni))
}
NLD_muni$area = local({
	aggr_area_from_dist = NLD_dist |>
		st_drop_geometry() |>
		mutate(code_muni = substr(code, 3, 6)) |>
		group_by(code_muni) |>
		summarize(area_sum = sum(area)) |>
		ungroup() |>
		mutate(code = paste0("GM", code_muni))
	aggr_area_from_dist$area_sum[match(NLD_muni$code, aggr_area_from_dist$code)]
})


# Set precision to 1: round coordinates by meters (to reduce file size)
NLD_prov$geometry = NLD_prov |> st_geometry() |> st_sfc(precision = 1) %>% st_as_binary %>% st_as_sfc
st_crs(NLD_prov) = st_crs(wk22)

NLD_dist$geometry = NLD_dist |> st_geometry() |> st_sfc(precision = 1) %>% st_as_binary %>% st_as_sfc
st_crs(NLD_dist) = st_crs(wk22)

NLD_muni$geometry = NLD_muni |> st_geometry() |> st_sfc(precision = 1) %>% st_as_binary %>% st_as_sfc
st_crs(NLD_muni) = st_crs(wk22)

if (FALSE) {
	# checks
	all(sapply(NLD_dist, function(x) if (!is.list(x)) sum(is.infinite(x)) else 0) == 0)
	all(sapply(NLD_dist, function(x) if (!is.list(x)) sum(is.nan(x)) else 0) == 0)
}


save(NLD_dist, file="data/NLD_dist.rda", compress="xz")
save(NLD_muni, file="data/NLD_muni.rda", compress="xz")
save(NLD_prov, file="data/NLD_prov.rda", compress="xz")

