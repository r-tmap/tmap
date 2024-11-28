library(cbsodataR)
library(sf)
library(tidyverse)

wb24 = cbs_get_data(id = "85984NED")
wb23 = cbs_get_data(id = "85618NED")
wb22 = cbs_get_data(id = "85318NED")

#https://geodata.cbs.nl/files/Wijkenbuurtkaart/WijkBuurtkaart_2023_v2.zip
dir = tempdir()

sort(sapply(wb22, function(x)sum(is.na(x))))




gemeentegrenzen <- st_read("https://service.pdok.nl/cbs/gebiedsindelingen/2017/wfs/v1_0?request=GetFeature&service=WFS&version=2.0.0&typeName=gemeente_gegeneraliseerd&outputFormat=json")

gm22 = st_read("https://service.pdok.nl/cbs/gebiedsindelingen/2022/wfs/v1_0?request=GetFeature&service=WFS&version=2.0.0&typeName=gemeente_gegeneraliseerd&outputFormat=json")

pv22 = st_read("https://service.pdok.nl/cbs/gebiedsindelingen/2022/wfs/v1_0?request=GetFeature&service=WFS&version=2.0.0&typeName=provincie_gegeneraliseerd&outputFormat=json")


#number of wijken in 2022: sum(substr(wb22$WijkenEnBuurten, 1, 2) == "WK")

wks = list()
for (start in seq(0, 3000, by = 1000)) {
	wks = c(wks, list(st_read(paste0("https://service.pdok.nl/cbs/gebiedsindelingen/2022/wfs/v1_0?request=GetFeature&service=WFS&version=2.0.0&typeName=wijk_gegeneraliseerd&outputFormat=json&startindex=", start))))
}
wk22 = do.call(rbind, wks)

sel = c(#statcode = "code",
	#statnaam = "name",
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

wb22b = wb22 |>
	select(all_of(names(sel))) |>
	rename_at(vars(names(sel)), ~sel) |>
	mutate(code = str_replace_all(code, " ", ""),
		   pop_0_14 = round(population_0_14 / population * 100),
		   pop_15_24 = round(population_15_24 / population * 100),
		   pop_25_44 = round(population_25_44 / population * 100),
		   pop_45_64 = round(population_45_64 / population * 100),
		   pop_65plus = round(population_65_plus / population * 100),
		   edu_appl_sci = round(edu_high / (edu_low + edu_middle + edu_high) * 100),
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
	mutate(prov_code = NLD_prov$code[unlist(st_intersects(st_point_on_surface(geometry), NLD_prov))]) |>
	relocate(prov_code, .after = name)

NLD_muni = gm22 |>
	rename(code = statcode,
		   name = statnaam) |>
	select(code, name) |>
	mutate(name = stringi::stri_trans_general(name, "Latin-ASCII")) |>
	left_join(wb22b, by = "code") |>
	mutate(prov_code = NLD_prov$code[unlist(st_intersects(st_point_on_surface(geometry), NLD_prov))]) |>
	relocate(prov_code, .after = name)

# set precision to 1: round coordinates by meters
NLD_prov$geometry = NLD_prov |> st_geometry() |> st_sfc(precision = 1) %>% st_as_binary %>% st_as_sfc
st_crs(NLD_prov) = st_crs(wk22)

NLD_dist$geometry = NLD_dist |> st_geometry() |> st_sfc(precision = 1) %>% st_as_binary %>% st_as_sfc
st_crs(NLD_dist) = st_crs(wk22)

NLD_muni$geometry = NLD_muni |> st_geometry() |> st_sfc(precision = 1) %>% st_as_binary %>% st_as_sfc
st_crs(NLD_muni) = st_crs(wk22)


save(NLD_dist, file="data/NLD_dist.rda", compress="xz")
save(NLD_muni, file="data/NLD_muni.rda", compress="xz")
save(NLD_prov, file="data/NLD_prov.rda", compress="xz")

