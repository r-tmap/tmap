# CARTO made API keys mandatory for their basemap tiles. Without a key the
# tiles still load, but rendered with a "API KEY REQUIRED" watermark. The
# packages tmap relies on for tile rendering (leaflet-providers.js in view
# mode, maptiles' built-in provider table in plot mode) may not (yet) know
# about the required "key" query parameter, so tmap builds the CARTO tile
# URL itself whenever an API key is supplied, following the URL scheme CARTO
# documents (see the CartoDB entry at
# https://github.com/leaflet-extras/leaflet-providers).
carto_variants = c(
	"CartoDB" = "light_all",
	"CartoDB.Positron" = "light_all",
	"CartoDB.PositronNoLabels" = "light_nolabels",
	"CartoDB.PositronOnlyLabels" = "light_only_labels",
	"CartoDB.DarkMatter" = "dark_all",
	"CartoDB.DarkMatterNoLabels" = "dark_nolabels",
	"CartoDB.DarkMatterOnlyLabels" = "dark_only_labels",
	"CartoDB.Voyager" = "rastertiles/voyager",
	"CartoDB.VoyagerNoLabels" = "rastertiles/voyager_nolabels",
	"CartoDB.VoyagerOnlyLabels" = "rastertiles/voyager_only_labels",
	"CartoDB.VoyagerLabelsUnder" = "rastertiles/voyager_labels_under"
)

is_carto_provider = function(serv) {
	grepl("^CartoDB(\\.|$)", serv)
}

carto_variant = function(serv) {
	unname(carto_variants[serv])
}

carto_url_template = function(variant) {
	sprintf("https://{s}.basemaps.cartocdn.com/%s/{z}/{x}/{y}{r}.png?key={apikey}", variant)
}

# Basemap providers that are not (yet) available in leaflet-providers (view mode) and maptiles (plot mode), and
# therefore added by tmap. The URL template has the file extension, which maptiles needs to read the tiles.
extra_providers = list(
	Esri.WorldDarkGrayCanvas = list(
		url = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}.jpg",
		credits = "Tiles © Esri - Esri, DeLorme, NAVTEQ",
		max_zoom = 16L)
)

is_extra_provider = function(serv) {
	serv %in% names(extra_providers)
}
