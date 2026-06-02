# Get basemap tile providers

Get basemap tile providers and their credits (attribution text).
`tmap_providers()` returns a list or vector of provider names or
credits. `tmap_provider_credits()` returns the attribution text for a
specific provider.

## Usage

``` r
.tmap_providers

tmap_provider_credits(provider)

tmap_providers(mode, credits = FALSE, as.list = credits)
```

## Arguments

- provider:

  provider name

- mode:

  mode. If not specified the current active mode is used.

- credits:

  If `TRUE` the credit (attribution) text is returned. If `FALSE`
  (default) the provider name.

- as.list:

  Should the output be returned as a list where names are provider
  names? By default `TRUE` when `credits` is also `TRUE`.

## Value

`tmap_providers()` returns a list or vector (see `as.list`) of provider
names or credits. `tmap_provider_credits()` returns the attribution text
for the specified provider. `.tmap_providers` is an environment; see
Details.

## Details

`.tmap_providers` is an environment populated with all available
provider names as named entries. Its primary purpose is to enable
autocomplete in IDEs such as RStudio: typing `.tmap_providers$` in the
console or a script triggers a dropdown list of all available providers,
making it easy to discover and select provider names without consulting
the documentation. It is not intended to be called as a function.

## Examples

``` r
# List all providers for the current mode
tmap_providers()
#>  [1] "OpenStreetMap"                  "OpenStreetMap.DE"              
#>  [3] "OpenStreetMap.France"           "OpenStreetMap.HOT"             
#>  [5] "OpenTopoMap"                    "Stadia.AlidadeSmooth"          
#>  [7] "Stadia.AlidadeSmoothDark"       "Stadia.OSMBright"              
#>  [9] "Stadia.Outdoors"                "Stadia.StamenToner"            
#> [11] "Stadia.StamenTonerBackground"   "Stadia.StamenTonerLines"       
#> [13] "Stadia.StamenTonerLabels"       "Stadia.StamenTonerLite"        
#> [15] "Stadia.StamenWatercolor"        "Stadia.StamenTerrain"          
#> [17] "Stadia.StamenTerrainBackground" "Stadia.StamenTerrainLabels"    
#> [19] "Esri.WorldStreetMap"            "Esri.WorldTopoMap"             
#> [21] "Esri.WorldImagery"              "Esri.WorldTerrain"             
#> [23] "Esri.WorldShadedRelief"         "Esri.OceanBasemap"             
#> [25] "Esri.NatGeoWorldMap"            "Esri.WorldGrayCanvas"          
#> [27] "CartoDB.Positron"               "CartoDB.PositronNoLabels"      
#> [29] "CartoDB.PositronOnlyLabels"     "CartoDB.DarkMatter"            
#> [31] "CartoDB.DarkMatterNoLabels"     "CartoDB.DarkMatterOnlyLabels"  
#> [33] "CartoDB.Voyager"                "CartoDB.VoyagerNoLabels"       
#> [35] "CartoDB.VoyagerOnlyLabels"      "Thunderforest.OpenCycleMap"    
#> [37] "Thunderforest.Transport"        "Thunderforest.TransportDark"   
#> [39] "Thunderforest.SpinalMap"        "Thunderforest.Landscape"       
#> [41] "Thunderforest.Outdoors"         "Thunderforest.Pioneer"         
#> [43] "Thunderforest.MobileAtlas"      "Thunderforest.Neighbourhood"   

# Use IDE autocomplete to discover providers interactively:
# type .tmap_providers$ in the RStudio console
```
