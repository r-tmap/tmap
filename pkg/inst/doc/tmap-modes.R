## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, fig.width=8, fig.height=4)
library(tmap)
library(leaflet)

## ------------------------------------------------------------------------
data(World, metro)
metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100

mapWorld <- tm_shape(World) +
	tm_polygons("income_grp", palette="-Blues", contrast=.7, id="name", title="Income group") +
	tm_shape(metro) +
	tm_bubbles("pop2010", col = "growth", 
			   border.col = "black", border.alpha = .5, 
			   style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf),
			   palette="-RdYlBu", contrast=1, 
			   title.size="Metro population", 
			   title.col="Growth rate (%)", id="name") + 
	tm_style_gray() + tm_format_World()

## ------------------------------------------------------------------------
mapWorld

## ------------------------------------------------------------------------
# current mode:
tmap_mode()

getwd()


## ---- eval=F, echo=F, warning=F, message=F-------------------------------
#  # set mode to view:
#  tmap_mode("view")

## ---- eval=F, echo=F, warning=F, message=F-------------------------------
#  mapWorld

## ---- include=FALSE------------------------------------------------------
tmap_mode("plot")

