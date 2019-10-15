## code to prepare `dataraw` dataset goes here
library(extractnetezza)
options(extractnetezza.context = 11)
compo <- get_compo(compo = c("COTYCONS", "COTYCON2"), inforce = TRUE)

usethis::use_data(compo, internal = FALSE, overwrite = TRUE)


# How to use on an external dataset


# Visualization
# This function takes a data containing IDs, radius and geocode
# and creates pockets for neighbors risks. It returns a data
# containing the pocket number for each PRCH_ID

library(plotly)
library(data.table)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(jointrisk)

dt <- extractnetezza::get_policies(
    inforce = TRUE,
    partial = "PRO",
    filters = list(MPROD_ID = c(2552251, 1071124, 1071125, 1071122),
                   MLIAF_ID = 4,
                   MCAAF_ID = 2),
    detailid = c(140, 959, 971, 1083, 1092, 9045, 9406, 9408, 14218, 14219, 14220, 14367, 14491, 14650, 14660, 14661)
  )
append_polygons_idx(dt, prefix = "PROD_", comautba = "PROD_14367")

create_map <- function(data, pocket) {

data_cap <- data

data_cap[, popup:= (paste("<b>Intervenant :</b>",PRCH_ID, "<br/> <b>Produit :</b>",PROD_CODE,"<br/> <b>TIV :</b>",MTTOTRAS
                          ,"<br/> <b>FUS :</b>",PRINCFUS,"<br/> <b>Type construction :</b>",TYPECONS,"<br/> <b>Classe biens :</b>",RISASGRB
                          ,"<br/> <b>Superficie :</b>",SUPERREZ,"<br/> <b>Rayon :</b>",round(RISKRADIUS,0)))]
data_cap$popup3 <- sapply(data_cap$popup,HTML)

icons3 <- awesomeIcons(
  icon = if_else(data_cap$PROD_CODE == "MCO", 'building',
                 if_else(data_cap$PROD_CODE == "MPF", 'calendar',
                         if_else(data_cap$PROD_CODE == "MB", 'print','shopping-cart'))),
  iconColor = 'Gainsboro',
  library = 'fa',
  markerColor = if_else(data_cap$PROD_CODE == "MCO", 'darkred',
                        if_else(data_cap$PROD_CODE == "MPF", 'darkgreen',
                                if_else(data_cap$PROD_CODE == "MB", 'darkblue','darkpurple'))),
  squareMarker = FALSE)


d_wgs84 <- st_transform(pocket, "+init=epsg:4326")

map <- leaflet() %>%
  setView(lng = -71.2682, lat = 46.7927, zoom = 07) %>%
  addProviderTiles(providers$Esri, group = "Esri") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")  %>%
  addAwesomeMarkers(~LONGICOM ,~LATITCOM, icon=icons3, popup = ~popup3,data= data_cap, clusterOptions =    markerClusterOptions())  %>%
  addLayersControl(baseGroups = c("Esri","Satellite"),
                   options = layersControlOptions(collapsed = F),
                   position = "topright"
  ) %>%
  addPolygons(data=d_wgs84) %>%
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "meters",
    secondaryLengthUnit = "kilometers",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479",
    localization = "fr")
return(map)

}

dt <- get_risks_cgen(init_con_cgen("../config.json"))
transform_cgen(dt)
calculate_radius(dt)
create_polygons(dt)

create_map(dt, .inmempockets)
