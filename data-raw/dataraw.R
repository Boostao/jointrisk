## code to prepare `dataraw` dataset goes here
library(extraw)
options(ext.context = 11)
compo <- get_compo(compo = c("COTYCONS", "COTYCON2"), inforce = TRUE)

usethis::use_data(compo, internal = FALSE, overwrite = TRUE)


# Creation of streets.RDS from HERE Streets layer
# (cpg, dbf, prj, shp, shx files extension with name Streets in
# HERE - 181H0 - Navstreets + Traffic Pattern + Road Roughness dataset)
# Put file in data-raw folder
# Once the file streets.RDS has been created upload to project storage
# sf version has to match the one in the image or you will get
# Error: C stack usage XXXXXXXXX is too close to the limit
# Currently it is 0.9.4 as newer version introduce long delays
library(sf)
library(data.table)
streets <- st_read(dsn = "./data-raw", "Streets")
streets <- st_transform(streets, 3488)
cls_ori <- class(streets)
setDT(streets)
nm <- names(streets)[-length(streets)]
reject_pcode <- c("H2J", "H2Y" , "H2Z", "H3B", "G1L", "G1K", "G1R")
streets <- streets[, LANE_RADIUS := 2L * as.integer(LANE_CAT)][PAVED == "Y" &
                                                               FERRY_TYPE == "H" &
                                                               BRIDGE == "N" &
                                                               PRIVATE == "N" &
                                                               PLOT_ROAD == "N" &
                                                               TUNNEL == "N" &
                                                               !L_POSTCODE %in% reject_pcode &
                                                               !R_POSTCODE %in% reject_pcode][, (nm) := NULL]
set(
  streets,
  j = c("xmin", "ymin", "xmax", "ymax"),
  value = as.list(data.table::rbindlist(lapply(streets$geometry, function(x) as.list(sf::st_bbox(x)))))
)
attr(streets, "class") <- cls_ori
saveRDS(streets, "./data-raw/streets.RDS")

# How to use on an external dataset


# Visualization
# This function takes a data containing IDs, radius and geocode
# and creates pockets for neighbors risks. It returns a data
# containing the pocket number for each PRCH_ID

library(dplyr)
library(data.table)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(jointrisk)
library(sf)
library(extraw)

dt <- extraw::get_policies(
    inforce = TRUE,
    partial = "PRO",
    filters = list(MPROD_ID = c(1071132, 2552253, 1071123, 1071124, 1071118, 2552251, 1071122, 1071125, 2552252),
                   MLIAF_ID = 4,
                   MCAAF_ID = 2),
    detailid = c(140, 959, 971, 1082, 1083,  9045, 9406, 9408, 14218, 14219, 14220, 14367, 14491, 14650, 14660, 14661),
    denormalize = TRUE
  )
streets <- readRDS("./data-raw/streets.RDS")
res <- append_polygons_idx(dt, prefix = "PROD_", streets = streets)

create_map <- function(data, pocket) {

data_cap <- data

data_cap[, popup := (paste("<b>Intervenant :</b>",MINTE_ID, "<br/> <b>Produit :</b>",PRODUIT,"<br/> <b>TIV :</b>",MTTOTRAS
                          ,"<br/> <b>FUS :</b>",PRINCFUS,"<br/> <b>Type construction :</b>",TYPECONS,"<br/> <b>Classe biens :</b>",RISASGRB
                          ,"<br/> <b>Superficie :</b>",SUPERREZ,"<br/> <b>Rayon :</b>",round(RISKRADIUS,0)))]
data_cap$popup3 <- sapply(data_cap$popup,HTML)

# Setup the icons
icons3 <- awesomeIcons(
  icon = case_when(data_cap$PRODUIT == "MCO" ~ 'building',
                   data_cap$PRODUIT == "MPF" ~ 'calendar',
                   data_cap$PRODUIT == "MB"  ~ 'print',
                   data_cap$PRODUIT == "MD"  ~ 'shopping-cart',
                   data_cap$PRODUIT == "MC"  ~ 'cut',
                   data_cap$PRODUIT == "MEN" ~ 'wrench',
                   data_cap$PRODUIT == "MG"  ~ 'baby',
                   data_cap$PRODUIT == "MGA" ~ 'car',
                   data_cap$PRODUIT == "CC"  ~ 'copyright'),
  iconColor = 'Gainsboro',
  library = 'fa',
  markerColor =case_when(data_cap$PRODUIT == "MCO" ~ 'darkred',
                         data_cap$PRODUIT == "MPF" ~ 'darkgreen',
                         data_cap$PRODUIT == "MB"  ~ 'darkblue',
                         data_cap$PRODUIT == "MD"  ~ 'darkpurple',
                         data_cap$PRODUIT == "MC"  ~ 'pink',
                         data_cap$PRODUIT == "MEN" ~ 'blue',
                         data_cap$PRODUIT == "MG"  ~ 'orange',
                         data_cap$PRODUIT == "MGA" ~ 'red',
                         data_cap$PRODUIT == "CC"  ~ 'black'),
  squareMarker = FALSE)

d_wgs84 <- st_transform(pocket, "+init=epsg:4326")

# Create the map
map <- leaflet() %>%
  setView(lng = -71.2682, lat = 46.7927, zoom = 07) %>%
  addProviderTiles(providers$Esri, group = "Esri") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")  %>%
  addAwesomeMarkers(~as.numeric(LONGICOM),~as.numeric(LATITCOM), icon = icons3, popup = ~popup3,data = data_cap, clusterOptions =    markerClusterOptions())  %>%
  addLayersControl(baseGroups = c("Esri","Satellite"),
                   options = layersControlOptions(collapsed = F),
                   position = "topright"
  ) %>%
  addPolygons(data = d_wgs84) %>%
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

carte <- create_map(res$source, res$pockets)
# carte

month_report <- format(Sys.Date(), "%y%m")
saveWidget(carte, paste0(getwd(), "/data-raw/Carte effectif_",month_report,".html"), selfcontained = T)
