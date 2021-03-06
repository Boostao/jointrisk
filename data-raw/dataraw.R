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
library(leaflet.extras)
library(htmlwidgets)
library(htmltools)
library(jointrisk)
library(sf)


cloud.cap::gcs_get_object("prod_cap_lake_sedo_sandbox_share", "streets.RDS", overwrite = TRUE)

dt <- data.table::rbindlist(
  list(
    "CGEN" = get_risks_cgen(), 
    "UGEN" = get_risks_ugen()
  ),
  use.names = TRUE,
  idcol = "SOURCE",
  fill = TRUE
)

set(dt, j = "MERGEKEY", value = seq_len(nrow(dt)))

streets <- readRDS("streets.RDS")

polygons <- update_polygons(dt, streets = streets)$poly

pockets <- sf::st_cast(sf::st_union(polygons), "POLYGON")

idx <- sf::st_intersects(polygons, pockets)
setDT(polygons)
set(polygons, j = "POLYINDX", value = unlist(idx))
setDT(dt)
set(dt,
    i = .subset2(polygons, "MERGEKEY"),
    j = "POLYINDX",
    value = .subset2(polygons, "POLYINDX"))
suppressWarnings(
  infosup <- dt[!is.na(POLYINDX),
                list(POLYMAXRISASGRB = max(RISASGRB, na.rm = TRUE),
                     POLYSUMMTTOTRAS = sum(MTTOTRAS, na.rm = TRUE)),
                by = POLYINDX])
setDT(infosup, key = "POLYINDX")
setDT(dt     , key = "POLYINDX")
pos <- which(!names(infosup) %chin% names(dt))
dt[, names(infosup)[pos] := infosup[dt[, list(POLYINDX)], pos, with = FALSE]]
dt[is.na(POLYINDX), c("POLYMAXRISASGRB", "POLYSUMMTTOTRAS") := list(RISASGRB, MTTOTRAS)]

res <- list("source" = dt, "pockets" = pockets)

create_map <- function(data, pocket) {
  
  data_cap <- data
  
  data_cap[, popup := (paste("<b>Source :</b>", SOURCE,"<br/> <b>Intervenant :</b>", INTE_NO, "<br/> <b>Situation ID :</b>", SIT_ID, "<br/> <b>Produit :</b>",PROD_CODE,"<br/> <b>TIV :</b>",MTTOTRAS
                             ,"<br/> <b>FUS :</b>",PRINCFUS,"<br/> <b>Type construction :</b>",TYPECONS,"<br/> <b>Classe biens :</b>",RISASGRB
                             ,"<br/> <b>Superficie :</b>",SUPERREZ,"<br/> <b>Rayon :</b>",round(RISKRADIUS,0)))]
  data_cap$popup3 <- sapply(data_cap$popup, HTML)
  
  # Setup the icons
  icons3 <- awesomeIcons(
    icon = case_when(data_cap$PROD_CODE == "MCO" ~ 'building',
                     data_cap$PROD_CODE == "MPF" ~ 'calendar',
                     data_cap$PROD_CODE == "MB"  ~ 'print',
                     data_cap$PROD_CODE == "MD"  ~ 'shopping-cart',
                     data_cap$PROD_CODE == "MC"  ~ 'cut',
                     data_cap$PROD_CODE == "MEN" ~ 'wrench',
                     data_cap$PROD_CODE == "MG"  ~ 'baby',
                     data_cap$PROD_CODE == "MGA" ~ 'car',
                     data_cap$PROD_CODE == "CC"  ~ 'copyright'),
    iconColor = 'Gainsboro',
    library = 'fa',
    markerColor =case_when(data_cap$PROD_CODE == "MCO" ~ 'darkred',
                           data_cap$PROD_CODE == "MPF" ~ 'darkgreen',
                           data_cap$PROD_CODE == "MB"  ~ 'darkblue',
                           data_cap$PROD_CODE == "MD"  ~ 'darkpurple',
                           data_cap$PROD_CODE == "MC"  ~ 'pink',
                           data_cap$PROD_CODE == "MEN" ~ 'blue',
                           data_cap$PROD_CODE == "MG"  ~ 'orange',
                           data_cap$PROD_CODE == "MGA" ~ 'red',
                           data_cap$PROD_CODE == "CC"  ~ 'black'),
    squareMarker = FALSE)
  
  d_wgs84 <- st_transform(pocket, 4326)
  
  # Create the map
  map <- leaflet() %>%
    setView(lng = -71.2682, lat = 46.7927, zoom = 07) %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "OpenStreetMap") %>%
    addSearchOSM() %>%
    addAwesomeMarkers(~as.numeric(LONGICOM),~as.numeric(LATITCOM), icon = icons3, popup = ~popup3,data = data_cap, clusterOptions =    markerClusterOptions())  %>%
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
carte

# month_report <- format(Sys.Date(), "%y%m")
# saveWidget(carte, paste0(getwd(), "/data-raw/Carte effectif_",month_report,".html"), selfcontained = T)
