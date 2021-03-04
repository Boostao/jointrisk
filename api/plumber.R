#* @apiTitle Commercial property joint risks
#* @apiDescription From a single risk, this API return a list of joint risks and related total insured value.                             

#* Health check
#* @get /
function(res) {
  return("OK")
}

#* Update polygons
#* @get /update_polygons
function() {
  future({
    invisible(gcs_get_object(jrfn, saveToDisk = jrfn, overwrite = TRUE))
    polys <- update_polygons(load_risk_cgen(), .jointrisk$assets$polygons, .jointrisk$assets$streets)
    rm(assets, envir = .jointrisk)
    .jointrisk$assets <- new.env(parent = .jointrisk)
    .jointrisk$assets$streets <- readRDS(stfn)
    .jointrisk$assets$polygons <- polys$poly
    return(polys$msg)    
  }, seed = NULL)
}

#* Get polygons id
#* @param dt:df*
#* @post /jointrisk/opus
#* @serializer json
function(dt = example) {
  return(
    list("version" = jointrisk:::pkgV,
         "results" = get_joint_risks(dt, .jointrisk$assets$polygons, .jointrisk$assets$streets))
  )
}

library(jointrisk)

Sys.setenv("GCS_AUTH_FILE" = Sys.getenv("GCS_AUTH_JSON_PATH"))
suppressPackageStartupMessages({library(googleCloudStorageR)})

sto <- gcs_list_objects()$name
jrfn <- grep("souss004.csv?", sto, value = TRUE)[1]
stfn <- grep("streets.RDS?", sto, value = TRUE)[1]

.jointrisk <- new.env()
.jointrisk$assets <- new.env(parent = .jointrisk)

invisible(gcs_get_object(jrfn, saveToDisk = jrfn, overwrite = TRUE))
invisible(gcs_get_object(stfn, saveToDisk = stfn, overwrite = TRUE))

options(jointrisk.riskfile = jrfn)

.jointrisk$assets$streets <- readRDS(stfn)

.jointrisk$assets$polygons <- update_polygons(load_risk_cgen(), streets = .jointrisk$assets$streets)$poly

warmup(.jointrisk$assets$polygons, .jointrisk$assets$streets)
example <- jsonlite::fromJSON('[{"PRCH_ID":82804587,"COMAUBAT":"NA","AFFECTAT":"C8085","LATITCOM":"46.77418",
                                 "LONGICOM":"-71.30196","PRINCFUS":"NA","SUPERREZ":"NA","UMESSUPE":"PI",
                                 "TYPECONS":2}]')
example[is.na(example)] <- "NA"

#* Returns a map of polygons 1km around the selection for validation
#* @get /jointrisk/map/<INTE_NO:int>/<POAS_NO:int>/<PRCH_NO:int>
#* @parser none
#* @serializer htmlwidget
function(INTE_NO, POAS_NO, PRCH_NO, res) {
  poly <- .jointrisk$assets$polygons[which(.jointrisk$assets$polygons$INTE_NO == INTE_NO &
                                           .jointrisk$assets$polygons$POAS_NO == POAS_NO &
                                           .jointrisk$assets$polygons$PRCH_NO == PRCH_NO),]
  if (nrow(poly) == 0) {
    res$status <- 200
    res$body <- "INTE_NO, POAS_NO, PRCH_NO combination not found"
    return(res)
  }
  poly <- .jointrisk$assets$polygons[which(
    (.jointrisk$assets$polygons$xmin - 1000L) < max(poly$xmin) &
    (.jointrisk$assets$polygons$xmax + 1000L) > min(poly$xmax) &
    (.jointrisk$assets$polygons$ymin - 1000L) < max(poly$ymin) &
    (.jointrisk$assets$polygons$ymax + 1000L) > min(poly$ymax)),]
  future({
    create_map(poly)
  }, seed = NULL)
}

# Map ----

library(leaflet)
library(leaflet.extras)
library(data.table)
library(promises)
library(future)
future::plan(future::multisession(workers = 2))

create_map <- function(poly) {
  
  pck <- sf::st_intersects(poly)
  pockets <- poly$geometry[unique(vapply(pck, `[`, numeric(1), 1))]
  pockets <- sf::st_transform(pockets, 4326)
  bounds <- sf::st_bbox(pockets)
  
  poly <- as.data.table(poly)[,grepl("[A-Z]+", names(poly), ignore.case = FALSE), with = FALSE]
  poly[, popup := (paste("<b>Intervenant :</b>",INTE_NO, "<br/> <b>Produit :</b>",PROD_CODE,"<br/> <b>TIV :</b>",MTTOTRAS
                         ,"<br/> <b>FUS :</b>",PRINCFUS,"<br/> <b>Type construction :</b>",TYPECONS,"<br/> <b>Classe biens :</b>",RISASGRB
                         ,"<br/> <b>Superficie :</b>",SUPERREZ,"<br/> <b>Rayon :</b>",round(RISKRADIUS,0)))]
  poly$popup3 <- sapply(poly$popup, htmltools::HTML)
  
  # Setup the icons
  iconset <- c("MCO" = 'building',
               "MCO" = 'building',
               "MPF" = 'calendar',
               "MB"  = 'print',
               "MD"  = 'shopping-cart',
               "MC"  = 'cut',
               "MEN" = 'wrench',
               "MG"  = 'baby',
               "MGA" = 'car',
               "CC"  = 'copyright')
  colorset <- c("MCO" = 'darkred',
                "MPF" = 'darkgreen',
                "MB"  = 'darkblue',
                "MD"  = 'darkpurple',
                "MC"  = 'pink',
                "MEN" = 'blue',
                "MG"  = 'orange',
                "MGA" = 'red',
                "CC"  = 'black')
  icons3 <- awesomeIcons(
    icon = unname(iconset[poly$PROD_CODE]),
    iconColor = 'Gainsboro',
    library = 'fa',
    markerColor = unname(colorset[poly$PROD_CODE]),
    squareMarker = FALSE)
  
  # Create the map
  map <- leaflet() %>%
    setMaxBounds(bounds$xmin[[1]], bounds$ymin[[1]], bounds$xmax[[1]], bounds$ymax[[1]]) %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addSearchOSM(options = searchOptions(collapsed = FALSE)) %>%
    addAwesomeMarkers(~as.numeric(LONGICOM),~as.numeric(LATITCOM), icon = icons3, popup = ~popup3, data = poly, clusterOptions = markerClusterOptions())  %>%
    addPolygons(data = pockets) %>%
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
