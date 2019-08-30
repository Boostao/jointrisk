data("compo")

#' @export
#' @importFrom data.table setDT dcast setnames
#' @importFrom ROracle dbGetQuery
get_risks <- function(con) {

  query <- paste0("

    SELECT POAS.POAS_NO,
           VEPC.PROD_CODE,
           POAS.INTE_NO,
           PRCH.PRCH_NO,
           VEPC.VEPC_ID,
           coalesce(ATGL.ATGL_CODE_INFO, cast(DEOB.ATGL_ID_DNM as varchar(15))) QUESTION,
           DEOB.DEOB_VAL REPONSE,
           DEOB.VAAT_ID_VAL

    FROM SOUS_TBL_POL_ASS             POAS,
         SOUS_TBL_PERD_COUVRT_POL_ASS PCPA,
         SOUS_TBL_COMBINSN_TRANSCT    COTR,
         SOUS_TBL_DETAIL_COMBINSN     DECT,
         SOUS_TBL_PERD_COUVRT         PECO,
         SOUS_TBL_VPROD_CHOISI        VEPC,
         SOUS_TBL_PROD_CHOISI         PRCH,
         SOUS_TBL_DETAIL_OBT          DEOB,
         PILO_TBL_ATTR_GLOB           ATGL

    WHERE
         POAS.VAAT_ID_LIG_AFF = 4
     AND POAS.VAAT_ID_CATEG_AFF = 2
     AND PCPA.POAS_ID = POAS.POAS_ID
     AND PCPA.VAAT_ID_TYPE_STATUT_DNM_STSO = 9
     AND sysdate BETWEEN PCPA.PCPA_DT_VIG AND NVL(PCPA.PCPA_DT_EXPIR, sysdate)
     AND COTR.PCPA_ID = PCPA.PCPA_ID
     AND COTR.COTR_IND_STA_PRCH_DNM = 'O'
     AND COTR.VAAT_ID_TYPE_STATUT_DNM_STSO = 9
     AND DECT.COTR_ID = COTR.COTR_ID
     AND DECT.DECT_IND_ACTIF = 'O'
     AND PECO.PECO_ID = DECT.PECO_ID
     AND sysdate BETWEEN PECO.PECO_DT_VIG AND NVL(PECO.PECO_DT_EXPIR, sysdate)
     AND VEPC.VEPC_ID = DECT.VEPC_ID_DNM
     AND VEPC.PROD_CODE IN ('MPF', 'MD', 'MB', 'MCO')
     AND PRCH.PRCH_ID = VEPC.PRCH_ID
     AND DEOB.VEPC_ID = VEPC.VEPC_ID
     AND DEOB.ATGL_ID_DNM IN (140, 959, 971, 1083, 1084, 1092, 9045, 9046, 9400, 9406, 9408, 9413, 10762, 10763, 14218, 14219, 14220, 14367, 14491, 14650, 14660, 14661)
     AND ATGL.ATGL_ID = DEOB.ATGL_ID_DNM

  ")

  dt <- dbGetQuery(con, query)
  setDT(dt, key = "VEPC_ID")

  # Check that there is only one VEPC_ID per INTE_NO/POAS_NO/PRCH_NO combination
  if (length(unique(dt$VEPC_ID)) > nrow(unique(dt[,list(INTE_NO, POAS_NO, PRCH_NO)]))) {
    dups <- dt[, list(COUNT = length(unique(VEPC_ID))), by = list(INTE_NO, POAS_NO, PRCH_NO)][COUNT > 1]
    dups <- paste0(dups[,paste0(INTE_NO, ", ", POAS_NO, ", ", PRCH_NO)], collapse = "\n")
    warning(paste0("Multiple VEPC_ID exist for the following combination :\nINTE_NO, POAS_NO, PRCH_NO\n", dups, "Only the max VEPC_ID per combination will be kept."))
    keep <- dt[, list(VEPC_ID = max(VEPC_ID)), by = list(INTE_NO, POAS_NO, PRCH_NO)][,list(VEPC_ID)]
    dt <- dt[keep]
  }

  dt <- dcast(dt, INTE_NO + POAS_NO + PRCH_NO + PROD_CODE ~ QUESTION, value.var = "REPONSE")
  setnames(dt, "14367", "COMAUTBA", skip_absent = TRUE)
  numcol  <- c("SUPERREZ", "SUTOOCCO", "RVEXTBET", "RVEXTBOI", "RVEXTPAP", "RVEXTBRI", "RVEXTALU", "PRINCFUS", "MTTOTRAS")
  suppressWarnings(dt[, (numcol) := lapply(.SD, as.numeric), .SDcols = numcol])

  set(dt, j = "REVETEME", value = ifelse(rowSums(dt[,list(RVEXTBET, RVEXTBRI)], na.rm = TRUE) > 59, "O", "N"))

  dt[compo$COTYCONS,
     on = list(MURSTRUC = MURSTRUC,
               PLANCHER = PLANCHER,
               TOITSTRU = TOITSTRU,
               REVETEME = REVETEME),
     `:=`(TYPECONS = TYPECONS)]

  dt[compo$COTYCON2,
     on = list(RESAUFEU = RESAUFEU),
     `:=`(TYCONS2 = TYCONS2)]

  return(dt)

}


# Usage temporaire

library(ROracle)
library(data.table)
# la liste de connectString est dans U:\ccap\asge\sqlnet\TNSNAMES.ORA
con <- dbConnect(Oracle(),
                 Sys.getenv("USERNAME"),
                 Sys.getenv("USERNAME"),
                 dbname = "(DESCRIPTION = (ADDRESS_LIST = (ADDRESS = (PROTOCOL=TCP)(HOST = opus_intg.bd.capitale.qc.ca)(PORT = 1710)))(CONNECT_DATA = (SERVICE_NAME = intg.capitale.qc.ca)(SERVER = DEDICATED)))",
                 bigint = "integer")

system.time(dt <- get_risks(con))

# il reste a peut-etre renommer les variables et valider les donnees
#       DETAIL    ID CODE_INFO                                       NOM_ABRG DATATYPE
#  1: AFFECTAT   140  AFFECTAT Affectation principale ***(imprimé au contrat)   CARACT
#  2: MURSTRUC   959  MURSTRUC                    Murs structure construction     CODE
#  3: TOITSTRU   971  TOITSTRU                             Toiture(structure)     CODE
#  4: SUPERREZ  1083  SUPERREZ                 Superficie du  rez-de-chaussee   ENTIER
#  5: SUTOOCCO  1084  SUTOOCCO             Superficie totale occupee commerce   ENTIER
#  6: UMESSUP2  1092  UMESSUP2                      Unite mesure=superficie 2     CODE
#  7: RVEXTBET  9045  RVEXTBET      Revet.ext.beton/bloc beton/briq.solide= %   ENTIER
#  8: RVEXTBOI  9046  RVEXTBOI   Revet.ext.bois/stucco/vinyle/autre déclin= %   ENTIER
#  9: RVEXTPAP  9400  RVEXTPAP   Revetement exterieur(papier  brique/aucun)=%   ENTIER
# 10: PLANCHER  9406  PLANCHER                       Plancher(s) construction   CARACT
# 11: RVEXTBRI  9408  RVEXTBRI          Revetement exterieur  brique/pierre=%   ENTIER
# 12: RVEXTALU  9413  RVEXTALU     Revetement  ext.  aluminium/métal/acier= %   ENTIER
# 13: TYPECONS 10762  TYPECONS                              Type construction   ENTIER
# 14: REVETEME 10763  REVETEME                        (conv. tar.) revêtement     CODE
# 15: RISASGRB 14218  RISASGRB                    Risque assuré - classe bien   CARACT
# 16: PRINCFUS 14219  PRINCFUS                      Protection incendie (fus)   ENTIER
# 17: MTTOTRAS 14220  MTTOTRAS                 Montant total du risque assuré   ENTIER
# 18:    14367 14367      <NA>             Communicant avec un autre bâtiment   CARACT
# 19: RESAUFEU 14491  RESAUFEU  Rés. au feu/incombus.(locataire 150 000 et -)   CARACT
# 20: PREGEOCO 14650  PREGEOCO                 Précision geocode - commercial   CARACT
# 21: LONGICOM 14660  LONGICOM                         Longitude - commercial   CARACT
# 22: LATITCOM 14661  LATITCOM                          Latitude - commercial   CARACT
#       DETAIL    ID CODE_INFO                                       NOM_ABRG DATATYPE


library(data.table)

# This function tranforms the data...obviously

transform_data <- function(data) {

# Change the data to data.table
data_cap_pure <- setDT(data)

# Keep only geocode with good precision
data_cap <- data_cap_pure[PREGEOCO %in% c("POINTADDRESS","STREETADDRESS","SUBADDRESS")]

# Remove lines with empty geocode
data_cap <- data_cap[!is.na(LATITCOM)]
data_cap <- data_cap[!is.na(LONGICOM)]

# Transform geocode in numeric
data_cap$LATITCOM <- as.numeric(data_cap$LATITCOM)
data_cap$LONGICOM <- as.numeric(data_cap$LONGICOM)

# Choose the right TYPECONS
which_lines <- which(is.na(data_cap$TYPECONS))
set(data_cap , i = which_lines , j = "TYPECONS" , value =  data_cap[which(is.na(data_cap$TYPECONS))]$TYCONS2)

# Put default value when we don't have the information
which_lines <- which(is.na(data_cap$RISASGRB))
set(data_cap , i = which_lines , j = "RISASGRB" , value =  "4")

which_lines <- which(is.na(data_cap$SUPERREZ))
set(data_cap , i = which_lines , j = "SUPERREZ" , value = 1)

which_lines <- which(is.na(data_cap$TYPECONS))
set(data_cap , i = which_lines , j = "TYPECONS" , value = 6)

which_lines <- which(is.na(data_cap$PRINCFUS))
set(data_cap , i = which_lines , j = "PRINCFUS" , value = 7)

# Transform RISASGRB in ordered factor
set(data_cap , j = "RISASGRB", value = ordered(data_cap$RISASGRB , levels = c("1", "2", "3", "4", "5", "6", "P")))

# Merge intervenant number with product number
set(data_cap , i =  , j = "ID" , value = paste0(data_cap$INTE_NO, "-",data_cap$POAS_NO, "-",data_cap$PBVEHRES))

# Keep only the columns needed
#data_cap <- data_cap[,c("AFFECTAT","COMAUTBA","LATITCOM","LONGICOM","SUPERREZ","UMESSUP2","TYPECONS","RISASGRB","PRINCFUS")]

return(data_cap)

}

dt2 <-transform_data(dt)


# This function calculates the radius for a risk in
# order to create neighbors risks pockets later on.
# It is based on many risk characteristics

calculate_radius <- function(data) {

# Input needed to calculate the radius : AFFECTAT, COMAUTBA, SUPERREZ, UMESSUP2, TYPECONS, RISASGRB, PRINCFUS

data_cap <- setDT(data)

# RISASGRB
# This factor will be a in a multiplication later on
set(data_cap , i = , j = "RISASGRB_fact" , value = 1)
which_lines <- which(data_cap$RISASGRB < "5")
set(data_cap , i = which_lines , j = "RISASGRB_fact" , value = 1)
which_lines <- which(data_cap$RISASGRB >= "5")
set(data_cap , i = which_lines , j = "RISASGRB_fact" , value = 2)

# PRINCFUS and TYPECONS
set(data_cap , i = , j = "PRINCFUS_TYPECONS_fact" , value = 0)

which_lines <- which(data_cap$PRINCFUS <= 4 & data_cap$PRINCFUS >= 1 & data_cap$TYPECONS <= 2 & data_cap$TYPECONS >= 1)
set(data_cap , i = which_lines , j = "PRINCFUS_TYPECONS_fact" , value = 3)
which_lines <- which(data_cap$PRINCFUS <= 4 & data_cap$PRINCFUS >= 1 & data_cap$TYPECONS <= 4 & data_cap$TYPECONS >= 3)
set(data_cap , i = which_lines , j = "PRINCFUS_TYPECONS_fact" , value = 6)
which_lines <- which(data_cap$PRINCFUS <= 4 & data_cap$PRINCFUS >= 1 & data_cap$TYPECONS <= 6 & data_cap$TYPECONS >= 5)
set(data_cap , i = which_lines , j = "PRINCFUS_TYPECONS_fact" , value = 9)

which_lines <- which(data_cap$PRINCFUS <= 7 & data_cap$PRINCFUS >= 5 & data_cap$TYPECONS <= 2 & data_cap$TYPECONS >= 1)
set(data_cap , i = which_lines , j = "PRINCFUS_TYPECONS_fact" , value = 4)
which_lines <- which(data_cap$PRINCFUS <= 7 & data_cap$PRINCFUS >= 5 & data_cap$TYPECONS <= 4 & data_cap$TYPECONS >= 3)
set(data_cap , i = which_lines , j = "PRINCFUS_TYPECONS_fact" , value = 8)
which_lines <- which(data_cap$PRINCFUS <= 7 & data_cap$PRINCFUS >= 5 & data_cap$TYPECONS <= 6 & data_cap$TYPECONS >= 5)
set(data_cap , i = which_lines , j = "PRINCFUS_TYPECONS_fact" , value = 12)

which_lines <- which(data_cap$PRINCFUS <= 10 & data_cap$PRINCFUS >= 8 & data_cap$TYPECONS <= 2 & data_cap$TYPECONS >= 1)
set(data_cap , i = which_lines , j = "PRINCFUS_TYPECONS_fact" , value = 8)
which_lines <- which(data_cap$PRINCFUS <= 10 & data_cap$PRINCFUS >= 8 & data_cap$TYPECONS <= 4 & data_cap$TYPECONS >= 3)
set(data_cap , i = which_lines , j = "PRINCFUS_TYPECONS_fact" , value = 16)
which_lines <- which(data_cap$PRINCFUS <= 10 & data_cap$PRINCFUS >= 8 & data_cap$TYPECONS <= 6 & data_cap$TYPECONS >= 5)
set(data_cap , i = which_lines , j = "PRINCFUS_TYPECONS_fact" , value = 25)

# COMAUTBA
# This factor will be a in a multiplication later on
set(data_cap , i = , j = "COMAUTBA_fact" , value = 1)
which_lines <- which(data_cap$COMAUTBA == "O")
set(data_cap , i = which_lines , j = "COMAUTBA_fact" , value = 1.25)

# AFFECTAT - Increase the radius for town hall
# This factor will be a in a multiplication later on
set(data_cap , i = , j = "AFFECTAT_fact" , value = 1)
which_lines <- which(data_cap$affectation == "6670")
set(data_cap , i = which_lines , j = "AFFECTAT_fact" , value = 1.25)

# Base radius calculate with area
set(data_cap , i = , j = "radius" , value = 9)
which_lines <- which(data_cap$UMESSUP2 == "P" & data_cap$SUPERREZ < 200)
set(data_cap , i = which_lines , j = "radius" , value = 9)
which_lines <- which(data_cap$UMESSUP2 == "P" & data_cap$SUPERREZ >= 200)
set(data_cap , i = which_lines , j = "radius" , value = sqrt((data_cap$SUPERREZ[which_lines]/10.764)/pi))
which_lines <- which(is.na(data_cap$UMESSUP2) & data_cap$SUPERREZ < 200)
set(data_cap , i = which_lines , j = "radius" , value = 9)
which_lines <- which(is.na(data_cap$UMESSUP2) & data_cap$SUPERREZ >= 200)
set(data_cap , i = which_lines , j = "radius" , value = sqrt((data_cap$SUPERREZ[which_lines]/10.764)/pi))
which_lines <- which(data_cap$UMESSUP2 == "M" & data_cap$SUPERREZ < 10)
set(data_cap , i = which_lines , j = "radius" , value = 9)
which_lines <- which(data_cap$UMESSUP2 == "M" & data_cap$SUPERREZ >= 10)
set(data_cap , i = which_lines , j = "radius" , value = sqrt((data_cap$SUPERREZ[which_lines])/pi))

# Calculate the radius with factors
# Warning : Radius is in meter
# I add 2 meters for conservatism purpose
set(data_cap , i = , j = "final_radius" , value = (data_cap$radius + 2 + data_cap$PRINCFUS_TYPECONS_fact * data_cap$RISASGRB_fact) * data_cap$COMAUTBA_fact * data_cap$AFFECTAT_fact)

# Keep only the columns needed
#data_cap <- data_cap[,c("LATITCOM","LONGICOM","final_radius")]

return(data_cap)

}

dt3 <-calculate_radius(dt2)






# This function takes a data containing IDs, radius and geocode
# and creates pockets for neighbors risks. It returns a data
# containing the pocket number for each PRCH_ID

create_polygons <- function(data) {

library(sf)

# Transform into spatial data...thug life
data_spatial <- st_as_sf(x = dt3,
                         coords = c("LONGICOM", "LATITCOM"),
                         crs = "+proj=longlat +datum=WGS84") %>% st_transform(3488)

# Create circles around geocode
circles <- st_buffer(data_spatial, dist = data_spatial$final_radius)

# Create polygons
pocket <-st_union(circles)

# The last one was a multipolygon...so sad...put that thug into polygons
pocket2 <-st_cast(pocket, "POLYGON")

# Output a matrix that checks if my geocodes are in each polygon
matrix_poly <-st_intersects(data_spatial,pocket2,sparse = TRUE)

# Change that matrix into an actually workable class
as.data.frame(matrix_poly)

############## Il faut exporter la liste de PRCH_ID pour OPUS et l'objet pocket2 pour le processus live

}





# Process liivvvveeeeeeeeeee
# Faire une fonction qui a du sens avec ça
# Transform into spatial data...thug life
data_spatial <- st_as_sf(x = data_cap_new,
                         coords = c("longitude", "latitude"),
                         crs = "+proj=longlat +datum=WGS84") %>% st_transform(3488)

# Create circles around geocode
cercles <- st_buffer(data_spatial, dist = data_spatial$rayon_final)

# Output a matrix that checks if my geocodes are in each polygon
matrix_poly <-st_intersects(cercles,poly,sparse = TRUE)

# Extract lines pocket touching our brand new potential risk
poly_touch<-poly[c(unlist(matrix_poly)),]






# This function takes a data containing IDs, radius and geocode
# and creates pockets for neighbors risks. It returns a data
# containing the pocket number for each PRCH_ID

create_map <- function(data, pocket) {

library(plotly)
library(data.table)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(htmltools)

data_cap <- data

data_cap[, popup:= (paste("<b>Intervenant :</b>",ID, "<br/> <b>Produit :</b>",PROD_CODE,"<br/> <b>TIV :</b>",MTTOTRAS
                          ,"<br/> <b>FUS :</b>",PRINCFUS,"<br/> <b>Type construction :</b>",TYPECONS,"<br/> <b>Classe biens :</b>",RISASGRB
                          ,"<br/> <b>Superficie :</b>",SUPERREZ,"<br/> <b>Rayon :</b>",round(final_radius,0)))]
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

create_map(dt3,pocket2)
