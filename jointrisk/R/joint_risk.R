# Constant values
TYPECONS_DEFAULT <- 6L
RISASGRB_DEFAULT <- "4"
SUPERREZ_DEFAULT <- 1L
PRINCFUS_DEFAULT <- 7L
RADIUSMT_DEFAULT <- 9L
GRDFLRAREA_M_LIM <- 10L
GRDFLRAREA_P_LIM <- 200L

RADIUS_MFAD <- 2L

TYPECONS_x_PRINCFUS <- matrix(nrow = 6L, ncol = 10L)
TYPECONS_x_PRINCFUS[, 1L:4L] <- c(3L, 3L, 6L, 6L, 9L, 9L)
TYPECONS_x_PRINCFUS[, 5L:7L] <- c(4L, 4L, 8L, 8L, 12L, 12L)
TYPECONS_x_PRINCFUS[, 8L:10L] <- c(8L, 8L, 16L, 16L, 25L, 25L)

pkgV <- as.character(packageVersion("jointrisk"))

#' @export
#' @title Initialize CGEN database connection
#' @description Establish connection to CGEN database using a configuration files.
#' @param config Configuration file
#' @importFrom ROracle Oracle dbConnect
#' @importFrom jsonlite fromJSON
init_con_cgen <- function(config) {
  if (!file.exists(config)) {
    stop("Could not create a DBI connection object from provided config path.")
  } else {
    config <- jsonlite::fromJSON(config)
    con <- ROracle::dbConnect(ROracle::Oracle(),
                          config$username,
                          config$password,
                          dbname = config$dbname,
                          bigint = "integer")
    return(con)
  }
}

#' @export
#' @title Get risks CGEN
#' @description Query to extract inforce commercial policies details for cgen to use in joint risks evaluation.
#' @param con A DBI connection object.
#' @return A data.table object with IDs and columns :
#' @importFrom data.table setDT dcast setnames
#' @importFrom ROracle dbGetQuery
get_risks_cgen <- function(con) {

  query <- paste0("

    SELECT VEPC.PROD_CODE,
           VEPC.PRCH_ID,
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
     AND DEOB.VEPC_ID = VEPC.VEPC_ID
     AND DEOB.ATGL_ID_DNM IN (140, 959, 971, 1083, 1084, 1092, 9045, 9046, 9400, 9406, 9408, 9413, 10762, 10763, 14218, 14219, 14220, 14367, 14491, 14650, 14660, 14661)
     AND ATGL.ATGL_ID = DEOB.ATGL_ID_DNM

  ")

  dt <- ROracle::dbGetQuery(con, query)
  data.table::setDT(dt, key = "VEPC_ID")

  # Check that there is only one VEPC_ID per PRCH_ID combination
  if (length(unique(dt$VEPC_ID)) > nrow(unique(dt[,list(PRCH_ID)]))) {
    dups <- dt[, list(COUNT = length(unique(VEPC_ID))), by = list(PRCH_ID)][COUNT > 1]
    dups <- paste0(dups[,paste0(PRCH_ID)], collapse = "\n")
    warning(paste0("Multiple VEPC_ID exist for the following :\nPRCH_ID\n", dups, "Only the max VEPC_ID per combination will be kept."))
    keep <- dt[, list(VEPC_ID = max(VEPC_ID)), by = list(PRCH_ID)][,list(VEPC_ID)]
    dt <- dt[keep]
  }

  dt <- data.table::dcast(dt, PRCH_ID + PROD_CODE ~ QUESTION, value.var = "REPONSE")
  data.table::setnames(dt, "14367", "COMAUTBA", skip_absent = TRUE)
  numcol  <- c("SUPERREZ", "SUTOOCCO", "RVEXTBET", "RVEXTBOI", "RVEXTPAP", "RVEXTBRI", "RVEXTALU", "PRINCFUS", "MTTOTRAS")
  suppressWarnings(dt[, (numcol) := lapply(.SD, as.integer), .SDcols = numcol])

  data.table::set(dt,
                  j = "REVETEME",
                  value = c("N", "O")[1L + as.integer(rowSums(dt[,list(RVEXTBET, RVEXTBRI)], na.rm = TRUE) > 59)])

  dt[compo$COTYCONS,
     on = list(MURSTRUC = MURSTRUC,
               PLANCHER = PLANCHER,
               TOITSTRU = TOITSTRU,
               REVETEME = REVETEME),
     `:=`(TYPECONS = as.integer(TYPECONS))]

  dt[compo$COTYCON2,
     on = list(RESAUFEU = RESAUFEU),
     `:=`(TYCONS2 = as.integer(TYCONS2))]

  data.table::set(dt,
                  j = c("MURSTRUC", "PLANCHER", "TOITSTRU", "REVETEME", "RESAUFEU"),
                  value = NULL)

  return(dt)

}


#' @export
#' @title Transformers, more than meet the eyes
#' @description This function tranforms the data...obviously.
#' @return The same data.table with some alterations.
#' @param dt Output from get_risks_cgen or from a json single input query in a data.table format
#' @importFrom data.table set
transform_cgen <- function(dt) {

  data.table::set(dt,
                  j = c("LATITCOM", "LONGICOM", "TYPECONS", "RISASGRB", "SUPERREZ", "PRINCFUS"),
                  value = list(
                    as.numeric(.subset2(dt, "LATITCOM")),
                    as.numeric(.subset2(dt, "LONGICOM")),
                    {x <- .subset2(dt, "TYPECONS"); y <- .subset2(dt, "TYCONS2");
                     nax <- which(is.na(x)); nay <- which(is.na(y[nax]));
                     x[nax] <- y[nax]; x[nax[nay]] <- TYPECONS_DEFAULT;
                     x},
                    {x <- .subset2(dt, "RISASGRB");
                     nax <- which(is.na(x));
                     x[nax] <- RISASGRB_DEFAULT;
                     x},
                    {x <- .subset2(dt, "SUPERREZ");
                     nax <- which(is.na(x));
                     x[nax] <- SUPERREZ_DEFAULT ;
                     x},
                    {x <- .subset2(dt, "PRINCFUS");
                     nax <- which(is.na(x));
                     x[nax] <- PRINCFUS_DEFAULT;
                     x}
                    )
  )

}

#' @export
#' @title Calculate radius
#' @description This function calculates the radius for a risk in order to create neighbors risks pockets
#' later on. It is based on many risk characteristics.
#' @return A data.table with an added RISKRADIUS column.
#' @param dt Transformed output in a data.table format
#' @importFrom data.table set %chin%
calculate_radius <- function(dt) {

  data.table::set(dt,
                  j = "RISKRADIUS",
                  value = list({
                    RISASGRB_F          <- c(1L, 2L)[1L + as.integer(.subset2(dt, "RISASGRB") %chin% c("5", "6", "P"))];
                    COMAUTBA_F          <- c(1, 1.25)[1L + as.integer(.subset2(dt, "COMAUTBA") %in% "O")];
                    AFFECTAT_F          <- c(1, 1.25)[1L + as.integer(.subset2(dt, "AFFECTAT") %in% "6670")];
                    M_OR_P_NA           <- 1L + as.integer(.subset2(dt, "UMESSUP2") %in% c("P", NA));
                    CONVERSION_F        <- c(1, 10.764)[M_OR_P_NA];
                    LOWERLIMIT_F        <- c(GRDFLRAREA_M_LIM, GRDFLRAREA_P_LIM)[M_OR_P_NA];
                    PRINCFUS_TYPECONS_F <- TYPECONS_x_PRINCFUS[.subset2(dt, "TYPECONS") + (.subset2(dt, "PRINCFUS") - 1L) * 6L];
                    RADIUS              <- rep(RADIUSMT_DEFAULT, nrow(dt));
                    GRDFLRAREA          <- .subset2(dt, "SUPERREZ");
                    IND                 <- GRDFLRAREA >= LOWERLIMIT_F
                    RADIUS[IND]         <- sqrt(GRDFLRAREA[IND]/CONVERSION_F[IND]/pi);
                    (RADIUS + RADIUS_MFAD + PRINCFUS_TYPECONS_F * RISASGRB_F) * COMAUTBA_F * AFFECTAT_F;
                  }))

}

#' Change the loaded in memory polygons
#' @param pockets A character string, name of netezza catalog.
set_pockets <- function(pockets) {
  e <- environment()
  v <- ".inmempockets"
  unlockBinding(v, parent.env(e))
  assign(v, pockets, parent.env(e))
  lockBinding(v, parent.env(e))
}

suppressWarnings(
.inmempockets <-
  sf::st_cast(sf::st_union(sf::st_buffer(
    sf::st_transform(
      sf::st_as_sf(
        data.table::data.table(
          PRCH_ID = numeric(),
          LONGICOM = numeric(),
          LATITCOM = numeric(),
          RISKRADIUS = numeric()),
        coords = c("LONGICOM", "LATITCOM"),
        crs = "+proj=longlat +datum=WGS84"),
    3488), dist = 1L)), "POLYGON"))

#' @export
#' @title Polygon-o-tron
#' @description This function takes a data containing IDs, radius and geocode and creates pockets
#' for neighbors risks. It returns a data containing the pocket number for each PRCH_ID
#' @param dt Transformed output with a calculated radius in a data.table format
#' @return A data.table .
#' @importFrom data.table set %chin% setkey as.data.table
#' @importFrom sf st_as_sf st_buffer st_union st_cast st_intersects st_transform
create_polygons <- function(dt) {

 src_dt <- dt[!is.na(LATITCOM) & !is.na(LONGICOM) & PREGEOCO %chin% c("POINTADDRESS","STREETADDRESS","SUBADDRESS"),
              list(PRCH_ID, LONGICOM, LATITCOM, RISKRADIUS)]
 dt_sp <- sf::st_as_sf(src_dt,
                       coords = c("LONGICOM", "LATITCOM"),
                       crs = "+proj=longlat +datum=WGS84")
 dt_sp <- sf::st_transform(dt_sp, 3488)
 circles <- sf::st_buffer(dt_sp, dist = src_dt$RISKRADIUS)
 pockets <- sf::st_cast(sf::st_union(circles), "POLYGON")
 set_pockets(pockets)

 dt_poly <- data.table::as.data.table(sf::st_intersects(dt_sp, pockets))
 set(dt_poly, j = "PRCH_ID", value = .subset2(src_dt, "PRCH_ID"))
 data.table::setkey(dt_poly, "PRCH_ID"); data.table::setkey(dt, "PRCH_ID")
 dt[, POLYGON_ID := dt_poly[dt[,list(PRCH_ID)], col.id]]

 return(dt[,list(PRCH_ID, POLYGON_ID)])

}

#' @export
#' @title Obtain polygons
#' @description This function is used to update and return polygon table.
#' @return A data.table with PRCH_ID and POLYGON_ID.
get_polygons <- function() {
  dt <- get_risks_cgen(init_con_cgen("../config.json"))
  transform_cgen(dt)
  calculate_radius(dt)
  create_polygons(dt)
  return(dt)
}

#' @export
#' @title Polygon blaster
#' @description This function return polygon id from an input data.table using
#' the precomputed pockets.
#' @param dt A data.frame with the following
#' @return PRCH_ID and POLYGON_ID with package version.
#' @importFrom data.table set setDT %chin%
#' @importFrom sf st_as_sf st_buffer st_union st_cast st_intersects st_transform
#' @examples
#' \dontrun{
#' dt <- jsonlite::fromJSON('[{"PRCH_ID":14543671,"COMAUTBA":"NA",
#' "AFFECTAT":"8112","LATITCOM":"45.6388","LONGICOM":"-73.8438",
#' "PRINCFUS":4,"RISASGRB":"1","SUPERREZ":1800,"UMESSUP2":"P",
#' "TYPECONS":5,"TYCONS2":"NA","RISKRADIUS":18.2958}]')
#' get_polygons_id(dt)
#' }
get_polygons_id <- function(dt) {
  setDT(dt)
  required <- c("PRCH_ID", "COMAUTBA", "AFFECTAT", "LATITCOM", "LONGICOM", "PRINCFUS",
                "RISASGRB", "SUPERREZ", "UMESSUP2", "TYPECONS", "TYCONS2")
  if (!all(required %chin% names(dt))) {
    notin <- required[!required %chin% names(dt)]
    stop(paste("Required fields", paste(notin, collapse = ", "), "not found"))
  }
  transform_cgen(dt)
  calculate_radius(dt)
  dt_sp <- sf::st_as_sf(dt,
                        coords = c("LONGICOM", "LATITCOM"),
                        crs = "+proj=longlat +datum=WGS84")
  dt_sp <- sf::st_transform(dt_sp, 3488)
  newrisk <- sf::st_buffer(dt_sp, dist = dt$RISKRADIUS)
  dt_poly <- as.data.frame(sf::st_intersects(newrisk, .inmempockets))
  setDT(dt_poly)
  data.table::set(dt_poly, j = "PRCH_ID", value = .subset2(dt, "PRCH_ID")[.subset2(dt_poly, "row.id")])

  return(c(list("version" = pkgV),
           list("polygons" = dt_poly[,list(POLYGON_ID = list(c(col.id))), by = list(PRCH_ID)])))
}

#' Warmup vectorizer functions for optimized runs.
#' @importFrom jsonlite fromJSON
#' @export
warmup <- function() {
  dt <- jsonlite::fromJSON('[{"PRCH_ID":14543671,"COMAUTBA":"NA","AFFECTAT":"8112","LATITCOM":"45.6388",
                            "LONGICOM":"-73.8438","PRINCFUS":4,"RISASGRB":"1","SUPERREZ":1800,"UMESSUP2":"P",
                            "TYPECONS":5,"TYCONS2":"NA","RISKRADIUS":18.2958}]')
  get_polygons_id(dt)
  return(invisible())
}
