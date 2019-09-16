# Constant values
TYPECONS_DEFAULT <- 6L;   RISASGRB_DEFAULT <- "4";  SUPERREZ_DEFAULT <- 1L
PRINCFUS_DEFAULT <- 7L;   RADIUSMT_DEFAULT <- 9L;   GRDFLRAREA_M_LIM <- 10L
GRDFLRAREA_P_LIM <- 200L; RADIUS_MFAD <- 2L

TYPECONS_x_PRINCFUS <- matrix(nrow = 6L, ncol = 10L)
TYPECONS_x_PRINCFUS[, 1L:4L] <- c(3L, 3L, 6L, 6L, 9L, 9L)
TYPECONS_x_PRINCFUS[, 5L:7L] <- c(4L, 4L, 8L, 8L, 12L, 12L)
TYPECONS_x_PRINCFUS[, 8L:10L] <- c(8L, 8L, 16L, 16L, 25L, 25L)

pkgV <- as.character(packageVersion("jointrisk"))


# Functions to extract data from datawarehouse used to compute polygons

#' @export
#' @title Get risks CGEN from Oracle
#' @description Query to extract inforce commercial policies details for cgen to use in joint risks evaluation.
#' @return A data.table object with IDs and columns.
#' @importFrom data.table setDT dcast setnames
get_risks_cgen_oracle <- function() {
  con <- ROracle::dbConnect(ROracle::Oracle(),
                            Sys.getenv("USERNAME"),
                            Sys.getenv("USERNAME"),
                            dbname = "(DESCRIPTION = (ADDRESS_LIST = (ADDRESS = (PROTOCOL=TCP)(HOST = opus_intg.bd.capitale.qc.ca)(PORT = 1710)))(CONNECT_DATA = (SERVICE_NAME = intg.capitale.qc.ca)(SERVER = DEDICATED)))",
                            bigint = "integer")

  # Updater avec la query qui utilise les codes equiv
  query <- paste0("
    SELECT POAS.POAS_NO,
           POAS.INTE_NO,
           PRCH.PRCH_NO,
           VEPC.PROD_CODE,
           VEPC.PRCH_ID,
           VEPC.VEPC_ID,
           coalesce(ATGL.ATGL_CODE_INFO, cast(DEOB.ATGL_ID_DNM as varchar(15))) QUESTION,
           DEOB.DEOB_VAL REPONSE

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
     AND DEOB.ATGL_ID_DNM IN (140, 959, 971, 1083, 1092, 9045, 9406, 9408, 14218, 14219, 14220, 14367, 14491, 14650, 14660, 14661)
     AND ATGL.ATGL_ID = DEOB.ATGL_ID_DNM

  ")

  dt <- ROracle::dbGetQuery(con, query, bulk_read = 50000L)
  data.table::setDT(dt, key = "VEPC_ID")

  # Check that there is only one VEPC_ID per PRCH_ID combination
  if (length(unique(dt$VEPC_ID)) > nrow(unique(dt[,list(PRCH_ID)]))) {
    dups <- dt[, list(COUNT = length(unique(VEPC_ID))), by = list(PRCH_ID)][COUNT > 1]
    dups <- paste0(dups[,paste0(PRCH_ID)], collapse = "\n")
    warning(paste0("Multiple VEPC_ID exist for the following :\nPRCH_ID\n", dups, "Only the max VEPC_ID per combination will be kept."))
    keep <- dt[, list(VEPC_ID = max(VEPC_ID)), by = list(PRCH_ID)][,list(VEPC_ID)]
    dt <- dt[keep]
  }

  dt <- data.table::dcast(dt, INTE_NO + POAS_NO + PRCH_NO + PRCH_ID + PROD_CODE ~ QUESTION, value.var = "REPONSE")

  data.table::setnames(dt, "14367", "COMAUTBA", skip_absent = TRUE)
  numcol  <- c("SUPERREZ", "RVEXTBET", "RVEXTBRI", "PRINCFUS", "MTTOTRAS")
  suppressWarnings(dt[, (numcol) := lapply(.SD, as.integer), .SDcols = numcol])

  dbDisconnect(con)

  return(dt)
}

#' @export
#' @title Get risks CGEN from extractnetezza
#' @description Query to extract inforce commercial policies details for cgen to use in joint risks evaluation.
#' @return A data.table object with IDs and columns.
#' @importFrom data.table setnames
get_risks_cgen <- function() {
  options(extractnetezza.context = 11)
  dt <- extractnetezza::get_policies(
    inforce = TRUE,
    partial = "PRO",
    filters = list(MPROD_ID = c(2552251, 1071124, 1071125, 1071122),
                   MLIAF_ID = 4,
                   MCAAF_ID = 2),
    detailid = c(140, 959, 971, 1083, 1092, 9045, 9406, 9408, 14218, 14219, 14220, 14367, 14491, 14650, 14660, 14661)
  )
  data.table::setnames(dt, gsub("PROD_", "", names(dt)))
  data.table::setnames(dt, c("MINTE_ID", "MPRCH_ID", "PRODUIT", "14367"), c("INTE_NO", "PRCH_ID", "PROD_CODE", "COMAUTBA"), skip_absent = TRUE)
  numcol  <- c("SUPERREZ", "RVEXTBET", "RVEXTBRI", "PRINCFUS", "MTTOTRAS")
  suppressWarnings(dt[, (numcol) := lapply(.SD, as.integer), .SDcols = numcol])
  return(dt[,list(INTE_NO, POAS_NO, PRCH_NO, PRCH_ID, PROD_CODE, COMAUTBA,
                  AFFECTAT, LATITCOM, LONGICOM, MTTOTRAS, MURSTRUC, PLANCHER,
                  PREGEOCO, PRINCFUS, RESAUFEU, RISASGRB, RVEXTBET, RVEXTBRI,
                  SUPERREZ, TOITSTRU, UMESSUP2)])
}

#' @export
#' @title Get risks CGEN from a file
#' @description Query to extract inforce commercial policies details for cgen to use in joint risks evaluation.
#' @return A data.table object with IDs and columns.
#' @importFrom data.table setDT fread setnames
load_risk_cgen <- function(file) {

  dt <- data.table::fread(file)
  # Uncomment when we have a working prototype with GCP buckets
  # data.table::setDT(dt, key = "VEPC_ID")
  #
  # # Check that there is only one VEPC_ID per PRCH_ID combination
  # if (length(unique(dt$VEPC_ID)) > nrow(unique(dt[,list(PRCH_ID)]))) {
  #   dups <- dt[, list(COUNT = length(unique(VEPC_ID))), by = list(PRCH_ID)][COUNT > 1]
  #   dups <- paste0(dups[,paste0(PRCH_ID)], collapse = "\n")
  #   warning(paste0("Multiple VEPC_ID exist for the following :\nPRCH_ID\n", dups, "Only the max VEPC_ID per combination will be kept."))
  #   keep <- dt[, list(VEPC_ID = max(VEPC_ID)), by = list(PRCH_ID)][,list(VEPC_ID)]
  #   dt <- dt[keep]
  # }
  #
  # dt <- data.table::dcast(dt, INTE_NO + POAS_NO + PRCH_NO + PRCH_ID + PROD_CODE ~ QUESTION, value.var = "REPONSE")
  #
  # data.table::setnames(dt, "14367", "COMAUTBA", skip_absent = TRUE)
  # numcol  <- c("SUPERREZ", "RVEXTBET", "RVEXTBRI", "PRINCFUS", "MTTOTRAS")
  # suppressWarnings(dt[, (numcol) := lapply(.SD, as.integer), .SDcols = numcol])

  return(dt)

}

#' @title Append TYPECONS
#' @description Append TYPECONS, TYCONS2 compo to its input.
#' @return A data.table with TYPECONS and TYCONS2 added and MURSTRUC, PLANCHER, TOITSTRU, REVETEME, RESAUFEU,
#' RVEXTBET, RVEXTBRI removed.
#' @param dt A suitable dt created used one the functions in this package (get_... or load_...)
#' @importFrom data.table set
append_typecons <- function(dt) {

  data.table::set(dt,
                  j = "REVETEME",
                  value = c("N", "O")[1L + as.integer(rowSums(dt[,list(RVEXTBET, RVEXTBRI)], na.rm = TRUE) > 59L)])

  dt[compo$COTYCONS,
     on = list(MURSTRUC = MURSTRUC,
               PLANCHER = PLANCHER,
               TOITSTRU = TOITSTRU,
               REVETEME = REVETEME),
     `:=`(TYPECONS = as.integer(TYPECONS))]

  dt[compo$COTYCON2,
     on = list(RESAUFEU = RESAUFEU),
     `:=`(TYCONS2 = as.integer(TYCONS2))]

  return(dt)

}

#' @title Transformers, more than meet the eyes
#' @description This function tranforms the data...obviously.
#' @return The same data.table with some alterations.
#' @param dt Output from append_typecons or from a json single input query in a data.table format
#' @importFrom data.table set
transform_cgen <- function(dt) {

  data.table::set(dt,
                  j = c("LATITCOM", "LONGICOM", "TYPECONS", "SUPERREZ", "PRINCFUS"),
                  value = list(
                    as.numeric(.subset2(dt, "LATITCOM")),
                    as.numeric(.subset2(dt, "LONGICOM")),
                    {x <- .subset2(dt, "TYPECONS"); y <- .subset2(dt, "TYCONS2");
                     nax <- which(is.na(x)); nay <- which(is.na(y[nax]));
                     x[nax] <- y[nax]; x[nax[nay]] <- TYPECONS_DEFAULT;
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

#' @title Calculate radius
#' @description This function calculates the radius for a risk in order to create neighbors risks pockets
#' later on. It is based on many risk characteristics.
#' @return A data.table with an added RISKRADIUS column.
#' @param dt Transformed output in a data.table format produced by transform_...
#' @importFrom data.table set %chin%
calculate_radius <- function(dt) {

  data.table::set(dt,
                  j = "RISKRADIUS",
                  value = list({
                    RISASGRB_F          <- c(1L, 2L)[1L + as.integer(.subset2(dt, "RISASGRB") %chin% c("5", "6", "P"))];
                    COMAUTBA_F          <- c(1, 1.25)[1L + as.integer(.subset2(dt, "COMAUTBA") %in% "O")];
                    AFFECTAT_F          <- c(1, 1.25)[1L + as.integer(.subset2(dt, "AFFECTAT") %in% "C6670")];
                    M_OR_P_NA           <- 1L + as.integer(.subset2(dt, "UMESSUP2") %in% c("PI", "*", NA));
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
set_binding <- function(binding, value) {
  e <- environment()
  unlockBinding(binding, parent.env(e))
  assign(binding, value, parent.env(e))
  lockBinding(binding, parent.env(e))
}

# Basic in memory polygons structure to match the one created with create polygons but with
# empty rows
suppressWarnings(
.inmempoly <- list(
  "polygons" =
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
      3488), dist = 1L)), "POLYGON"),
  "boundaries" =
    data.table::data.table(xmin = numeric(), ymin = numeric(), xmax = numeric(), ymax = numeric()),
  "details" =
    data.table::data.table(INTE_NO = integer(), POAS_NO = integer(), PRCH_NO = integer(),
                           PRCH_ID = integer(), MTTOTRAS = integer(), LONGICOM = numeric(),
                           LATITCOM = numeric(), RISKRADIUS = numeric(), RISASGRB = character(),
                           POLYGON_INDEX = integer())))


#' @title Polygon-o-tron
#' @description This function takes a data containing IDs, radius and geocode and creates pockets
#' for neighbors risks. It returns a data containing the pocket number for each PRCH_ID
#' @param dt Transformed output with a calculated radius in a data.table format
#' @return A data.table .
#' @importFrom data.table set %chin% setkey as.data.table rbindlist
#' @importFrom sf st_as_sf st_buffer st_union st_cast st_intersects st_transform st_bbox
#' @importFrom uuid UUIDgenerate
create_polygons <- function(dt) {

 details <- dt[!is.na(LATITCOM) & !is.na(LONGICOM) & PREGEOCO %chin% c("POINTADDRESS","STREETADDRESS","SUBADDRESS"),
              list(INTE_NO, POAS_NO, PRCH_NO, PRCH_ID, MTTOTRAS, LONGICOM, LATITCOM, RISKRADIUS, RISASGRB)]

 dt_sp <- sf::st_as_sf(details,
                       coords = c("LONGICOM", "LATITCOM"),
                       crs = "+proj=longlat +datum=WGS84")
 dt_sp <- sf::st_transform(dt_sp, 3488)
 circles <- sf::st_buffer(dt_sp, dist = details$RISKRADIUS)

 polygons <- sf::st_cast(sf::st_union(circles), "POLYGON")
 boundaries <- data.table::rbindlist(lapply(polygons, function(x) {as.list(sf::st_bbox(x))}))
 details$POLYGON_INDEX <- data.table::as.data.table(sf::st_intersects(dt_sp, polygons))$col.id

 set_binding(".inmempoly", list("polygons" = polygons, "boundaries" = boundaries, "details" = details))

 return(invisible())

}

#' @export
#' @title Obtain polygons
#' @description This function is used to update and return polygon table.
#' @param source A data.table used to update the polygon. Use load_risk_cgen(file) or
#' get_cgen_risk() (require extractnetezza) or get_cgen_risk_oracle() to source from
#' Oracle INTG (require ROracle).
#' @return A data.table with PRCH_ID and POLYGON_ID.
update_polygons <- function(source) {
  tick <- Sys.time()
  prev_npol <- nrow(.inmempoly$boundaries)
  prev_ndet <- nrow(.inmempoly$details)
  append_typecons(source)
  transform_cgen(source)
  calculate_radius(source)
  create_polygons(source)
  next_npol <- nrow(.inmempoly$boundaries)
  next_ndet <- nrow(.inmempoly$details)
  tock <- Sys.time() - tick
  cat("Polygons definition updated on ", paste0(Sys.getenv(c("COMPUTERNAME", "HOSTNAME")), collapse = ""),
      " in ", format(unclass(tock), digits = 4)," ", attr(tock, "units"),". Previous definition had ",
      prev_npol," polygons from ", prev_ndet," risks, current has ", next_npol," polygons from ",
      next_ndet," risks.\n", sep = "")
  return(invisible())
}

#' @export
#' @title Polygon blaster
#' @description This function return polygon id from an input data.table using
#' the precomputed pockets.
#' @param dt A data.frame with the following
#' @return PRCH_ID and POLYGON_ID with package version.
#' @importFrom data.table set setDT %chin% rbindlist
#' @importFrom sf st_as_sf st_buffer st_union st_cast st_intersects st_transform st_bbox
#' @examples
#' \dontrun{
#' dt <- jsonlite::fromJSON('[{"PRCH_ID":14543671,"COMAUTBA":"NA",
#' "AFFECTAT":"C8112","LATITCOM":"45.6388","LONGICOM":"-73.8438",
#' "MTTOTRAS":940000,"PRINCFUS":4,"RISASGRB":"1","SUPERREZ":1800,
#' "UMESSUP2":"PI","TYPECONS":5,"TYCONS2":"NA","RISKRADIUS":18.2958}]')
#' get_polygons_id(dt)
#' }
get_polygon_id <- function(dt) {
  data.table::setDT(dt)
  required <- c("PRCH_ID", "COMAUTBA", "AFFECTAT", "LATITCOM", "LONGICOM", "MTTOTRAS",
                "PRINCFUS", "RISASGRB", "SUPERREZ", "UMESSUP2", "TYPECONS", "TYCONS2")
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
  b <- as.list(sf::st_bbox(newrisk))
  candidate_polys_idx <- .inmempoly$boundaries[b$xmin < xmax & b$xmax > xmin & b$ymin < ymax & b$ymax > ymin, which = TRUE]
  matches <- sf::st_intersects(newrisk, .inmempoly$polygons[candidate_polys_idx])

  # Formuler une reponse de retour qui fait du sens
  # Enlever les risques conjoints avec lui-même (meme PRCH_ID ou POL_NO, PRCH_NO)
  # Qu'est-ce qui arrive sur un copier-produit?
  # max RISASGRB, sum MTTOTRAS

  res <- lapply(seq_len(length(matches)), function(x) {
    prch_id <- .subset2(dt, "PRCH_ID")[x]
    jr <- .inmempoly$details[POLYGON_INDEX %in% candidate_polys_idx[matches[[x]]] & PRCH_ID != prch_id]
    tiv <- sum(.subset2(dt, "MTTOTRAS")[x], .subset2(jr, "MTTOTRAS"), na.rm = TRUE)
    maxgrb <- max(.subset2(dt, "RISASGRB")[x], .subset2(jr, "RISASGRB"), na.rm = TRUE)
    set(jr, j = c("LONGICOM", "LATITCOM", "RISKRADIUS", "POLYGON_INDEX"), value = NULL)
    list(
      "PRCH_ID" = prch_id,
      "TIV" = tiv,
      "MAXGRB" = maxgrb,
      "JOINTRISKS" = jr
    )})

  return(res)
}

#' Warmup vectorizer functions for optimized runs.
#' @importFrom jsonlite fromJSON
#' @export
warmup <- function() {
  dt <- jsonlite::fromJSON('[{"PRCH_ID":14543672,"COMAUTBA":"NA","AFFECTAT":"C8112","LATITCOM":"45.6388",
                            "LONGICOM":"-73.8438","MTTOTRAS":940000,"PRINCFUS":4,"RISASGRB":"1","SUPERREZ":1800,"UMESSUP2":"PI",
                            "TYPECONS":5,"TYCONS2":"NA","RISKRADIUS":18.2958}]')
  get_polygons_id(dt)
  return(invisible())
}
