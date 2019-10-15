# Constant values
TYPECONS_DEFAULT <- 6L;   RISASGRB_DEFAULT <- "4";  SUPERREZ_DEFAULT <- 1L
PRINCFUS_DEFAULT <- 7L;   RADIUSMT_DEFAULT <- 9L;   GRDFLRAREA_M_LIM <- 10L
GRDFLRAREA_P_LIM <- 200L; RADIUS_MFAD <- 2L

TYPECONS_x_PRINCFUS <- matrix(nrow = 6L, ncol = 10L)
TYPECONS_x_PRINCFUS[, 1L:4L] <- c(3L, 3L, 6L, 6L, 9L, 9L)
TYPECONS_x_PRINCFUS[, 5L:7L] <- c(4L, 4L, 8L, 8L, 12L, 12L)
TYPECONS_x_PRINCFUS[, 8L:10L] <- c(8L, 8L, 16L, 16L, 25L, 25L)

pkgV <- as.character(packageVersion("jointrisk"))
.inmempoly <- data.frame()

#' @title Append TYPECONS
#' @description Append TYPECONS, TYCONS2 compo at the end of input table.
#' @param dt A data.table containing required fields.
#' @return A data.table with TYPECONS and TYCONS2.
#' @importFrom data.table set
append_typecons <- function(dt) {
  data.table::set(dt,
                  j = "REVETEME",
                  value = c("N", "O")[1L + as.integer(rowSums(dt[,list(RVEXTBET, RVEXTBRI)], na.rm = TRUE) > 59L)])

  dt[compo$COTYCONS,
     on = c("MURSTRUC==MURSTRUC",
            "PLANCHER==PLANCHER",
            "TOITSTRU==TOITSTRU",
            "REVETEME"),
     `:=`("TYPECONS" = as.integer(TYPECONS))]

  dt[compo$COTYCON2,
     on = "RESAUFEU==RESAUFEU",
     `:=`("TYCONS2" = as.integer(TYCONS2))]

  return(dt)

}

#' @title Calculate radius
#' @description This function calculates the radius for a risk in order to create neighbors risks pockets
#' later on. It is based on many risk characteristics.
#' @return A data.table with an added output column.
#' @param dt A data.table containing required fields.
#' @importFrom data.table set %chin%
calculate_radius <- function(dt) {
  data.table::set(dt,
                  j = "RISKRADIUS",
                  value = list({
                    # Insured risk property class construct
                    RISASGRB            <- .subset2(dt, "RISASGRB")
                    nax                 <- which(is.na(RISASGRB))
                    RISASGRB[nax]       <- RISASGRB_DEFAULT
                    RISASGRB_F          <- c(1L, 2L)[1L + as.integer(RISASGRB %chin% c("5", "6", "P"))]

                    COMAUTBA_F          <- c(1, 1.25)[1L + as.integer(.subset2(dt, "COMAUTBA") %in% "O")]
                    AFFECTAT_F          <- c(1, 1.25)[1L + as.integer(.subset2(dt, "AFFECTAT") %in% "C6670")]
                    M_OR_P_NA           <- 1L + as.integer(.subset2(dt, "UMESSUP2") %in% c("PI", "*", NA))
                    CONVERSION_F        <- c(1, 10.764)[M_OR_P_NA]
                    LOWERLIMIT_F        <- c(GRDFLRAREA_M_LIM, GRDFLRAREA_P_LIM)[M_OR_P_NA]
                    # Construction type construct
                    TYPECONS            <- .subset2(dt, "TYPECONS")
                    TYCONS2             <- .subset2(dt, "TYCONS2")
                    nax                 <- which(is.na(TYPECONS))
                    nay                 <- which(is.na(TYCONS2[nax]))
                    TYPECONS[nax]       <- TYCONS2[nax]
                    TYPECONS[nax[nay]]  <- TYPECONS_DEFAULT
                    # Fire underwriters survey fire protectection zone
                    PRINCFUS            <- .subset2(dt, "PRINCFUS")
                    nax                 <- which(is.na(PRINCFUS))
                    PRINCFUS[nax]       <- PRINCFUS_DEFAULT
                    # Combined factor
                    PRINCFUS_TYPECONS_F <- TYPECONS_x_PRINCFUS[TYPECONS + (PRINCFUS - 1L) * 6L]

                    RADIUS              <- rep(RADIUSMT_DEFAULT, nrow(dt))
                    # Ground floor area NA replacement
                    GRDFLRAREA          <- .subset2(dt, "SUPERREZ")
                    nax                 <- which(is.na(GRDFLRAREA))
                    GRDFLRAREA[nax]     <- SUPERREZ_DEFAULT

                    IND                 <- (GRDFLRAREA >= LOWERLIMIT_F)
                    RADIUS[IND]         <- sqrt(GRDFLRAREA[IND]/CONVERSION_F[IND]/pi)
                    (RADIUS + RADIUS_MFAD + PRINCFUS_TYPECONS_F * RISASGRB_F) * COMAUTBA_F * AFFECTAT_F
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

#' @title Polygon-o-tron
#' @description This function takes a data containing IDs, radius and geocode and creates pockets
#' for neighbors risks. It returns a data containing the pocket number for each PRCH_ID
#' @param dt A data.table containing required fields.
#' @return A data.table.
#' @importFrom data.table set %chin% setkey as.data.table rbindlist
#' @importFrom sf st_as_sf st_buffer st_union st_cast st_intersects st_transform st_bbox
create_polygons <- function(dt) {

 polygons <- dt[!is.na(LATITCOM) &
                !is.na(LONGICOM) &
                PREGEOCO %chin% c("POINTADDRESS","STREETADDRESS","SUBADDRESS")]

 polygons <- sf::st_as_sf(polygons,
                          coords = c("LONGICOM", "LATITCOM"),
                          crs = "+proj=longlat +datum=WGS84")
 polygons <- sf::st_transform(polygons, 3488)
 polygons <- sf::st_buffer(polygons, dist = .subset2(polygons, "RISKRADIUS"))
 classes <- attr(polygons, "class")
 # reclass as data.table to allow data.table operation
 setDT(polygons)
 data.table::set(polygons,
     j = c("xmin", "ymin", "xmax", "ymax"),
     value = as.list(data.table::rbindlist(
       lapply(
         .subset2(polygons, "geometry"),
         function(x) {
           as.list(sf::st_bbox(x))
           }
         )
       )
     ))
 # and back into an sf
 attr(polygons, "class") <- classes

 return(polygons)

}

#' @export
#' @title Obtain polygons
#' @description This function is used to update and return polygon table.
#' @param source A data.table used to update the polygon. Use load_risk_cgen(file) or
#' get_cgen_risk() (require extractnetezza).
#' @return A data.table with PRCH_ID and POLYGON_ID.
update_polygons <- function(source) {
  tick <- Sys.time()
  prev_npol <- nrow(.inmempoly)
  append_typecons(source)
  calculate_radius(source)
  polygons <- create_polygons(source)
  set_binding(".inmempoly", polygons)
  next_npol <- nrow(.inmempoly)
  tock <- Sys.time() - tick
  cat("Polygons definition updated on ", paste0(Sys.getenv(c("COMPUTERNAME", "HOSTNAME")), collapse = ""),
      " in ", format(unclass(tock), digits = 4)," ", attr(tock, "units"),". Previous definition had ",
      prev_npol," polygons, current has ", next_npol," polygons.\n", sep = "")
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
get_joint_risks <- function(dt) {
  if (nrow(.inmempoly) == 0L) {
    res <- list("WARNING" = "Empty polygons definition.")
  } else {
    data.table::setDT(dt)
    required <- c("PRCH_ID", "COMAUTBA", "AFFECTAT", "LATITCOM", "LONGICOM", "MTTOTRAS",
                  "PRINCFUS", "RISASGRB", "SUPERREZ", "UMESSUP2", "TYPECONS", "TYCONS2")
    if (!all(required %chin% names(dt))) {
      notin <- required[!required %chin% names(dt)]
      stop(paste("Required fields", paste(notin, collapse = ", "), "not found"))
    }
    calculate_radius(dt)
    dt_sp <- sf::st_as_sf(dt,
                          coords = c("LONGICOM", "LATITCOM"),
                          crs = "+proj=longlat +datum=WGS84")
    dt_sp <- sf::st_transform(dt_sp, 3488)
    newrisk <- sf::st_buffer(dt_sp, dist = .subset2(dt, "RISKRADIUS"))
    b <- as.list(sf::st_bbox(newrisk))
    poly_idx <- which(.subset2(b, "xmin") < .subset2(.inmempoly, "xmax") &
                      .subset2(b, "xmax") > .subset2(.inmempoly, "xmin") &
                      .subset2(b, "ymin") < .subset2(.inmempoly, "ymax") &
                      .subset2(b, "ymax") > .subset2(.inmempoly, "ymin"))
    matches <- sf::st_intersects(newrisk, .inmempoly[poly_idx, with = FALSE])

    # Formuler une reponse de retour qui fait du sens
    # Enlever les risques conjoints avec lui-même (meme PRCH_ID ou POL_NO, PRCH_NO)
    # Qu'est-ce qui arrive sur un copier-produit?
    # max RISASGRB, sum MTTOTRAS

    prch_id <- .subset2(dt, "PRCH_ID")
    mttotras <- .subset2(dt, "MTTOTRAS")

    res <- lapply(seq_len(length(matches)), function(x) {
      self_idx <- which(prch_id[x] == .subset2(.inmempoly, "PRCH_ID"))
      jr_idx <- candidate_polys_idx[matches[[x]]]
      jr <- setDT(copy(.inmempoly[jr_idx[!jr_idx %in% self_idx], with = FALSE]))[, list(PRCH_ID, INTE_NO, POAS_NO, PRCH_NO, MTTOTRAS, RISASGRB)]
      tiv <- sum(mttotras[x], .subset2(jr, "MTTOTRAS"), na.rm = TRUE)
      maxgrb <- max(.subset2(dt, "RISASGRB")[x], .subset2(jr, "RISASGRB"), na.rm = TRUE)
      set(jr, j = c("LONGICOM", "LATITCOM", "RISKRADIUS", "POLYGON_INDEX"), value = NULL)
      list(
        "PRCH_ID" = prch_id[x],
        "TIV" = tiv,
        "MAXGRB" = maxgrb,
        "JOINTRISKS" = jr
      )})
  }
  return(res)
}

#' @export
#' @title Add polygon index
#' @description Adds a polygon index to a data.table of commercial risks.
#' Use the others parameters for custom input names but all of .
#' @param dt A data.table to which you want to add polygon index. If the field name
#' in the table does not match the default value, just change the parameter for
#' that field. Field value are equivalence value (COOP_ID 11).
#' @param prefix A character string. In case the fields just need a prefix (something like "PROD_ID"). Default to "".
#' @param affectat A character string. Default to paste0(prefix, "AFFECTAT").
#' @param comautba A character string. Default to paste0(prefix, "COMAUTBA").
#' @param latitcom A character string. Default to paste0(prefix, "LATITCOM").
#' @param longicom A character string. Default to paste0(prefix, "LONGICOM").
#' @param murstruc A character string. Default to paste0(prefix, "MURSTRUC").
#' @param plancher A character string. Default to paste0(prefix, "PLANCHER").
#' @param pregeoco A character string. Default to paste0(prefix, "PREGEOCO").
#' @param princfus A character string. Default to paste0(prefix, "PRINCFUS").
#' @param resaufeu A character string. Default to paste0(prefix, "RESAUFEU").
#' @param risasgrb A character string. Default to paste0(prefix, "RISASGRB").
#' @param rvextbet A character string. Default to paste0(prefix, "RVEXTBET").
#' @param rvextbri A character string. Default to paste0(prefix, "RVEXTBRI").
#' @param superrez A character string. Default to paste0(prefix, "SUPERREZ").
#' @param toitstru A character string. Default to paste0(prefix, "TOITSTRU").
#' @param umessup2 A character string. Default to paste0(prefix, "UMESSUP2").
#' @return A data.table with paste0(prefix, "POLYINDX") column. If the risk has no
#' appropriate geolocation information, index will be NA.
#' @importFrom sf st_cast st_intersects st_union
#' #' @examples
#' \dontrun{
#' dt <- extractnetezza::get_policies(
#'     inforce = TRUE,
#'     partial = "PRO",
#'     filters = list(MPROD_ID = c(2552251, 1071124, 1071125, 1071122),
#'                    MLIAF_ID = 4,
#'                    MCAAF_ID = 2),
#'     detailid = c(140, 959, 971, 1083, 1092, 9045, 9406, 9408, 14218, 14219, 14220, 14367, 14491, 14650, 14660, 14661)
#'   )
#' append_polygons_idx(dt, prefix = "PROD_", comautba = "PROD_14367")
#' }
append_polygons_idx <- function(dt,
                                prefix = "",
                                affectat = paste0(prefix, "AFFECTAT"),
                                comautba = paste0(prefix, "COMAUTBA"),
                                latitcom = paste0(prefix, "LATITCOM"),
                                longicom = paste0(prefix, "LONGICOM"),
                                murstruc = paste0(prefix, "MURSTRUC"),
                                plancher = paste0(prefix, "PLANCHER"),
                                pregeoco = paste0(prefix, "PREGEOCO"),
                                princfus = paste0(prefix, "PRINCFUS"),
                                resaufeu = paste0(prefix, "RESAUFEU"),
                                risasgrb = paste0(prefix, "RISASGRB"),
                                rvextbet = paste0(prefix, "RVEXTBET"),
                                rvextbri = paste0(prefix, "RVEXTBRI"),
                                superrez = paste0(prefix, "SUPERREZ"),
                                toitstru = paste0(prefix, "TOITSTRU"),
                                umessup2 = paste0(prefix, "UMESSUP2")) {
  source <- copy(dt[, c(affectat, comautba, latitcom, longicom, murstruc, plancher,
                            pregeoco, princfus, resaufeu, risasgrb, rvextbet,
                            rvextbri, superrez, toitstru, umessup2), with = FALSE])
  setnames(source,
           c(affectat, comautba, latitcom, longicom, murstruc, plancher, pregeoco, princfus,
             resaufeu, risasgrb, rvextbet, rvextbri, superrez, toitstru, umessup2),
           c("AFFECTAT", "COMAUTBA", "LATITCOM", "LONGICOM", "MURSTRUC", "PLANCHER", "PREGEOCO", "PRINCFUS",
             "RESAUFEU", "RISASGRB", "RVEXTBET", "RVEXTBRI", "SUPERREZ", "TOITSTRU", "UMESSUP2"))
  set(source, j = "MERGEKEY", value = seq_len(nrow(source)))
  jointrisk:::append_typecons(source)
  jointrisk:::calculate_radius(source)
  polygons <- jointrisk:::create_polygons(source)
  pockets <- sf::st_cast(sf::st_union(polygons), "POLYGON")
  idx <- sf::st_intersects(polygons, pockets)
  setDT(polygons)
  set(polygons, j = "POLYINDX", value = unlist(idx))
  setDT(dt)
  set(dt,
      i = .subset2(polygons, "MERGEKEY"),
      j = paste0(prefix, "POLYINDX"),
      value = .subset2(polygons, "POLYINDX"))
  return(invisible())
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
load_risk_cgen <- function() {

  file <- getOption("jointrisk.riskfile")

  dt <- data.table::fread(file)
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

  return(dt)

}

#' Warmup vectorizer functions for optimized runs.
#' @importFrom jsonlite fromJSON
#' @export
warmup <- function() {
  dt <- jsonlite::fromJSON('[{"PRCH_ID":82804587,"COMAUTBA":"NA","AFFECTAT":"C8085","LATITCOM":"46.77418",
                            "LONGICOM":"-71.30196","MTTOTRAS":"NA","PRINCFUS":"NA","RISASGRB":"NA","SUPERREZ":"NA","UMESSUP2":"PI",
                            "TYPECONS":2,"TYCONS2":"NA","RISKRADIUS":15.00000}]')
  get_joint_risks(dt)
  return(invisible())
}
