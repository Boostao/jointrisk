# Constant values
TYPECONS_DEFAULT <- 6L;   RISASGRB_DEFAULT <- "4";  SUPERREZ_DEFAULT <- 1L
PRINCFUS_DEFAULT <- 7L;   RADIUSMT_DEFAULT <- 9L;   GRDFLRAREA_M_LIM <- 10L
GRDFLRAREA_P_LIM <- 200L; RADIUS_MFAD <- 2L;        LANE_WIDTH <- 2L;
COMAUBAT_DEFAULT <- "N";

TYPECONS_x_PRINCFUS <- matrix(nrow = 6L, ncol = 10L)
TYPECONS_x_PRINCFUS[, 1L:4L] <- c(3L, 3L, 6L, 6L, 9L, 9L)
TYPECONS_x_PRINCFUS[, 5L:7L] <- c(4L, 4L, 8L, 8L, 12L, 12L)
TYPECONS_x_PRINCFUS[, 8L:10L] <- c(8L, 8L, 16L, 16L, 25L, 25L)

pkgV <- as.character(packageVersion("jointrisk"))

#' @title Append TYPECONS
#' @description Append TYPECONS, TYCONS2 compo at the end of input table.
#' @param dt A data.table containing required fields.
#' @return A data.table with TYPECONS and TYCONS2.
#' @import data.table
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
  
  dt[is.na(TYPECONS), "TYPECONS" := TYCONS2]
  dt[, "TYCONS2" := NULL]

  return(dt)

}

#' @title Calculate radius
#' @description This function calculates the radius for a risk in order to create neighbors risks pockets
#' later on. It is based on many risk characteristics.
#' @return A data.table with an added output column.
#' @param dt A data.table containing required fields.
calculate_radius <- function(dt) {
  data.table::set(dt,
                  j = "RISKRADIUS",
                  value = list({
                    # Insured risk property class construct
                    RISASGRB            <- .subset2(dt, "RISASGRB")
                    if (is.null(RISASGRB)) {
                      RISASGRB <- rep(RISASGRB_DEFAULT, nrow(dt))
                    } else {
                      nax                 <- which(is.na(RISASGRB))
                      RISASGRB[nax]       <- RISASGRB_DEFAULT
                    }
                    RISASGRB_F          <- c(1L, 2L)[1L + as.integer(RISASGRB %chin% c("5", "6", "R", "P"))]
                    COMAUBAT            <- .subset2(dt, "COMAUBAT")
                    if (is.null(COMAUBAT)) {
                      COMAUBAT <- rep(COMAUBAT_DEFAULT, nrow(dt))
                    } else {
                      nax                 <- which(is.na(COMAUBAT))
                      COMAUBAT[nax]       <- COMAUBAT_DEFAULT
                    }
                    COMAUBAT_F          <- c(1, 1.25)[1L + as.integer(COMAUBAT %in% "O")]
                    AFFECTAT_F          <- c(1, 1.25)[1L + as.integer(grepl("6670$", .subset2(dt, "AFFECTAT")))]
                    M_OR_P_NA           <- 1L + as.integer(!.subset2(dt, "UMESSUPE") %in% c("M", "ME"))
                    CONVERSION_F        <- c(1, 10.764)[M_OR_P_NA]
                    LOWERLIMIT_F        <- c(GRDFLRAREA_M_LIM, GRDFLRAREA_P_LIM)[M_OR_P_NA]
                    # Construction type construct
                    TYPECONS            <- .subset2(dt, "TYPECONS")
                    nax                 <- which(is.na(TYPECONS))
                    TYPECONS[nax]       <- TYPECONS_DEFAULT
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
                    (RADIUS + RADIUS_MFAD + PRINCFUS_TYPECONS_F * RISASGRB_F) * COMAUBAT_F * AFFECTAT_F
                  }))
}

#' @title Add buffer and remove streets polygons
#' @description This function remove streets from an sf table of POINTS polygons.
#' @return An sf object
#' @param polys An sf object of POLYGON.
#' @param streets An sf object of LINESTRING with LANE_RADIUS.
#' @importFrom sf st_buffer st_bbox st_cast st_difference st_union st_nearest_feature
buff_and_remove_streets <- function(polys, streets) {
  if (nrow(polys) < 200) {
    return(buff_and_remove_streets_api(polys, streets))
  } else {
    return(buff_and_remove_streets_batch(polys, streets))
  }
}

#' @title Add buffer and remove streets polygons API optimized version
#' @description This function remove streets from an sf table of POINTS polygons.
#' @return An sf object
#' @param polys An sf object of POLYGON.
#' @param streets An sf object of LINESTRING with LANE_RADIUS.
#' @importFrom sf st_buffer st_bbox st_cast st_difference st_union st_nearest_feature st_crs
#' @importFrom units set_units
buff_and_remove_streets_api <- function(polys, streets) {
 pts <- polys$geometry
 polys <- sf::st_set_precision(
   sf::st_buffer(polys, dist = .subset2(polys, "RISKRADIUS"), nQuadSegs = 3),
   units::set_units(10, nm)
 )
 cls_ori <- attr(polys, "class"); setDT(polys);
 data.table::set(
   polys,
   j = c("xmin", "ymin", "xmax", "ymax"),
   value = as.list(data.table::rbindlist(lapply(polys$geometry, function(x) as.list(sf::st_bbox(x)))))
 )
 xmax <- .subset2(streets, "xmax"); xmin <- .subset2(streets, "xmin")
 ymax <- .subset2(streets, "ymax"); ymin <- .subset2(streets, "ymin")
 base_crs <- sf::st_crs(polys$geometry)
 for (i in 1L:nrow(polys)) {
   poly1 <- polys[i,]
   idx <- which(.subset2(poly1, "xmin") < xmax &
                .subset2(poly1, "xmax") > xmin &
                .subset2(poly1, "ymin") < ymax &
                .subset2(poly1, "ymax") > ymin)
   if (length(idx) > 0L) {
     area_streets <- streets[idx,]
     area_streets <- sf::st_set_precision(
       sf::st_buffer(
         area_streets$geometry,
         dist = LANE_WIDTH * .subset2(area_streets, "LANE_RADIUS"),
         nQuadSegs = 3
       ),
       units::set_units(10, nm)
     )
     new_geo <- sf::st_cast(
       sf::st_sfc(
         sf:::CPL_geos_op2("difference",
                           poly1$geometry,
                           sf::st_sfc(sf:::CPL_geos_union(area_streets))),
         crs = base_crs
       )
       , "POLYGON")
     new_geo <- new_geo[sf:::CPL_geos_nearest_feature(pts[i], new_geo)]
     if (lengths(new_geo)) {
       data.table::set(
         polys,
         i = i,
         j = "geometry",
         value = list("geometry" = new_geo)
       ) 
     }
   }
 }
 attr(polys, "class") <- cls_ori
 return(polys)
}

#' @title Add buffer and remove streets polygons batch optimized version (> 200 polygons).
#' @description This function remove streets from an sf table of POINTS polygons.
#' @return An sf object
#' @param polys An sf object of POLYGON.
#' @param streets An sf object of LINESTRING with LANE_RADIUS.
#' @importFrom sf st_buffer st_bbox st_cast st_difference st_union st_nearest_feature
buff_and_remove_streets_batch <- function(polys, streets) {
  pts <- polys$geometry
  polys <- sf::st_set_precision(
    sf::st_buffer(polys, dist = .subset2(polys, "RISKRADIUS"), nQuadSegs = 3),
    units::set_units(10, nm)
  )
  inter <- sf:::CPL_geos_binop(
    polys$geometry,
    streets$geometry,
    "intersects",
    pattern = NA_character_,
    prepared = TRUE
  )
  streets <- streets[unique(unlist(inter)),]
  inter <- sf:::CPL_geos_binop(
    polys$geometry,
    streets$geometry,
    "intersects",
    pattern = NA_character_,
    prepared = TRUE
  )
  streets <- sf::st_set_precision(
    sf::st_buffer(streets$geometry, dist = LANE_WIDTH * .subset2(streets, "LANE_RADIUS"), nQuadSegs = 3),
    units::set_units(10, nm)
  )
  cls_ori <- attr(polys, "class"); setDT(polys)
  base_crs <- sf::st_crs(polys$geometry)
  for (i in 1L:nrow(polys)) {
    idx <- inter[[i]]
    if (length(idx) > 0L) {
      poly1 <- polys[i,]
      area_streets <- streets[idx]
      new_geo <- sf::st_cast(
        sf::st_sfc(
          sf:::CPL_geos_op2("difference",
                            poly1$geometry,
                            sf::st_sfc(sf:::CPL_geos_union(area_streets))),
          crs = base_crs
        )
        , "POLYGON")
      new_geo <- new_geo[sf:::CPL_geos_nearest_feature(pts[i], new_geo)]
      if (lengths(new_geo)) {
        data.table::set(
          polys,
          i = i,
          j = "geometry",
          value = list("geometry" = new_geo)
        )
      }
    }
  }
  attr(polys, "class") <- cls_ori
  return(polys)
}

#' @title Polygon-o-tron
#' @description This function takes a data containing IDs, radius and geocode and creates pockets
#' for neighbors risks. It returns a data containing the pocket number for each PRCH_ID
#' @param dt A data.table containing required fields.
#' @param streets A LINESTRING object of navstreets with LANE_RADIUS and geometry elements.
#' @return A data.table.
#' @importFrom sf st_as_sf st_buffer st_union st_cast st_intersects st_transform st_bbox
create_polygons <- function(dt, streets) {
 on.exit(gc())
 dt <- as.data.frame(dt)
 polygons <- dt[!is.na(dt$LATITCOM) &
                !is.na(dt$LONGICOM) &
                dt$PREGEOCO %in% c("POINTADDRESS","STREETADDRESS","SUBADDRESS"),]
 rm(dt)
 polygons <- sf::st_as_sf(polygons,
                          coords = c("LONGICOM", "LATITCOM"),
                          crs = "+proj=longlat +datum=WGS84",
                          remove = FALSE)
 polygons <- sf::st_transform(polygons, 3488)
 polygons <- buff_and_remove_streets(polygons, streets)
 inter <- sf:::CPL_geos_binop(
   polygons$geometry,
   polygons$geometry,
   "intersects",
   pattern = NA_character_,
   prepared = TRUE)
 lngs <- lengths(inter)
 batch <- which(lngs>1)
 pockets <- sf::st_cast(sf::st_sfc(sf:::CPL_geos_union(polygons[batch,]$geometry)), "POLYGON")
 pockets <- c(pockets, polygons[which(lngs==1),]$geometry)
 idx <- sf:::CPL_geos_binop(
   polygons$geometry,
   pockets,
   "intersects",
   pattern = NA_character_,
   prepared = TRUE)
 classes <- attr(polygons, "class")
 # reclass as data.table to allow data.table operation
 setDT(polygons)
 data.table::set(
   polygons,
   j = c("xmin", "ymin", "xmax", "ymax"),
   value = as.list(data.table::rbindlist(lapply(pockets, function(x) as.list(sf::st_bbox(x))))[unlist(idx)])
 )
 data.table::set(
   polygons,
   j = "geometry",
   value = pockets[unlist(idx)]
 )
 # and back into an sf
 attr(polygons, "class") <- classes
 return(polygons)

}

#' @export
#' @title Obtain polygons
#' @description This function is used to update and return polygon table.
#' @param source A data.table used to update the polygon. Use load_risk_cgen(file) or
#' get_cgen_risk() (require extraw).
#' @param polygons A polygons ref to update from.
#' @param streets A LINESTRING object of navstreets with LANE_RADIUS and geometry elements.
#' @return A data.table with PRCH_ID and POLYGON_ID.
update_polygons <- function(source, polygons = NULL, streets = NULL) {
  tick <- Sys.time()
  prev_npol <- if (is.null(polygons)) {0L} else {nrow(polygons)}
  calculate_radius(source)
  polygons <- create_polygons(source, streets)
  next_npol <- nrow(polygons)
  tock <- Sys.time() - tick
  msg <- paste0("Polygons definition updated on ", paste0(Sys.getenv(c("COMPUTERNAME", "HOSTNAME")), collapse = ""),
      " in ", format(unclass(tock), digits = 4)," ", attr(tock, "units"),". Previous definition had ",
      prev_npol," polygons, current has ", next_npol," polygons.\n", sep = "")
  cat(msg)
  return(list(msg = msg, poly = polygons))
}

#' @export
#' @title Polygon blaster
#' @description This function return polygon id from an input data.table using
#' the precomputed pockets.
#' @param dt A data.frame with the following
#' @param polygons A data.frame to compare dt to.
#' @param streets A LINESTRING object of navstreets with LANE_RADIUS and geometry elements.
#' @return PRCH_ID and POLYGON_ID with package version.
#' @importFrom sf st_as_sf st_buffer st_union st_cast st_intersects st_transform st_bbox
#' @examples
#' \dontrun{
#' dt <- jsonlite::fromJSON('[{"PRCH_ID":14543671,"COMAUBAT":"NA",
#' "AFFECTAT":"C8112","LATITCOM":"46.77418", "LONGICOM":"-71.30196",
#' "PRINCFUS":4,"SUPERREZ":1800, "UMESSUPE":"PI","TYPECONS":5}]')
#' get_joint_risks(dt, polygons)
#' }
get_joint_risks <- function(dt, polygons, streets) {
  if (nrow(polygons) == 0L) {
    res <- list("WARNING" = "Empty polygons definition.")
  } else {
    data.table::setDT(dt)
    required_cgen <- c("PRCH_ID", "COMAUBAT", "AFFECTAT", "LATITCOM", "LONGICOM",
                       "PRINCFUS", "SUPERREZ", "UMESSUPE", "TYPECONS")
    required_ugen <- c("SIT_ID", "AFFECTAT", "LATITCOM", "LONGICOM",
                       "PRINCFUS", "SUPERREZ", "UMESSUPE", "TYPECONS")
    if (!all(required_cgen %chin% names(dt)) & !all(required_ugen %chin% names(dt))) {
      notin_cgen <- required_cgen[!required_cgen %chin% names(dt)]
      notin_ugen <- required_ugen[!required_ugen %chin% names(dt)]
      stop(paste0("Required fields for CGEN [", paste(notin_cgen, collapse = ", "),
                  "] / UGEN [", paste(notin_ugen, collapse = ", "), "] not found"))
    }
    numcol  <- c("SUPERREZ", "PRINCFUS", "TYPECONS")
    suppressWarnings(dt[, (numcol) := lapply(.SD, as.integer), .SDcols = numcol])
    calculate_radius(dt)
    dt_sp <- sf::st_as_sf(dt,
                          coords = c("LONGICOM", "LATITCOM"),
                          crs = "+proj=longlat +datum=WGS84")
    dt_sp <- sf::st_transform(dt_sp, 3488)
    newrisk <- buff_and_remove_streets(dt_sp, streets)
    b <- as.list(sf::st_bbox(newrisk))
    poly_idx <- which(.subset2(b, "xmin") < .subset2(polygons, "xmax") &
                      .subset2(b, "xmax") > .subset2(polygons, "xmin") &
                      .subset2(b, "ymin") < .subset2(polygons, "ymax") &
                      .subset2(b, "ymax") > .subset2(polygons, "ymin"))
    matches <- sf:::CPL_geos_binop(
      newrisk$geometry,
      polygons[poly_idx, with = FALSE]$geometry,
      "intersects",
      pattern = NA_character_,
      prepared = TRUE)
    # Formuler une reponse de retour qui fait du sens
    # Enlever les risques conjoints avec lui-même (meme PRCH_ID ou POL_NO, PRCH_NO)
    # Qu'est-ce qui arrive sur un copier-produit?
    # max RISASGRB

    ref_id <- grep("^PRCH_ID$|^SIT_ID$", names(dt), value = TRUE)
    id <- .subset2(dt, ref_id)

    res <- lapply(seq_len(length(matches)), function(x) {
      self_idx <- which(id[x] == .subset2(polygons, ref_id))
      jr_idx <- poly_idx[matches[[x]]]
      jr <- setDT(copy(polygons[jr_idx[!jr_idx %in% self_idx], with = FALSE]))[, list(SOURCE, PRCH_ID, INTE_NO, POAS_NO, PRCH_NO, SIT_ID, MTTOTRAS, RISASGRB)]
      tiv <- suppressWarnings(sum(.subset2(jr, "MTTOTRAS"), na.rm = TRUE))
      maxgrb <- suppressWarnings(max(.subset2(jr, "RISASGRB"), na.rm = TRUE))
      subres <- list(
        "ID" = id[x],
        "jointRiskTotalInsuredValue" = tiv,
        "jointRiskMaximumPropertyClass" = maxgrb,
        "JOINTRISKS" = jr
      )
      names(subres)[1] <- ref_id
      return(subres)
    })
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
#' @param comaubat A character string. Default to paste0(prefix, "COMAUBAT").
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
#' @param umessupe A character string. Default to paste0(prefix, "UMESSUPE").
#' @param mttotras A character string. Default to paste0(prefix, "MTTOTRAS").
#' @param streets A LINESTRING object of navstreets with LANE_RADIUS and geometry elements.
#' @return A data.table with paste0(prefix,"POLYINDX"), paste0(prefix,"POLYMAXRISASGRB"),
#' paste0(prefix,"POLYSUMMTTOTRAS") columns. If the risk has no appropriate geolocation
#' information, index will be NA and other values will just be the individual risk values.
#' @importFrom sf st_cast st_intersects st_union
#' @examples
#' \dontrun{
#' dt <- extraw::get_policies(
#'     inforce = TRUE,
#'     partial = "PRO",
#'     filters = list(MPROD_ID = c(2552251, 1071124, 1071125, 1071122),
#'                    MLIAF_ID = 4,
#'                    MCAAF_ID = 2),
#'     detailid = c(140, 959, 971, 1082, 1083, 9045, 9406, 9408, 14218, 14219, 14220, 14367, 14491, 14650, 14660, 14661)
#'   )
#' append_polygons_idx(dt, prefix = "PROD_", comaubat = "PROD_14367")
#' }
append_polygons_idx <- function(dt,
                                prefix = "",
                                affectat = paste0(prefix, "AFFECTAT"),
                                comaubat = paste0(prefix, "COMAUBAT"),
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
                                umessupe = paste0(prefix, "UMESSUPE"),
                                mttotras = paste0(prefix, "MTTOTRAS"),
                                streets) {
  source <- copy(dt[, c(affectat, comaubat, latitcom, longicom, murstruc, plancher,
                            pregeoco, princfus, resaufeu, risasgrb, rvextbet,
                            rvextbri, superrez, toitstru, umessupe, mttotras, "MINTE_ID", "PRODUIT"), with = FALSE])
  setnames(source,
           c(affectat, comaubat, latitcom, longicom, murstruc, plancher, pregeoco, princfus,
             resaufeu, risasgrb, rvextbet, rvextbri, superrez, toitstru, umessupe, mttotras),
           c("AFFECTAT", "COMAUBAT", "LATITCOM", "LONGICOM", "MURSTRUC", "PLANCHER", "PREGEOCO", "PRINCFUS",
             "RESAUFEU", "RISASGRB", "RVEXTBET", "RVEXTBRI", "SUPERREZ", "TOITSTRU", "UMESSUPE", "MTTOTRAS"))
  set(source, j = "MERGEKEY", value = seq_len(nrow(source)))
  append_typecons(source)
  calculate_radius(source)
  polygons <- create_polygons(source, streets)
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
                  list(POLYMAXRISASGRB = max(get(risasgrb), na.rm = TRUE),
                       POLYSUMMTTOTRAS = sum(get(mttotras), na.rm = TRUE)),
                  by = POLYINDX])
  setDT(infosup, key = "POLYINDX")
  setDT(dt     , key = "POLYINDX")
  pos <- which(!names(infosup) %chin% names(dt))
  dt[, names(infosup)[pos] := infosup[dt[, list(POLYINDX)], pos, with = FALSE]]
  dt[is.na(POLYINDX), c("POLYMAXRISASGRB", "POLYSUMMTTOTRAS") := list(get(risasgrb), get(mttotras))]
  nm <- c("POLYINDX", "POLYMAXRISASGRB", "POLYSUMMTTOTRAS")
  setnames(dt, nm, paste0(prefix, nm), skip_absent = TRUE)
  return(invisible(list("source" = source, "pockets" = pockets)))
}

#' @export
#' @title Get risks CGEN from extraw
#' @description Query to extract inforce commercial policies details for cgen to use in joint risks evaluation.
#' @return A data.table object with IDs and columns.
get_risks_cgen <- function() {
  options(extraw.context = 11)
  dt <- extraw::get_policies(
    inforce = TRUE,
    partial = "PRO",
    filters = list(MPROD_ID = c(2552251, 1071124, 1071125, 1071122),
                   MLIAF_ID = 4,
                   MCAAF_ID = 2),
    detailid = c(140, 959, 971, 1082, 1083, 9045, 9406, 9408, 14218, 14219, 14220, 14367, 14491, 14650, 14660, 14661),
    denormalize = TRUE
  )
  data.table::setnames(dt, gsub("PROD_", "", names(dt)))
  data.table::setnames(dt, c("MINTE_ID", "MPRCH_ID", "PRODUIT", "14367"), c("INTE_NO", "PRCH_ID", "PROD_CODE", "COMAUBAT"), skip_absent = TRUE)
  numcol  <- c("SUPERREZ", "RVEXTBET", "RVEXTBRI", "PRINCFUS", "MTTOTRAS")
  suppressWarnings(dt[, (numcol) := lapply(.SD, as.integer), .SDcols = numcol])
  append_typecons(dt)
  return(dt[,list(INTE_NO, POAS_NO, PRCH_NO, PRCH_ID, PROD_CODE, COMAUBAT,
                  AFFECTAT, LATITCOM, LONGICOM, MTTOTRAS, PREGEOCO, PRINCFUS,
                  RISASGRB, SUPERREZ, UMESSUPE, TYPECONS)])
}

#' @export
#' @title Get risks UGEN from extraw
#' @description Query to extract inforce commercial policies details for ugen to use in joint risks evaluation.
#' @return A data.table object with IDs and columns.
get_risks_ugen <- function() {

  query <- paste0("
    WITH 
    MONTANT_LIMITE_TOTAL AS (
      SELECT PRC_NO_POLICE,
             PRC_TERME,
             PRC_VERSION,
             PRC_EMPLACEMENT,
             SUM(PRC_LIMITE_CATASTROPHE) PRC_LIMITE_CATASTROPHE
      FROM PRD_ACTUARIAT_VESTA..SOUS_PROT_COMM PRC
      WHERE PRC_SECTION_GAR IN ('B', 'E')
      GROUP BY PRC_NO_POLICE,
               PRC_TERME,
               PRC_VERSION,
               PRC_EMPLACEMENT
    ),
    CODE_ACTIVITE AS (
      SELECT SIA_SIT_ID,
             MAX(CDA_CODE_INDUSTRIE) CDA_CODE_INDUSTRIE
      FROM PRD_ACTUARIAT_VESTA..SOUS_SITUATION_ACTIVITE SIA
      LEFT JOIN PRD_ACTUARIAT_VESTA..SOUS_CODE_ACTIVITE CDA
             ON CDA.CDA_ITEM = SIA.SIA_ACTIVITE_ITEM AND
                CDA.CDA_SEQUENCE = SIA.SIA_ACTIVITE_SEQUENCE AND
                CDA.CDA_VERSION = SIA.SIA_ACTIVITE_VERSION
      WHERE SIA_IND_PRINCIPALE = 'O'
      GROUP BY SIA_SIT_ID
    ),
    CLASSE_BIEN AS (
      SELECT SUBQ.SIT_ID CLSB_SIT_ID,
             MAX(SUBQ.CDA_PRO_CLASSE) CDA_PRO_CLASSE
      FROM (
        SELECT SIT.SIT_ID,
	             CDA.CDA_PRO_CLASSE,
               ROW_NUMBER() OVER(
                  PARTITION BY SIA.SIA_SIT_ID, SIA.SIA_ACTIVITE_ITEM, SIA.SIA_ACTIVITE_SEQUENCE
                  ORDER BY SIA.SIA_ACTIVITE_VERSION DESC) as RK
	      FROM PRD_ACTUARIAT_VESTA..SOUS_SITUATION SIT
	      JOIN PRD_ACTUARIAT_VESTA..SOUS_SITUATION_ACTIVITE SIA
	        ON SIA.SIA_SIT_ID = SIT.SIT_ID
	      JOIN PRD_ACTUARIAT_VESTA..SOUS_CODE_ACTIVITE CDA
	        ON CDA.CDA_ITEM = SIA.SIA_ACTIVITE_ITEM
	       AND CDA.CDA_SEQUENCE = SIA.SIA_ACTIVITE_SEQUENCE
	       AND CDA.CDA_VERSION = SIA.SIA_ACTIVITE_VERSION
        WHERE SIA.SIA_PC_SUPERFICIE > 0
      ) SUBQ
      WHERE SUBQ.RK = 1
      GROUP BY SUBQ.SIT_ID
    )
    
    SELECT SIT_ID,
           POL_CODE_PRODUIT PROD_CODE,
           SIT_SUPERFICIE SUPERREZ,
           SIT_SUPERFICIE_UNITE UMESSUPE,
           CAST(SIT_CODE_CONSTRUCTION AS INT) TYPECONS,
           ADR_LATD_TERTR_DEBRD LATITCOM,
           ADR_LONGT_TERTR_DEBRD LONGICOM,
           UPPER(ADR_CODE_PRECISION_GEOCODE) PREGEOCO,
           CASE SIR_TER_FUS
             WHEN '000' THEN NULL
             ELSE CAST(SIR_TER_FUS AS INT)
           END PRINCFUS,
           CDA_CODE_INDUSTRIE AFFECTAT,
           PRC_LIMITE_CATASTROPHE MTTOTRAS,
           CLSB.CDA_PRO_CLASSE RISASGRB
    
    FROM PRD_ACTUARIAT_VESTA..SOUS_POLICE POL
    
    JOIN PRD_ACTUARIAT_VESTA..SOUS_SITUATION SIT
      ON SIT.SIT_NO_POLICE = POL.POL_NUMERO AND 
         SIT.SIT_TERME = POL.POL_TERME AND
         SIT.SIT_VERSION = POL.POL_VERSION
    
    LEFT JOIN CODE_ACTIVITE CDA
           ON CDA.SIA_SIT_ID = SIT.SIT_ID
    
    LEFT JOIN PRD_ACTUARIAT_VESTA..SOUT_ADRESSE ADR
           ON ADR.ADR_NO_REFERENCE = SIT.SIT_NO_POLICE AND
              ADR.ADR_TERME = SIT.SIT_TERME AND
              ADR.ADR_VERSION = SIT.SIT_VERSION AND
              ADR.ADR_NO_IDENTITE = SIT.SIT_NUMERO AND
              ADR.ADR_NO_SEQUENCE = '0' AND
              ADR.ADR_PROVENANCE = 'SITUA'
    
    LEFT JOIN PRD_ACTUARIAT_VESTA..SOUS_SITUATION_TARIF SIR
           ON SIR.SIR_NO_POLICE = SIT.SIT_NO_POLICE AND
              SIR.SIR_TERME = SIT.SIT_TERME AND 
              SIR.SIR_VERSION = SIT.SIT_VERSION AND
              SIR.SIR_NO_SITUATION = SIT.SIT_NUMERO
    
    LEFT JOIN MONTANT_LIMITE_TOTAL PRC
           ON PRC.PRC_NO_POLICE = SIT.SIT_NO_POLICE AND
              PRC.PRC_TERME = SIT.SIT_TERME AND 
              PRC.PRC_VERSION = SIT.SIT_VERSION AND
              PRC.PRC_EMPLACEMENT = SIT.SIT_NUMERO
    
    LEFT JOIN CLASSE_BIEN CLSB
           ON CLSB.CLSB_SIT_ID = SIT.SIT_ID
    
    WHERE POL_LIGAFF = 'BC' AND
          POL_STATUT = 'V' AND 
          POL_TRANSACTION != 'A' AND 
          (SIT_TRANSACTION IS NULL OR SIT_TRANSACTION != 'A') AND
          POL_DATE_PRISE_EFFET <= '", Sys.Date(),"' AND 
          POL_DATE_FIN_EFFET > '", Sys.Date(),"'
  ")
  
  con <- extraw::init_con(entity = "UGEN")
  dt <- extraw::dispatch_query(con, query)

  numcol  <- c("SUPERREZ", "PRINCFUS", "MTTOTRAS", "TYPECONS")
  suppressWarnings(dt[, (numcol) := lapply(.SD, as.integer), .SDcols = numcol])
  return(dt)
}

#' @export
#' @title Get risks CGEN from a file
#' @param file file to load
#' @description Query to extract inforce commercial policies details for cgen to use in joint risks evaluation.
#' @return A data.table object with IDs and columns.
load_risk_cgen <- function(file) {

  dt <- data.table::fread(file, header = FALSE,
                          col.names = c("POAS_NO", "INTE_NO", "PRCH_NO", "PROD_CODE", "PRCH_ID", "VEPC_ID", "QUESTION", "REPONSE"))
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

  numcol  <- c("SUPERREZ", "PRINCFUS", "MTTOTRAS", "TYPECONS")
  suppressWarnings(dt[, (numcol) := lapply(.SD, as.integer), .SDcols = numcol])

  return(dt[order((as.numeric(LATITCOM)*1000 + as.numeric(LONGICOM)))])

}

#' @export
#' @title Get risks UGEN from a file
#' @param file file to load
#' @description Query to extract inforce commercial policies details for ugen to use in joint risks evaluation.
#' @return A data.table object with IDs and columns.
load_risk_ugen <- function(file) {
  
  dt <- data.table::fread(file)
  data.table::setDT(dt, key = "SIT_ID")
  
  # Check that there is only one row per SIT_ID
  if (length(unique(dt$SIT_ID)) < nrow(dt)) {
    dups <- which(duplicated(dt$SIT_ID))
    warning(paste0("Duplicated SIT_ID. Only the first row of duplicated SIT_ID will be kept."))
    dt <- dt[-dups]
  }
  
  numcol  <- c("SUPERREZ", "PRINCFUS", "MTTOTRAS", "TYPECONS")
  suppressWarnings(dt[, (numcol) := lapply(.SD, as.integer), .SDcols = numcol])
  
  return(dt[order((as.numeric(LATITCOM)*1000 + as.numeric(LONGICOM)))])
  
}

#' Warmup vectorizer functions for optimized runs.
#' @importFrom jsonlite fromJSON
#' @export
warmup <- function(polygons, streets) {
  dt <- jsonlite::fromJSON('[{"PRCH_ID":82804587,"COMAUBAT":"NA","AFFECTAT":"C8085","LATITCOM":"46.77418",
                            "LONGICOM":"-71.30196","PRINCFUS":"NA","SUPERREZ":"NA","UMESSUPE":"PI",
                            "TYPECONS":2}]')
  get_joint_risks(dt, polygons, streets)
  return(invisible())
}
