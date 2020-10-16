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
  invisible(gcs_get_object(jrfn, saveToDisk = jrfn, overwrite = TRUE))
  polys <- update_polygons(load_risk_cgen(), .jointrisk$assets$polygons, .jointrisk$assets$streets)
  rm(assets, envir = .jointrisk)
  .jointrisk$assets <- new.env(parent = .jointrisk)
  .jointrisk$assets$streets <- readRDS(stfn)
  .jointrisk$assets$polygons <- polys$poly
  return(polys$msg)
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
