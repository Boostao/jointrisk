options(jointrisk.riskfile = "souss004.csv")
streets <- readRDS("./data-raw/streets.RDS")
library(jointrisk)

polygons <- update_polygons(load_risk_cgen(), streets = streets)
warmup(polygons, streets)

#* Health check
#* @get /
function() {
    return("OK")
}

#* Update polygons
#* @get /update_polygons
function() {
    polygons <- update_polygons(load_risk_cgen(), streets = streets) 
    return("Polygons updated")
}

#* Get polygons id
#* @param dt:df*
#* @post /jointrisk/opus
#* @serializer json
function(dt) {
    return(
      list("version" = jointrisk:::pkgV,
           "results" = get_joint_risks(dt, polygons, streets))
    )
}