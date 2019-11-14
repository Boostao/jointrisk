options(jointrisk.riskfile = "souss004.csv")
library(jointrisk)

polygons <- update_polygons(load_risk_cgen())
warmup(polygons)

#* Health check
#* @get /
function() {
    return("OK")
}

#* Update polygons
#* @get /update_polygons
function() {
    polygons <- update_polygons(load_risk_cgen()) 
    return("Polygons updated")
}

#* Get polygons id
#* @param dt
#* @post /jointrisk/opus
#* @serializer json
function(dt) {
    return(
      list("version" = jointrisk:::pkgV,
           "results" = get_joint_risks(dt, polygons))
    )
}