library(jointrisk)
options(jointrisk.riskfile = "filelocation.csv")

update_polygons(load_risk_cgen())
warmup()

#* Health check
#* @get /
function() {
    return("OK")
}

#* Update polygons
#* @get /update_polygons
function() {
    return(update_polygons(load_risk_cgen()))
}

#* Get polygons id
#* @param dt
#* @post /polygons
#* @serializer json
function(dt) {
    return(
      list("version" = jointrisk:::pkgV,
           "results" = get_joint_risks(dt))
    )
}
