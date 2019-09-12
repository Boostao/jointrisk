library(jointrisk)
warmup()

#* Health check
#* @get /
function() {
    return("OK")
}

#* Update polygons
#* @get /update_polygons
#* @param file
function(file) {
    return(update_polygons(load_risk_cgen(file)))
}

#* Get polygons id
#* @param dt
#* @post /polygons
#* @serializer json
function(dt) {
    return(get_polygons_id(dt))
}
