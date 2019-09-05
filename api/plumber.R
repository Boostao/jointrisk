library(jointrisk)
warmup()

#* Health check
#* @get /
function() {
    return("OK")
}

#* Update polygons
#* @post /update_polygons
#* @serializer jsongzipped
function() {
    return(get_polygons())
}

#* Get polygons id
#* @param dt
#* @post /polygons
#* @serializer json
function(dt) {
    return(get_polygons_id(dt))
}
