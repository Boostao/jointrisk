library(plumber)
serializer_jsongzipped <- function(...) {
  function(val, req, res, errorHandler) {
    tryCatch({
      json <- jsonlite::toJSON(val, na = "string", ...)
      res$setHeader("Content-Type", "application/json")
      res$setHeader("Content-Encoding", "gzip")
      res$body <- memCompress(json, "gzip")

      return(res$toResponse())
    }, error = function(e){
      errorHandler(req, res, e)
    })
  }
}
addSerializer("jsongzipped", serializer_jsongzipped)
pr <- plumb("/etc/plumber.R")
pr$registerHooks(list(
  postroute = function(req) {
    if (req$REQUEST_METHOD == "POST") {
      browser()
      cat("[", req$REQUEST_METHOD, req$PATH_INFO, "] - REQUEST - ", req$postBody, "\n", sep = "")
    }
  },
  postserialize = function(req, res) {
    if (req$REQUEST_METHOD == "POST") {
      browser()
      cat("[", req$REQUEST_METHOD, req$PATH_INFO, "] - RESPONSE - ", res$status, " - ", res$body, "\n", sep = "")
    }
  }
))
pr$run(host = "0.0.0.0", port = 8004)
