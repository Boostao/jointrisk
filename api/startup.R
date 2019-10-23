library(plumber)

pr <- plumb("./api/plumber.R")
pr$registerHooks(list(
  postserialize = function(req, res) {
    if (req$REQUEST_METHOD == "POST") {
      cat("[", req$REQUEST_METHOD, req$PATH_INFO, "] - RESPONSE - ", res$status, "\n", sep = "")
    }
  }
))

pr$run(host = "0.0.0.0", port = 8004)
