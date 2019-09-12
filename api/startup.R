library(plumber)

pr <- plumb("../api/plumber.R")
pr$registerHooks(list(
  postroute = function(req) {
    if (req$REQUEST_METHOD == "POST") {
      cat("[", req$REQUEST_METHOD, req$PATH_INFO, "] - REQUEST - ", req$postBody, "\n", sep = "")
    }
  },
  postserialize = function(req, res) {
    if (req$REQUEST_METHOD == "POST") {
      cat("[", req$REQUEST_METHOD, req$PATH_INFO, "] - RESPONSE - ", res$status, " - ", res$body, "\n", sep = "")
    }
  }
))

pr$run(host = "0.0.0.0", port = 8004)
