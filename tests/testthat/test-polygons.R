context("Polygons creation")
test_that("Compo are available", {
  expect_equal(names(compo), c("COTYCONS", "COTYCON2"))
})

test_that("Compo are same as inforce", {
  skip_on_ci()
  library(extraw)
  inforce_compo <- get_compo(c("COTYCONS", "COTYCON2"), inforce = TRUE)
  expect_equal(compo, inforce_compo)
})

