context("Polygons creation")
test_that("Compo are available and same as inforce", {
  expect_equal(names(compo), c("COTYCONS", "COTYCON2"))
  inforce_compo <- get_compo(c("COTYCONS", "COTYCON2"), inforce = TRUE)
  expect_equal(compo, inforce_compo)
})
