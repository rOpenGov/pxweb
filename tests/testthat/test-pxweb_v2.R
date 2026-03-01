
test_that(desc = "pxweb API v2", {
  # Keep same style as existing live API tests.
  skip_on_cran()
  skip_if_offline()

  # API v1
  pxapi_1 <- pxweb(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24")
  expect_equal(pxapi_1$version, "v1")

  pxapi_2 <- pxweb(url = "https://api.scb.se/OV0104/v1/doris/sv")
  expect_equal(pxapi_2$version, "v1")

  # Example table id from SCB v2 documentation.
  metadata_url <- "https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/metadata"
  pxapi_v2 <- pxweb(url = metadata_url)
  expect_equal(pxapi_v2$version, "v2")

})

