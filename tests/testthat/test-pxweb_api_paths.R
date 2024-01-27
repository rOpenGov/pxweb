# Test suits for the examples in the documentation

context("pxweb_api_paths")

test_that(desc = "Access api paths", {
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  skip_if_offline()

  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0301/"
  expect_silent(scb <- pxweb(url))

  expect_equal(pxweb_api_name(scb), "api.scb.se")

  expect_equal(pxweb_api_subpath(scb, init_slash = FALSE), "OV0104/v1/doris/sv")
  expect_equal(pxweb_api_subpath(scb, init_slash = TRUE), "/OV0104/v1/doris/sv")
  expect_equal(pxweb_api_subpath(scb, as_vector = TRUE), c("OV0104", "v1", "doris", "sv"))

  expect_equal(pxweb_api_path(scb, init_slash = FALSE), "OV0104/v1/doris/sv/ssd/START/AM/AM0301")
  expect_equal(pxweb_api_path(scb, init_slash = TRUE), "/OV0104/v1/doris/sv/ssd/START/AM/AM0301")
  expect_equal(pxweb_api_path(scb, as_vector = TRUE), c("OV0104", "v1", "doris", "sv", "ssd", "START", "AM", "AM0301"))

  expect_equal(pxweb_api_dbpath(scb, init_slash = FALSE), "ssd/START/AM/AM0301")
  expect_equal(pxweb_api_dbpath(scb, init_slash = TRUE), "/ssd/START/AM/AM0301")
  expect_equal(pxweb_api_dbpath(scb, as_vector = TRUE), c("ssd", "START", "AM", "AM0301"))
})
