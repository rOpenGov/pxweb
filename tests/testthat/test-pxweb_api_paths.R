# Test suits for the examples in the documentation

context("pxweb_api_paths")

test_that(desc = "Access api paths", {
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0301/"
  scb <- list(
    url = parse_url_or_fail(url),
    paths = list(
      api_subpath = list(
        path = "OV0104/v1/doris/sv",
        vector = c("OV0104", "v1", "doris", "sv")
      )
    )
  )
  class(scb) <- c("pxweb", "list")

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
